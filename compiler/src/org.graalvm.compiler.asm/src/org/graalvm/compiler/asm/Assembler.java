/*
 * Copyright (c) 2009, 2019, Oracle and/or its affiliates. All rights reserved.
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS FILE HEADER.
 *
 * This code is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License version 2 only, as
 * published by the Free Software Foundation.  Oracle designates this
 * particular file as subject to the "Classpath" exception as provided
 * by Oracle in the LICENSE file that accompanied this code.
 *
 * This code is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 * version 2 for more details (a copy is included in the LICENSE file that
 * accompanied this code).
 *
 * You should have received a copy of the GNU General Public License version
 * 2 along with this work; if not, write to the Free Software Foundation,
 * Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA.
 *
 * Please contact Oracle, 500 Oracle Parkway, Redwood Shores, CA 94065 USA
 * or visit www.oracle.com if you need additional information or have any
 * questions.
 */
package org.graalvm.compiler.asm;

import static java.lang.Math.max;
import static java.lang.Math.min;

import java.nio.ByteOrder;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Consumer;

import org.graalvm.compiler.core.common.NumUtil;
import org.graalvm.compiler.debug.GraalError;

import jdk.vm.ci.code.Register;
import jdk.vm.ci.code.StackSlot;
import jdk.vm.ci.code.TargetDescription;

/**
 * The platform-independent base class for the assembler.
 */
public abstract class Assembler {

    public static long bitMask(int hiBit, int loBit) {
        return -1l << (63 - hiBit + loBit) >>> (63 - hiBit);
    }

    public abstract static class CodeAnnotation {
        /**
         * The position (bytes from the beginning of the method) of the annotated instruction.
         */
        public final int instructionPosition;

        protected CodeAnnotation(int instructionStartPosition) {
            this.instructionPosition = instructionStartPosition;
        }
    }

    public interface Patchable {
        Patcher patcher();
    }

    public final TargetDescription target;
    private List<LabelHint> jumpDisplacementHints;

    public static class Patcher {
        public final PatchGroup[] groups;

        public Patcher(PatchDefinition[] patchDefinitions) {
            this(new PatchGroup(patchDefinitions));
        }

        public Patcher(PatchGroup... groups) {
            super();
            this.groups = groups;
        }

        public static Patcher from(BitSpec spec) {
            return new Patcher(PatchDefinition.fromBitSpec(spec));
        }

        public void patch(byte[] array, int offset, long value) {
            long valueTmp = value;
            for (PatchGroup pg : groups) {
                long toPatch = Long.min(pg.maxValue, Long.max(pg.minValue, valueTmp));
                if (toPatch > pg.maxValue) {
                    toPatch = pg.maxValue;
                } else if (toPatch < pg.minValue) {
                    toPatch = pg.minValue;
                }
                for (PatchDefinition d : pg.definitions) {
                    int idx = offset + d.byteOffset;
                    int word = array[idx] & ~d.outputMask;
                    long outputBitsBase = toPatch >>> d.inputShift;
                    long outputBits = (outputBitsBase << d.outputShift) & d.outputMask;
                    array[idx] = (byte) (word | outputBits);
                }
                valueTmp -= toPatch;
            }
            assert valueTmp == 0 : "Could not completly patch value 0x" + Long.toHexString(value) + " remaining part: 0x" + Long.toHexString(valueTmp);
        }
    }

    public static class PatchGroup {
        public final int bitCount;
        public final long minValue;
        public final long maxValue;
        public final PatchDefinition[] definitions;

        public PatchGroup(PatchDefinition... definitions) {
            super();
            int bitCount = 0;
            for (PatchDefinition definition : definitions) {
                bitCount += definition.bitCount;
            }
            this.bitCount = bitCount;
            boolean signed = definitions[definitions.length - 1].signExtend;
            if (signed) {
                this.maxValue = bitMask(bitCount - 2, 0);
                this.minValue = -1l << (bitCount - 1);
            } else {
                this.maxValue = bitMask(bitCount - 1, 0);
                this.minValue = 0;
            }
            this.definitions = definitions;
        }

    }

    public static class PatchDefinition {
        public final int outputMask;
        public final int bitCount;
        public final int outputShift;
        public final int inputShift;
        public final int byteOffset;
        public final boolean signExtend;

        public PatchDefinition(int inputShift, int outputMask, int byteOffset, boolean signExtend) {
            super();
            this.outputMask = outputMask;
            this.bitCount = Integer.bitCount(outputMask);
            this.outputShift = Integer.numberOfTrailingZeros(outputMask);
            this.inputShift = inputShift;
            this.byteOffset = byteOffset;
            this.signExtend = signExtend;
        }

        public static PatchDefinition[] fromBitSpec(BitSpec spec) {
            ArrayList<PatchDefinition> result = new ArrayList<>();
            doBuildFromBitSpec(spec, result, 0, 0);
            return result.toArray(new PatchDefinition[result.size()]);
        }

        private static void doBuildFromBitSpec(BitSpec spec, ArrayList<PatchDefinition> result, int offset, int inBitOffset) {
            if (spec instanceof ContinousBitSpec) {
                fromContinous(spec, result, offset, inBitOffset);
            } else if (spec instanceof CompositeBitSpec) {
                CompositeBitSpec comp = (CompositeBitSpec) spec;
                doBuildFromBitSpec(comp.right, result, offset, inBitOffset);
                doBuildFromBitSpec(comp.left, result, offset, inBitOffset + comp.right.getWidth());
            } else if (spec instanceof ConcatBitSpec) {
                ConcatBitSpec conc = (ConcatBitSpec) spec;
                int iOffset = offset;
                int bitOffset = inBitOffset;
                for (BitSpec iSpec : conc.bitSpecs) {
                    doBuildFromBitSpec(iSpec, result, iOffset, bitOffset);
                    iOffset += iSpec.wordLength();
                    bitOffset += iSpec.getWidth();
                }
            } else {
                throw GraalError.shouldNotReachHere("Unknown BitSpec " + spec + " of type " + spec.getClass());
            }
        }

        private static void fromContinous(BitSpec spec, ArrayList<PatchDefinition> result, int offset, int inBitOffset) {
            ContinousBitSpec cont = (ContinousBitSpec) spec;
            int bitOffset = inBitOffset;
            int bitsConsumed = 0;
            for (int i = 0; i < spec.wordLength(); i++) {
                int byteLow = i * 8;
                int byteHi = (i + 1) * 8;
                int byteLowBit = max(byteLow, min(byteHi, cont.lowBit));
                int byteHiBit = max(byteLow, min(byteHi, cont.hiBit + 1));
                int inputShift = byteHiBit - byteLowBit;
                int inputMask = (1 << inputShift) - 1;
                int outputMask = inputMask << (byteLowBit % 8);
                if (inputShift > 0) {
                    bitsConsumed += inputShift;
                    boolean signExtend = spec.isSignExtend() && bitsConsumed == spec.getWidth();
                    result.add(new PatchDefinition(bitOffset, outputMask, offset + i, signExtend));
                }
                bitOffset += inputShift;
            }
        }

        @Override
        public String toString() {
            return "Mask: 0b" + Integer.toBinaryString(outputMask);
        }
    }

    public abstract static class BitSpec {

        protected final boolean signExtend;

        public BitSpec(boolean signExtend) {
            super();
            this.signExtend = signExtend;
        }

        public final boolean isSignExtend() {
            return signExtend;
        }

        public abstract int setBits(int word, int value);

        public abstract int getBits(int word);

        public abstract int getMask();

        public abstract int getWidth();

        public abstract int wordLength();

        public abstract ByteOrder byteOrder();

        public abstract boolean valueFits(int value);
    }

    public static final class ContinousBitSpec extends BitSpec {
        private final int hiBit;
        private final int lowBit;
        private final int width;
        private final int mask;
        private final int wordLength;
        private final String name;

        public ContinousBitSpec(int hiBit, int lowBit, String name) {
            this(hiBit, lowBit, 4, name);
        }

        public ContinousBitSpec(int hiBit, int lowBit, int wordLength, String name) {
            this(hiBit, lowBit, false, wordLength, name);
        }

        public ContinousBitSpec(int hiBit, int lowBit, boolean signExt, String name) {
            this(hiBit, lowBit, signExt, 4, name);
        }

        public ContinousBitSpec(int hiBit, int lowBit, boolean signExt, int wordLength, String name) {
            super(signExt);
            assert hiBit < wordLength * 8;
            this.hiBit = hiBit;
            this.lowBit = lowBit;
            this.width = hiBit - lowBit + 1;
            mask = ((1 << width) - 1) << lowBit;
            this.name = name;
            this.wordLength = wordLength;
        }

        @Override
        public int setBits(int word, int value) {
            assert valueFits(value) : String.format("Value 0x%x for field %s does not fit.", value, this);
            return (word & ~mask) | ((value << lowBit) & mask);
        }

        @Override
        public int getBits(int word) {
            if (signExtend) {
                return ((word & mask) << (31 - hiBit)) >> (32 - width);
            } else {
                return (word & mask) >>> lowBit;
            }
        }

        @Override
        public int getWidth() {
            return width;
        }

        @Override
        public int getMask() {
            return mask;
        }

        @Override
        public int wordLength() {
            return wordLength;
        }

        @Override
        public ByteOrder byteOrder() {
            return ByteOrder.LITTLE_ENDIAN;
        }

        @Override
        public String toString() {
            return String.format("%s [%d:%d]", name, hiBit, lowBit);
        }

        @Override
        public boolean valueFits(int value) {
            if (signExtend) {
                return NumUtil.isSignedNbit(getWidth(), value);
            } else {
                return NumUtil.isUnsignedNbit(getWidth(), value);
            }
        }

    }

    public static final class ConcatBitSpec extends BitSpec {
        BitSpec[] bitSpecs;
        final int wordLength;
        final int bitWidth;

        public ConcatBitSpec(BitSpec... bitSpecs) {
            super(bitSpecs[bitSpecs.length - 1].isSignExtend());
            this.bitSpecs = bitSpecs;
            int l = 0;
            int bits = 0;
            for (BitSpec spec : bitSpecs) {
                l += spec.wordLength();
                bits += spec.getWidth();
            }
            this.wordLength = l;
            this.bitWidth = bits;
        }

        @Override
        public ByteOrder byteOrder() {
            return ByteOrder.LITTLE_ENDIAN;
        }

        @Override
        public int getBits(int word) {
            throw GraalError.shouldNotReachHere();
        }

        @Override
        public int setBits(int word, int value) {
            throw GraalError.shouldNotReachHere();
        }

        @Override
        public int getMask() {
            throw GraalError.shouldNotReachHere();
        }

        @Override
        public int getWidth() {
            return bitWidth;
        }

        @Override
        public int wordLength() {
            return wordLength;
        }

        @Override
        public boolean valueFits(int value) {
            throw GraalError.shouldNotReachHere();
        }
    }

    public static final class CompositeBitSpec extends BitSpec {
        private final BitSpec left;
        private final int leftWidth;
        private final BitSpec right;
        private final int rightWidth;
        private final int width;

        public CompositeBitSpec(BitSpec left, BitSpec right) {
            super(left.isSignExtend());
            assert !right.isSignExtend() : String.format("Right field %s must not be sign extended", right);
            this.left = left;
            this.leftWidth = left.getWidth();
            this.right = right;
            this.rightWidth = right.getWidth();
            this.width = leftWidth + rightWidth;
        }

        @Override
        public int getBits(int word) {
            int l = left.getBits(word);
            int r = right.getBits(word);
            return (l << rightWidth) | r;
        }

        @Override
        public int setBits(int word, int value) {
            int l = leftBits(value);
            int r = rightBits(value);
            return left.setBits(right.setBits(word, r), l);
        }

        private int leftBits(int value) {
            return getBits(value, width - 1, rightWidth, signExtend);
        }

        private int rightBits(int value) {
            return getBits(value, rightWidth - 1, 0, false);
        }

        @Override
        public int getWidth() {
            return width;
        }

        @Override
        public int getMask() {
            int l = left.getMask();
            int r = right.getMask();
            return l | r;
        }

        @Override
        public int wordLength() {
            return 4;
        }

        @Override
        public ByteOrder byteOrder() {
            return ByteOrder.LITTLE_ENDIAN;
        }

        @Override
        public String toString() {
            return String.format("CompositeBitSpec[%s, %s]", left, right);
        }

        @Override
        public boolean valueFits(int value) {
            int l = leftBits(value);
            int r = rightBits(value);
            return left.valueFits(l) && right.valueFits(r);
        }

        private static int getBits(int inst, int hiBit, int lowBit, boolean signExtended) {
            int shifted = inst >> lowBit;
            if (signExtended) {
                return shifted;
            } else {
                return shifted & ((1 << (hiBit - lowBit + 1)) - 1);
            }
        }
    }

    /**
     * Labels with instructions to be patched when it is {@linkplain Label#bind bound}.
     */
    Label labelsWithPatches;

    /**
     * Backing code buffer.
     */
    private final Buffer codeBuffer;

    protected Consumer<CodeAnnotation> codePatchingAnnotationConsumer;

    public Assembler(TargetDescription target) {
        this.target = target;
        this.codeBuffer = new Buffer(target.arch.getByteOrder());
    }

    public void setCodePatchingAnnotationConsumer(Consumer<CodeAnnotation> codeAnnotationConsumer) {
        assert this.codePatchingAnnotationConsumer == null : "overwriting existing value";
        this.codePatchingAnnotationConsumer = codeAnnotationConsumer;
    }

    /**
     * Returns the current position of the underlying code buffer.
     *
     * @return current position in code buffer
     */
    public int position() {
        return codeBuffer.position();
    }

    public final void emitByte(int x) {
        codeBuffer.emitByte(x);
    }

    public final void emitShort(int x) {
        codeBuffer.emitShort(x);
    }

    public final void emitInt(int x) {
        codeBuffer.emitInt(x);
    }

    public final void emitLong(long x) {
        codeBuffer.emitLong(x);
    }

    public final void emitByte(int b, int pos) {
        codeBuffer.emitByte(b, pos);
    }

    public final void emitShort(int b, int pos) {
        codeBuffer.emitShort(b, pos);
    }

    public final void emitInt(int b, int pos) {
        codeBuffer.emitInt(b, pos);
    }

    public final void emitLong(long b, int pos) {
        codeBuffer.emitLong(b, pos);
    }

    public final int getByte(int pos) {
        return codeBuffer.getByte(pos);
    }

    public final int getShort(int pos) {
        return codeBuffer.getShort(pos);
    }

    public final int getInt(int pos) {
        return codeBuffer.getInt(pos);
    }

    private static final String NEWLINE = System.lineSeparator();

    /**
     * Some GPU architectures have a text based encoding.
     */
    public final void emitString(String x) {
        emitString0("\t");  // XXX REMOVE ME pretty-printing
        emitString0(x);
        emitString0(NEWLINE);
    }

    // XXX for pretty-printing
    public final void emitString0(String x) {
        codeBuffer.emitBytes(x.getBytes(), 0, x.length());
    }

    public void emitString(String s, int pos) {
        codeBuffer.emitBytes(s.getBytes(), pos);
    }

    /**
     * Closes this assembler. No extra data can be written to this assembler after this call.
     *
     * @param trimmedCopy if {@code true}, then a copy of the underlying byte array up to (but not
     *            including) {@code position()} is returned
     * @return the data in this buffer or a trimmed copy if {@code trimmedCopy} is {@code true}
     */
    public byte[] close(boolean trimmedCopy) {
        checkAndClearLabelsWithPatches();
        return codeBuffer.close(trimmedCopy);
    }

    public byte[] copy(int start, int end) {
        return codeBuffer.copyData(start, end);
    }

    private void checkAndClearLabelsWithPatches() throws InternalError {
        Label label = labelsWithPatches;
        while (label != null) {
            if (label.patchPositions != null) {
                throw new GraalError("Label used by instructions at following offsets has not been bound: %s", label.patchPositions);
            }
            Label next = label.nextWithPatches;
            label.nextWithPatches = null;
            label = next;
        }
        labelsWithPatches = null;
    }

    public void bind(Label l) {
        assert !l.isBound() : "can bind label only once";
        l.bind(position(), this);
    }

    public abstract void align(int modulus);

    public abstract void jmp(Label l);

    protected abstract void patchJumpTarget(int branch, int jumpTarget);

    private Map<Label, String> nameMap;

    /**
     * Creates a name for a label.
     *
     * @param l the label for which a name is being created
     * @param id a label identifier that is unique with the scope of this assembler
     * @return a label name in the form of "L123"
     */
    protected String createLabelName(Label l, int id) {
        return "L" + id;
    }

    /**
     * Gets a name for a label, creating it if it does not yet exist. By default, the returned name
     * is only unique with the scope of this assembler.
     */
    public String nameOf(Label l) {
        if (nameMap == null) {
            nameMap = new HashMap<>();
        }
        String name = nameMap.get(l);
        if (name == null) {
            name = createLabelName(l, nameMap.size());
            nameMap.put(l, name);
        }
        return name;
    }

    /**
     * This is used by the CompilationResultBuilder to convert a {@link StackSlot} to an
     * {@link AbstractAddress}.
     */
    public abstract AbstractAddress makeAddress(Register base, int displacement);

    /**
     * Returns a target specific placeholder address that can be used for code patching.
     *
     * @param instructionStartPosition The start of the instruction, i.e., the value that is used as
     *            the key for looking up placeholder patching information.
     */
    public abstract AbstractAddress getPlaceholder(int instructionStartPosition);

    /**
     * Emits a NOP instruction to advance the current PC.
     */
    public abstract void ensureUniquePC();

    public void reset() {
        codeBuffer.reset();
        captureLabelPositions();
    }

    private void captureLabelPositions() {
        if (jumpDisplacementHints == null) {
            return;
        }
        for (LabelHint request : this.jumpDisplacementHints) {
            request.capture();
        }
    }

    public LabelHint requestLabelHint(Label label) {
        if (jumpDisplacementHints == null) {
            jumpDisplacementHints = new ArrayList<>();
        }
        LabelHint hint = new LabelHint(label, position());
        this.jumpDisplacementHints.add(hint);
        return hint;
    }

    public InstructionCounter getInstructionCounter() {
        throw new UnsupportedOperationException("Instruction counter is not implemented for " + this);
    }

    public static class LabelHint {
        private Label label;
        private int forPosition;
        private int capturedTarget = -1;

        protected LabelHint(Label label, int lastPosition) {
            super();
            this.label = label;
            this.forPosition = lastPosition;
        }

        protected void capture() {
            this.capturedTarget = label.position();
        }

        public int getTarget() {
            assert isValid();
            return capturedTarget;
        }

        public int getPosition() {
            assert isValid();
            return forPosition;
        }

        public boolean isValid() {
            return capturedTarget >= 0;
        }
    }

    /**
     * Instruction counter class which gives the user of the assembler to count different kinds of
     * instructions in the generated assembler code.
     */
    public interface InstructionCounter {
        String[] getSupportedInstructionTypes();

        int[] countInstructions(String[] instructionTypes, int beginPc, int endPc);
    }
}
