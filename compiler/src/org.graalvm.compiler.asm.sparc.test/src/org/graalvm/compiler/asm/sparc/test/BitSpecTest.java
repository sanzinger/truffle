/*
 * Copyright (c) 2016, 2018, Oracle and/or its affiliates. All rights reserved.
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
package org.graalvm.compiler.asm.sparc.test;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.nio.ByteBuffer;
import java.nio.ByteOrder;

import org.graalvm.compiler.asm.Assembler;
import org.graalvm.compiler.asm.Assembler.BitSpec;
import org.graalvm.compiler.asm.Assembler.CompositeBitSpec;
import org.graalvm.compiler.asm.Assembler.ConcatBitSpec;
import org.graalvm.compiler.asm.Assembler.ContinousBitSpec;
import org.graalvm.compiler.asm.Assembler.PatchDefinition;
import org.graalvm.compiler.asm.Assembler.PatchGroup;
import org.graalvm.compiler.asm.Assembler.Patcher;
import org.junit.Test;

public class BitSpecTest {

    private static final BitSpec d4hi = new ContinousBitSpec(23, 20, true, "d4hi");
    private static final BitSpec d4lo = new ContinousBitSpec(7, 4, false, "d4lo");
    private static final BitSpec d8 = new CompositeBitSpec(d4hi, d4lo);

    @Test
    public void testSpecContinous64() {
        ContinousBitSpec spec = new ContinousBitSpec(63, 0, 8, "test");
        PatchDefinition[] def = PatchDefinition.fromBitSpec(spec);
        assertEquals(8, def.length);
        int b = 0;
        long inputMask = 0xFF;
        for (PatchDefinition d : def) {
            assertEquals(0xFF, d.outputMask);
            assertEquals(8, d.bitCount);
            assertEquals(0, d.outputShift);
            assertEquals(b++, d.byteOffset);
            inputMask <<= 8;
        }
    }

    @Test
    public void testSpecGen1() {
        ContinousBitSpec spec = new ContinousBitSpec(2, 0, "test");
        PatchDefinition[] def = PatchDefinition.fromBitSpec(spec);
        assertEquals(0b111, def[0].outputMask);
        assertEquals(3, def[0].bitCount);
        assertEquals(0, def[0].outputShift);
        assertEquals(0, def[0].byteOffset);
        assertEquals(1, def.length);
    }

    @Test
    public void testSpecGen2() {
        ContinousBitSpec spec = new ContinousBitSpec(3, 1, "test");
        PatchDefinition[] def = PatchDefinition.fromBitSpec(spec);
        assertEquals(0b1110, def[0].outputMask);
        assertEquals(3, def[0].bitCount);
        assertEquals(1, def[0].outputShift);
        assertEquals(0, def[0].byteOffset);
        assertEquals(1, def.length);
    }

    @Test
    public void testSpecGenSecondByte1() {
        ContinousBitSpec spec = new ContinousBitSpec(10, 8, "test");
        PatchDefinition[] def = PatchDefinition.fromBitSpec(spec);
        assertEquals(0b111, def[0].outputMask);
        assertEquals(3, def[0].bitCount);
        assertEquals(0, def[0].outputShift);
        assertEquals(1, def[0].byteOffset);
        assertEquals(1, def.length);
    }

    @Test
    public void testSpecGenSecondByte2() {
        ContinousBitSpec spec = new ContinousBitSpec(15, 13, "test");
        PatchDefinition[] def = PatchDefinition.fromBitSpec(spec);
        assertEquals(0b1110_0000, def[0].outputMask);
        assertEquals(3, def[0].bitCount);
        assertEquals(5, def[0].outputShift);
        assertEquals(1, def[0].byteOffset);
        assertEquals(1, def.length);
    }

    @Test
    public void testSpecGenSecondReverse() {
        BitSpec spec = new CompositeBitSpec(new ContinousBitSpec(3, 2, "left"), new ContinousBitSpec(10, 8, "right"));
        PatchDefinition[] def = PatchDefinition.fromBitSpec(spec);
        assertEquals(2, def.length);
        assertEquals(0b111, def[0].outputMask);
        assertEquals(0, def[0].inputShift);
        assertEquals(3, def[0].bitCount);
        assertEquals(0, def[0].outputShift);
        assertEquals(1, def[0].byteOffset);

        assertEquals(0b1100, def[1].outputMask);
        assertEquals(3, def[1].inputShift);
        assertEquals(2, def[1].bitCount);
        assertEquals(2, def[1].outputShift);
        assertEquals(0, def[1].byteOffset);
    }

    @Test
    public void testSpecGenFourthByte1() {
        ContinousBitSpec spec = new ContinousBitSpec(26, 24, "test");
        PatchDefinition[] def = PatchDefinition.fromBitSpec(spec);
        assertEquals(0b111, def[0].outputMask);
        assertEquals(3, def[0].bitCount);
        assertEquals(0, def[0].outputShift);
        assertEquals(3, def[0].byteOffset);
        assertEquals(1, def.length);
    }

    @Test
    public void testSpecGenFourthByte2() {
        ContinousBitSpec spec = new ContinousBitSpec(31, 29, "test");
        PatchDefinition[] def = PatchDefinition.fromBitSpec(spec);
        assertEquals(0b1110_0000, def[0].outputMask);
        assertEquals(3, def[0].bitCount);
        assertEquals(5, def[0].outputShift);
        assertEquals(3, def[0].byteOffset);
        assertEquals(1, def.length);
    }

    @Test
    public void testSpecGenByteBoundary() {
        ContinousBitSpec spec = new ContinousBitSpec(9, 6, "test");
        PatchDefinition[] def = PatchDefinition.fromBitSpec(spec);
        assertEquals(2, def.length);
        assertEquals(0b1100_0000, def[0].outputMask);
        assertEquals(2, def[0].bitCount);
        assertEquals(6, def[0].outputShift);
        assertEquals(0, def[0].byteOffset);

        assertEquals(0b11, def[1].outputMask);
        assertEquals(2, def[1].bitCount);
        assertEquals(0, def[1].outputShift);
        assertEquals(1, def[1].byteOffset);
    }

    @Test
    public void testMultiWordSpec() {
        ContinousBitSpec spec = new ContinousBitSpec(7, 6, "test");
        ConcatBitSpec concatSpec = new ConcatBitSpec(spec, spec, spec);
        PatchDefinition[] def = PatchDefinition.fromBitSpec(concatSpec);
        assertEquals(3, def.length);
        assertEquals(0b1100_0000, def[0].outputMask);
        assertEquals(0, def[0].byteOffset);
        assertEquals(0b1100_0000, def[1].outputMask);
        assertEquals(4, def[1].byteOffset);
        assertEquals(0b1100_0000, def[2].outputMask);
        assertEquals(8, def[2].byteOffset);
    }

    @Test
    public void testPatcher64Bit() {
        ContinousBitSpec spec = new ContinousBitSpec(63, 0, true, 8, "i64");
        Patcher p = Patcher.from(spec);
        byte[] a = new byte[9];
        p.patch(a, 1, 0xCAFEBABEDEADDEADl);
        assertArrayEquals(new byte[]{0x00, (byte) 0xad, (byte) 0xde, (byte) 0xad, (byte) 0xde, (byte) 0xbe, (byte) 0xba, (byte) 0xfe, (byte) 0xca}, a);
    }

    @Test
    public void testDifferentSizeSignExtend() {
        Patcher patcher = new Patcher(new PatchGroup(
                        new PatchDefinition(00, 0b0000_0011, 0, false),
                        new PatchDefinition(02, 0b0000_0100, 0, true)),
                        new PatchGroup(new PatchDefinition(00, 0b0000_1111, 1, true)));

        byte[] b = new byte[8];
        ByteBuffer buff = ByteBuffer.wrap(b).order(ByteOrder.LITTLE_ENDIAN);
        int aMin = -1 << 2;
        int bMin = -1 << 3;
        int abMin = aMin + bMin;

        int[] patchValues = new int[]{aMin, bMin, abMin};

        for (int i = 0; i < patchValues.length; i++) {
            buff.position(0);
            int imm = patchValues[i];
            patcher.patch(b, 0, imm);
            int aByte = buff.get();
            int bByte = buff.get();
            int sum = signExtend(aByte, 2) + signExtend(bByte, 3);

            assertEquals(imm, sum);
        }
    }

    @Test
    public void testPatch323() {
        long imm = 0xfffffffffff00100l;
        Patcher patcher = new Patcher(new PatchDefinition[]{
                        new PatchDefinition(00, 0b0110_0000, 3, false), // adrp
                        new PatchDefinition(02, 0b0110_0000, 0, false), // adrp
                        new PatchDefinition(04, 0b1000_0000, 0, false), // adrp
                        new PatchDefinition(05, 0b1111_1111, 1, false), // adrp
                        new PatchDefinition(13, 0b1111_1111, 2, true), // adrp
        });
        byte[] x = new byte[8];
        for (long input = -1 << 15; input < (1 << 16) - 1; input++) {
            imm = input;
            long aMin = -1l << 7;
            long aMax = (1l << 7) - 1;

            long bMin = Assembler.bitMask(63, 15);
            long bMax = Assembler.bitMask(15, 8);

            long a1 = Math.min(aMax, Math.max(aMin, imm));
            imm -= a1;
            long a2 = Math.min(aMax, Math.max(aMin, imm));
            imm -= a2;

            long b1 = Math.min(bMax, Math.max(bMin, imm));
            imm -= b1;
            long b2 = Math.min(bMax, Math.max(bMin, imm));
            imm -= b2;
            long sum = a1 + a2 + b1 + b2;
            assertEquals(input, sum);
            assertEquals(0, imm);
        }
    }

    @Test
    public void testPatch324() {
        Assembler.PatchGroup pg1 = new Assembler.PatchGroup(
                        new PatchDefinition(0, 0xFF, 0, false),
                        new PatchDefinition(8, 0xFF, 1, true));
        Assembler.PatchGroup pg2 = new Assembler.PatchGroup(
                        new PatchDefinition(0, 0xFF, 2, false),
                        new PatchDefinition(8, 0xFF, 3, true));
        Patcher patcher = new Patcher(pg1, pg2);

        byte[] x = new byte[8];
        ByteBuffer buff = ByteBuffer.wrap(x).order(ByteOrder.LITTLE_ENDIAN);
        for (long input = -1 << 15; input < (1 << 16) - 1; input++) {
            patcher.patch(x, 0, input);
            long sum = buff.getShort(0) + buff.getShort(2);
            assertEquals(input, sum);
        }
    }

    /**
     * Test scenario from aarch64 adrp %r1, %pc + #imm; ldr [%r1 + #imm]
     */
    @Test
    public void testPatchAdrpLdr() {
        ContinousBitSpec immlo = new ContinousBitSpec(30, 29, false, "immlo");
        ContinousBitSpec immhi = new ContinousBitSpec(23, 5, true, "immhi");
        CompositeBitSpec adrp = new CompositeBitSpec(immhi, immlo);
        ContinousBitSpec imm9 = new ContinousBitSpec(20, 12, true, "imm9");
        ConcatBitSpec adrpLdr = new ConcatBitSpec(adrp, imm9);
        Patcher patcher = Patcher.from(adrpLdr);

        // @formatter:off
        // adrp: 1dd10000 dddddddd dddddddd dddeeeee -> dddddddd dddddddd d dd dd
        // ldr:  1x111000 010ddddd dddd01ee eeefffff ->              dddd d dd dd
        // @formatter:on
        patcher = new Patcher(new PatchGroup(new PatchDefinition(0, 0b0110_0000, 3, false),
                        new PatchDefinition(2, 0b1110_0000, 0, false),
                        new PatchDefinition(5, 0b1111_1111, 1, false),
                        new PatchDefinition(13, 0b1111_1111, 2, true)),
                        new PatchGroup(
                                        new PatchDefinition(0, 0b1111_0000, 5, false),
                                        new PatchDefinition(4, 0b0001_1111, 6, true)));

        byte[] b = new byte[8];
        ByteBuffer buff = ByteBuffer.wrap(b).order(ByteOrder.LITTLE_ENDIAN);
        buff.putLong(0xdeaddeaddeaddeadl);
        int adrpMin = -1 << (adrp.getWidth() - 1);
        int adrpMax = (1 << (adrp.getWidth() - 1)) - 1;
        int imm9Min = -1 << 8;
        int imm9Max = (1 << 8) - 1;
        int adrpLdrMin = adrpMin + imm9Min;
        int adrpLdrMax = adrpMax + imm9Max;

        int[] patchValues = new int[]{adrpLdrMin, adrpMin, imm9Min, adrpLdrMax, adrpMax, imm9Max};

        for (int i = 0; i < patchValues.length; i++) {
            buff.position(0);
            int imm = patchValues[i];
            patcher.patch(b, 0, imm);
            int adrpInsn = buff.getInt();
            int result = (adrpInsn >> 29) & 0b11;
            result |= ((adrpInsn >> 5) & ((1 << 19) - 1)) << 2;
            result = signExtend(result, adrp.getWidth() - 1);
            int ldrInsn = buff.getInt();
            result += signExtend((ldrInsn >> 12) & ((1 << 9) - 1), 8);
            assertEquals(imm, result);
        }
    }

    @Test
    public void testPatchAddContinousSpecs2() {
        Patcher abPatcher = new Patcher(new PatchGroup(
                        new PatchDefinition(0, 0xFF, 0, false),
                        new PatchDefinition(8, 0xFF, 1, false)),
                        new PatchGroup(new PatchDefinition(0, 0xFF, 2, false),
                                        new PatchDefinition(8, 0xFF, 3, false)));
        byte[] array = new byte[8];
        ByteBuffer buff = ByteBuffer.wrap(array).order(ByteOrder.LITTLE_ENDIAN);
        buff.putLong(0xdeaddeaddeaddeadl);

        int[] patchValues = new int[]{2 * 0xcafe, 0xcafe, 0, 0xFFFF, 2 * 0xFFFF};

        for (int i = 0; i < patchValues.length; i++) {
            buff.position(0);
            int imm = patchValues[i];
            abPatcher.patch(array, 0, imm);
            int a = buff.getShort() & 0xFFFF;
            int b = buff.getShort() & 0xFFFF;
            assertEquals(imm, a + b);
        }
    }

    @Test
    public void testPatchAddContinousSpecs3() {
        Patcher abPatcher = new Patcher(new PatchGroup(new PatchDefinition(0, 0xFF, 0, false)),
                        new PatchGroup(new PatchDefinition(0, 0xFF, 1, false)));
        byte[] array = new byte[8];
        ByteBuffer buff = ByteBuffer.wrap(array).order(ByteOrder.LITTLE_ENDIAN);
        buff.putLong(0xdeaddeaddeaddeadl);

        int[] patchValues = new int[]{2 * 0xfe, 0xfe, 0, 0xFF, 2 * 0xFF};

        for (int i = 0; i < patchValues.length; i++) {
            buff.position(0);
            int imm = patchValues[i];
            abPatcher.patch(array, 0, imm);
            int a = buff.get() & 0xFF;
            int b = buff.get() & 0xFF;
            assertEquals(imm, a + b);
        }
    }

    private static final int signExtend(int value, int bit) {
        int shiftAmt = 31 - bit;
        return (value << shiftAmt) >> shiftAmt;
    }

    @Test
    public void testPatchContinouesSignExtend() {
        ContinousBitSpec immhi = new ContinousBitSpec(2, 0, true, "immhi");
        Patcher patcher = Patcher.from(immhi);
        byte[] b = new byte[8];
        ByteBuffer buff = ByteBuffer.wrap(b).order(ByteOrder.LITTLE_ENDIAN);
        buff.putLong(0xdeaddeaddeaddeadl);
        long imm = -1;
        buff.position(0);
        patcher.patch(b, 0, imm);
        int i = buff.getInt();
        int leadingZeros = 32 - 3;
        i = (i << leadingZeros) >> leadingZeros;
        assertEquals(imm, i);
    }

    @Test
    public void testPatchShift() {
        // Test the patch starting with the third bit from input
        PatchDefinition[] def = new PatchDefinition[]{
                        new PatchDefinition(2, 0b0000_1111, 1, false) // inputShift==2 => skip
                                                                      // the lower 2 bits
        };
        Patcher p = new Patcher(def);
        byte[] b = new byte[3];
        p.patch(b, 1, 0);
        try {
            p.patch(b, 1, 1);
            fail("Expected assertion error");
        } catch (AssertionError e) {
            // success
        }
        try {
            p.patch(b, 1, 2);
            fail("Expected assertion error");
        } catch (AssertionError e) {
            // success
        }
        try {
            p.patch(b, 1, 3);
            fail("Expected assertion error");
        } catch (AssertionError e) {
            // success
        }
        p.patch(b, 1, 4);
    }

    @Test
    public void testPatchDefinitionAddUnsigned() {
        Patcher p = new Patcher(new PatchGroup(new PatchDefinition(0, 0b0000_1111, 1, false)), new PatchGroup(new PatchDefinition(0, 0b0011_1100, 0, false)));
        byte[] b = new byte[3];
        for (int i = 0x0; i < 0x1e; i++) {
            p.patch(b, 1, i);
            assertEquals(i, (b[1] >> 2) + b[2]);
        }
    }

    @Test
    public void testPatchDefinitionAddSigned() {
        Patcher p = new Patcher(new PatchGroup(new PatchDefinition(0, 0b0000_1111, 1, true)),
                        new PatchGroup(new PatchDefinition(0, 0b0001_1110, 0, true)));
        byte[] b = new byte[3];
        for (int i = -16; i < 15; i++) {
            p.patch(b, 1, i);
            int sum = signExtend(b[2], 3) + (signExtend(b[1], 4) >> 1);
            assertEquals(i, sum);
        }
    }

    @Test
    public void testPatchDefinitionAddShift() {
        PatchDefinition[] def = new PatchDefinition[]{
                        new PatchDefinition(4, 0b0000_1111, 1, false),
                        new PatchDefinition(0, 0b0000_1111, 0, false),
        };
        Patcher p = new Patcher(def);
        byte[] b = new byte[3];
        p.patch(b, 1, 0xCA);
        assertEquals(0xA, b[1]);
        assertEquals(0xC, b[2]);
    }

    @Test
    public void testContinousSignExtend() {
        testSetGet(d4hi, 0x00700000, 0x00000007);
        testSetGet(d4hi, 0x00800000, 0xFFFFFFF8);
    }

    @Test
    public void testContinousZeroExtend() {
        testSetGet(d4lo, 0x000000F0, 0x0000000F);
        testSetGet(d4lo, 0x00000070, 0x00000007);
    }

    public void testSetGet(BitSpec bs, int encoded, int decoded) {
        assertTrue(bs.valueFits(decoded));
        assertEquals(encoded, bs.setBits(0, decoded));
        assertEquals(decoded, bs.getBits(encoded));
    }

    @Test
    public void testContinousSignExtendValueFits() {
        assertFalse(d4hi.valueFits(0xf));
        assertFalse(d4hi.valueFits(0x10));
        assertFalse(d4hi.valueFits(0x17));
    }

    @Test
    public void testContinousZeroExtendValueFits() {
        assertFalse(d4lo.valueFits(0x10));
    }

    @Test(expected = AssertionError.class)
    public void testContinousSignExtendSetFail1() {
        d4hi.setBits(0, 0xf);
    }

    @Test(expected = AssertionError.class)
    public void testContinousSignExtendSetFail2() {
        d4hi.setBits(0, 0xFFFFFFF0);
    }

    @Test(expected = AssertionError.class)
    public void testContinousZeroExtendSetFail1() {
        d4lo.setBits(0, 0x10);
    }

    @Test
    public void testCompositeSignExtended() {
        testSetGet(d8, 0x00f000c0, 0xfffffffc);
        testSetGet(d8, 0x008000c0, 0xffffff8c);
        testSetGet(d8, 0x007000c0, 0x7c);
    }

    @Test(expected = AssertionError.class)
    public void testCompositeSignExtendedFail1() {
        d8.setBits(0, 0x00000080);
    }

    @Test(expected = AssertionError.class)
    public void testCompositeSignExtendedFail2() {
        d8.setBits(0, 0xEFFFFF80);
    }

    @Test
    public void testCompositeValueFits() {
        assertTrue(d8.valueFits(0xfffffffc));
        assertTrue(d8.valueFits(0xffffff8c));
        assertTrue(d8.valueFits(0x7c));
        assertFalse(d8.valueFits(0x8c));
        assertFalse(d8.valueFits(0xEFFFFF80));
    }
}
