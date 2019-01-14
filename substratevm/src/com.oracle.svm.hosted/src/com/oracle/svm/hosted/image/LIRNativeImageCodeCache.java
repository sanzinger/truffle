/*
 * Copyright (c) 2017, 2018, Oracle and/or its affiliates. All rights reserved.
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
package com.oracle.svm.hosted.image;

import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.NavigableMap;
import java.util.Set;
import java.util.TreeMap;

import org.graalvm.compiler.asm.amd64.AMD64BaseAssembler.OperandDataAnnotation;
import org.graalvm.compiler.code.CompilationResult;
import org.graalvm.compiler.core.common.NumUtil;
import org.graalvm.compiler.debug.DebugContext;
import org.graalvm.compiler.debug.GraalError;
import org.graalvm.compiler.debug.Indent;
import org.graalvm.nativeimage.ImageSingletons;
import org.graalvm.word.WordFactory;

import com.oracle.graal.pointsto.meta.AnalysisMethod;
import com.oracle.objectfile.ObjectFile;
import com.oracle.svm.core.SubstrateOptions;
import com.oracle.svm.core.code.CodeInfoEncoder;
import com.oracle.svm.core.code.CodeInfoQueryResult;
import com.oracle.svm.core.code.CodeInfoTable;
import com.oracle.svm.core.code.FrameInfoDecoder;
import com.oracle.svm.core.code.ImageCodeInfo;
import com.oracle.svm.core.graal.code.CGlobalDataReference;
import com.oracle.svm.core.graal.code.InstructionPatcher;
import com.oracle.svm.core.util.Counter;
import com.oracle.svm.core.util.VMError;
import com.oracle.svm.hosted.NativeImageOptions;
import com.oracle.svm.hosted.code.CompilationInfoSupport;
import com.oracle.svm.hosted.image.NativeBootImage.NativeTextSectionImpl;
import com.oracle.svm.hosted.meta.HostedMethod;
import com.oracle.svm.hosted.meta.MethodPointer;

import jdk.vm.ci.code.site.Call;
import jdk.vm.ci.code.site.ConstantReference;
import jdk.vm.ci.code.site.DataPatch;
import jdk.vm.ci.code.site.DataSectionReference;
import jdk.vm.ci.code.site.Infopoint;
import jdk.vm.ci.code.site.Reference;

public class LIRNativeImageCodeCache extends NativeImageCodeCache {

    public static final int CODE_ALIGNMENT = 16;
    private static final byte CODE_FILLER_BYTE = (byte) 0xCC;

    private final NavigableMap<Integer, CompilationResult> compilationsByStart = new TreeMap<>();

    private int codeCacheSize;

    public LIRNativeImageCodeCache(Map<HostedMethod, CompilationResult> compilations, NativeImageHeap imageHeap) {
        super(compilations, imageHeap);
    }

    @Override
    public int getCodeCacheSize() {
        assert codeCacheSize > 0;
        return codeCacheSize;
    }

    public CompilationResult getCompilationAtOffset(int offset) {
        Entry<Integer, CompilationResult> floor = compilationsByStart.floorEntry(offset);
        if (floor != null) {
            return floor.getValue();
        } else {
            return null;
        }
    }

    @Override
    public void layout(DebugContext debug) {
        layoutMethods(debug);
        layoutConstants();
    }

    @Override
    protected void printCompilationResult(HostedMethod method, CompilationResult result) {
        System.out.format("%8d %5d %s: frame %d\n", method.getCodeAddressOffset(), result.getTargetCodeSize(), method.format("%H.%n(%p)"), result.getTotalFrameSize());
    }

    @SuppressWarnings("try")
    private void layoutMethods(DebugContext debug) {

        try (Indent indent = debug.logAndIndent("layout methods")) {

            // Assign a location to all methods.
            assert codeCacheSize == 0;
            HostedMethod firstMethod = null;
            for (Entry<HostedMethod, CompilationResult> entry : compilations.entrySet()) {

                HostedMethod method = entry.getKey();
                if (firstMethod == null) {
                    firstMethod = method;
                }
                CompilationResult compilation = entry.getValue();
                compilationsByStart.put(codeCacheSize, compilation);
                method.setCodeAddressOffset(codeCacheSize);
                codeCacheSize = NumUtil.roundUp(codeCacheSize + compilation.getTargetCodeSize(), CODE_ALIGNMENT);
            }

            // Build run-time metadata.
            FrameInfoCustomization frameInfoCustomization = new FrameInfoCustomization();
            CodeInfoEncoder codeInfoEncoder = new CodeInfoEncoder(frameInfoCustomization, null);
            for (Entry<HostedMethod, CompilationResult> entry : compilations.entrySet()) {
                final HostedMethod method = entry.getKey();
                final CompilationResult compilation = entry.getValue();
                codeInfoEncoder.addMethod(method, compilation, method.getCodeAddressOffset());
            }

            if (NativeImageOptions.PrintMethodHistogram.getValue()) {
                System.out.println("encoded deopt entry points                 ; " + frameInfoCustomization.numDeoptEntryPoints);
                System.out.println("encoded during call entry points           ; " + frameInfoCustomization.numDuringCallEntryPoints);
            }

            ImageCodeInfo imageCodeInfo = CodeInfoTable.getImageCodeCache();
            codeInfoEncoder.encodeAll();
            codeInfoEncoder.install(imageCodeInfo);
            imageCodeInfo.setData(MethodPointer.factory(firstMethod), WordFactory.unsigned(codeCacheSize));

            if (CodeInfoEncoder.Options.CodeInfoEncoderCounters.getValue()) {
                for (Counter counter : ImageSingletons.lookup(CodeInfoEncoder.Counters.class).group.getCounters()) {
                    System.out.println(counter.getName() + " ; " + counter.getValue());
                }
            }

            if (Options.VerifyDeoptimizationEntryPoints.getValue()) {
                /*
                 * Missing deoptimization entry points lead to hard-to-debug transient failures, so
                 * we want the verification on all the time and not just when assertions are on.
                 */
                verifyDeoptEntries(imageCodeInfo);
            }

            assert verifyMethods(codeInfoEncoder);
        }
    }

    private void verifyDeoptEntries(ImageCodeInfo imageCodeInfo) {
        boolean hasError = false;
        List<Entry<AnalysisMethod, Set<Long>>> deoptEntries = new ArrayList<>(CompilationInfoSupport.singleton().getDeoptEntries().entrySet());
        deoptEntries.sort((e1, e2) -> e1.getKey().format("%H.%n(%p)").compareTo(e2.getKey().format("%H.%n(%p)")));

        for (Entry<AnalysisMethod, Set<Long>> entry : deoptEntries) {
            HostedMethod method = imageHeap.getUniverse().lookup(entry.getKey());
            List<Long> encodedBcis = new ArrayList<>(entry.getValue());
            encodedBcis.sort((v1, v2) -> Long.compare(v1, v2));

            for (long encodedBci : encodedBcis) {
                hasError |= verifyDeoptEntry(imageCodeInfo, method, encodedBci);
            }
        }
        if (hasError) {
            VMError.shouldNotReachHere("Verification of deoptimization entry points failed");
        }
    }

    private static boolean verifyDeoptEntry(ImageCodeInfo imageCodeInfo, HostedMethod method, long encodedBci) {
        int deoptOffsetInImage = method.getDeoptOffsetInImage();
        if (deoptOffsetInImage <= 0) {
            return error(method, encodedBci, "entry point method not compiled");
        }

        CodeInfoQueryResult result = new CodeInfoQueryResult();
        long relativeIP = imageCodeInfo.lookupDeoptimizationEntrypoint(deoptOffsetInImage, encodedBci, result);
        if (relativeIP < 0) {
            return error(method, encodedBci, "entry point not found");
        }
        if (result.getFrameInfo() == null || !result.getFrameInfo().isDeoptEntry() || result.getFrameInfo().getEncodedBci() != encodedBci) {
            return error(method, encodedBci, "entry point found, but wrong property");
        }
        return false;
    }

    private static boolean error(HostedMethod method, long encodedBci, String msg) {
        System.out.println(method.format("%H.%n(%p)") + ", encodedBci " + encodedBci + " (bci " + FrameInfoDecoder.readableBci(encodedBci) + "): " + msg);
        return true;
    }

    private boolean verifyMethods(CodeInfoEncoder codeInfoEncoder) {
        for (Entry<HostedMethod, CompilationResult> entry : compilations.entrySet()) {
            codeInfoEncoder.verifyMethod(entry.getValue(), entry.getKey().getCodeAddressOffset());
        }
        return true;
    }

    /**
     * Patch references from code to other code and constant data. Generate relocation information
     * in the process. More patching can be done, and correspondingly fewer relocation records
     * generated, if the caller passes a non-null rodataDisplacementFromText.
     *
     * @param relocs a relocation map
     */
    @Override
    public void patchMethods(RelocatableBuffer relocs, ObjectFile objectFile) {

        /*
         * Patch instructions which reference code or data by address.
         *
         * Note that the image we write happens to be naturally position-independent on x86-64,
         * since both code and data references are PC-relative.
         *
         * So not only can we definitively fix up the all code--code and code--data references as
         * soon as we have assigned all our addresses, but also, the resulting blob can be loaded at
         * any address without relocation (and therefore potentially shared between many processes).
         * (This is true for shared library output only, not relocatable code.)
         *
         * These properties may change. Once the code includes references to external symbols, we
         * will either no longer have a position-independent image (if we stick with the current
         * load-time relocation approach) or will require us to implement a PLT (for
         * {code,data}->code references) and GOT (for code->data references).
         *
         * Splitting text from rodata is straightforward when generating shared libraries or
         * executables, since even in the case where the loader has to pick a different virtual
         * address range than the one preassigned in the object file, it will preserve the offsets
         * between the vaddrs. So, if we're generating a shared library or executable (i.e.
         * something with vaddrs), we always know the offset of our data from our code (and
         * vice-versa). BUT if we're generating relocatable code, we don't know that yet. In that
         * case, the caller will pass a null rodataDisplacecmentFromText, and we behave accordingly
         * by generating extra relocation records.
         */

        // in each compilation result...
        for (Entry<HostedMethod, CompilationResult> entry : compilations.entrySet()) {
            HostedMethod method = entry.getKey();
            CompilationResult compilation = entry.getValue();

            // the codecache-relative offset of the compilation
            int compStart = method.getCodeAddressOffset();

            InstructionPatcher patcher = new InstructionPatcher(compilation);
            // ... patch direct call sites.
            for (Infopoint infopoint : compilation.getInfopoints()) {
                if (infopoint instanceof Call && ((Call) infopoint).direct) {
                    Call call = (Call) infopoint;

                    // NOTE that for the moment, we don't make static calls to external
                    // (e.g. native) functions. So every static call site has a target
                    // which is also in the code cache (a.k.a. a section-local call).
                    // This will change, and we will have to case-split here... but not yet.
                    int callTargetStart = ((HostedMethod) call.target).getCodeAddressOffset();

                    // Patch a PC-relative call.
                    // This code handles the case of section-local calls only.
                    int pcDisplacement = callTargetStart - (compStart + call.pcOffset);
                    OperandDataAnnotation patchData = patcher.findPatchData(call.pcOffset, pcDisplacement);
                    if (patchData != null) {
                        patchData.patch(pcDisplacement, compilation.getTargetCode());
                    } else {
                        throw new GraalError("Cannot find patch data for infopoint " + infopoint);
                    }
                }
            }
            // ... and patch references to constant data
            for (DataPatch dataPatch : compilation.getDataPatches()) {
                Reference ref = dataPatch.reference;
                /*
                 * Constants are allocated offsets in a separate space, which can be emitted as
                 * read-only (.rodata) section.
                 */
                OperandDataAnnotation patchData = patcher.findPatchData(dataPatch.pcOffset, 0);
                /*
                 * The relocation site is some offset into the instruction, which is some offset
                 * into the method, which is some offset into the text section (a.k.a. code cache).
                 * The offset we get out of the RelocationSiteInfo accounts for the first two, since
                 * we pass it the whole method. We add the method start to get the section-relative
                 * offset.
                 */
                long siteOffset = compStart + patchData.operandPosition;
                if (ref instanceof DataSectionReference || ref instanceof CGlobalDataReference) {
                    /*
                     * Do we have an addend? Yes; it's constStart. BUT x86/x86-64 PC-relative
                     * references are relative to the *next* instruction. So, if the next
                     * instruction starts n bytes from the relocation site, we want to subtract n
                     * bytes from our addend.
                     */
                    long addend = (patchData.nextInstructionPosition - patchData.operandPosition);
                    relocs.addPCRelativeRelocationWithAddend((int) siteOffset, patchData.operandSize, addend, ref);
                } else if (ref instanceof ConstantReference) {
                    assert SubstrateOptions.SpawnIsolates.getValue() : "Inlined object references must be base-relative";
                    relocs.addDirectRelocationWithoutAddend((int) siteOffset, patchData.operandSize, ref);
                } else {
                    throw VMError.shouldNotReachHere("Unknown type of reference in code");
                }
            }
        }
    }

    @Override
    public void writeCode(RelocatableBuffer buffer) {
        int startPos = buffer.getPosition();
        /*
         * Compilation start offsets are relative to the beginning of the code cache (since the heap
         * size is not fixed at the time they are computed). This is just startPos, i.e. we start
         * emitting the code wherever the buffer is positioned when we're called.
         */
        for (Entry<HostedMethod, CompilationResult> entry : compilations.entrySet()) {
            HostedMethod method = entry.getKey();
            CompilationResult compilation = entry.getValue();

            buffer.setPosition(startPos + method.getCodeAddressOffset());
            int codeSize = compilation.getTargetCodeSize();
            buffer.putBytes(compilation.getTargetCode(), 0, codeSize);

            for (int i = codeSize; i < NumUtil.roundUp(codeSize, CODE_ALIGNMENT); i++) {
                buffer.putByte(CODE_FILLER_BYTE);
            }
        }
        buffer.setPosition(startPos);
    }

    @Override
    public NativeTextSectionImpl getTextSectionImpl(RelocatableBuffer buffer, ObjectFile objectFile, NativeImageCodeCache codeCache) {
        return new NativeTextSectionImpl(buffer, objectFile, codeCache) {
            @Override
            protected void defineMethodSymbol(String name, ObjectFile.Element section, HostedMethod method, CompilationResult result) {
                final int size = result == null ? 0 : result.getTargetCodeSize();
                objectFile.createDefinedSymbol(name, section, method.getCodeAddressOffset(), size, true, true);
            }
        };
    }

    @Override
    public String[] getCCInputFiles(Path tempDirectory, String imageName) {
        String relocatableFileName = tempDirectory.resolve(imageName + ObjectFile.getFilenameSuffix()).toString();
        return new String[]{relocatableFileName};
    }
}
