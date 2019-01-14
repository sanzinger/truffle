/*
 * Copyright (c) 2018, 2018, Oracle and/or its affiliates. All rights reserved.
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
package com.oracle.svm.core.graal.code.aarch64;

import static com.oracle.svm.core.util.VMError.shouldNotReachHere;
import static com.oracle.svm.core.util.VMError.unimplemented;
import static jdk.vm.ci.aarch64.AArch64.lr;
import static jdk.vm.ci.aarch64.AArch64.sp;
import static jdk.vm.ci.hotspot.aarch64.AArch64HotSpotRegisterConfig.fp;
import static org.graalvm.compiler.core.common.GraalOptions.ZapStackOnMethodEntry;
import static org.graalvm.compiler.lir.LIRValueUtil.asConstantValue;

import com.oracle.svm.core.config.ConfigurationValues;
import org.graalvm.compiler.asm.Assembler;
import org.graalvm.compiler.asm.aarch64.AArch64Address;
import org.graalvm.compiler.asm.aarch64.AArch64Assembler;
import org.graalvm.compiler.asm.aarch64.AArch64Assembler.PrefetchMode;
import org.graalvm.compiler.asm.aarch64.AArch64MacroAssembler;
import org.graalvm.compiler.asm.aarch64.AArch64MacroAssembler.ScratchRegister;
import org.graalvm.compiler.code.CompilationResult;
import org.graalvm.compiler.core.aarch64.AArch64ArithmeticLIRGenerator;
import org.graalvm.compiler.core.aarch64.AArch64LIRGenerator;
import org.graalvm.compiler.core.aarch64.AArch64LIRKindTool;
import org.graalvm.compiler.core.aarch64.AArch64MoveFactory;
import org.graalvm.compiler.core.aarch64.AArch64NodeLIRBuilder;
import org.graalvm.compiler.core.aarch64.AArch64NodeMatchRules;
import org.graalvm.compiler.core.common.CompilationIdentifier;
import org.graalvm.compiler.core.common.CompressEncoding;
import org.graalvm.compiler.core.common.LIRKind;
import org.graalvm.compiler.core.common.alloc.RegisterAllocationConfig;
import org.graalvm.compiler.core.common.spi.ForeignCallLinkage;
import org.graalvm.compiler.core.common.spi.LIRKindTool;
import org.graalvm.compiler.core.gen.DebugInfoBuilder;
import org.graalvm.compiler.core.gen.LIRGenerationProvider;
import org.graalvm.compiler.core.target.Backend;
import org.graalvm.compiler.debug.DebugContext;
import org.graalvm.compiler.lir.ConstantValue;
import org.graalvm.compiler.lir.LIR;
import org.graalvm.compiler.lir.LIRFrameState;
import org.graalvm.compiler.lir.LIRInstruction;
import org.graalvm.compiler.lir.LIRInstructionClass;
import org.graalvm.compiler.lir.LabelRef;
import org.graalvm.compiler.lir.Opcode;
import org.graalvm.compiler.lir.StandardOp;
import org.graalvm.compiler.lir.StandardOp.LoadConstantOp;
import org.graalvm.compiler.lir.StandardOp.BlockEndOp;
import org.graalvm.compiler.lir.StandardOp.SaveRegistersOp;
import org.graalvm.compiler.lir.Variable;
import org.graalvm.compiler.lir.aarch64.AArch64BreakpointOp;
import org.graalvm.compiler.lir.aarch64.AArch64Call;
import org.graalvm.compiler.lir.aarch64.AArch64ControlFlow;
import org.graalvm.compiler.lir.aarch64.AArch64FrameMap;
import org.graalvm.compiler.lir.aarch64.AArch64FrameMapBuilder;
import org.graalvm.compiler.lir.aarch64.AArch64LIRInstruction;
import org.graalvm.compiler.lir.aarch64.AArch64Move;
import org.graalvm.compiler.lir.aarch64.AArch64Move.PointerCompressionOp;
import org.graalvm.compiler.lir.aarch64.AArch64PrefetchOp;
import org.graalvm.compiler.lir.asm.CompilationResultBuilder;
import org.graalvm.compiler.lir.asm.CompilationResultBuilderFactory;
import org.graalvm.compiler.lir.asm.DataBuilder;
import org.graalvm.compiler.lir.asm.FrameContext;
import org.graalvm.compiler.lir.framemap.FrameMap;
import org.graalvm.compiler.lir.framemap.FrameMapBuilder;
import org.graalvm.compiler.lir.framemap.FrameMapBuilderTool;
import org.graalvm.compiler.lir.framemap.ReferenceMapBuilder;
import org.graalvm.compiler.lir.gen.LIRGenerationResult;
import org.graalvm.compiler.lir.gen.LIRGeneratorTool;
import org.graalvm.compiler.nodes.BreakpointNode;
import org.graalvm.compiler.nodes.DirectCallTargetNode;
import org.graalvm.compiler.nodes.IndirectCallTargetNode;
import org.graalvm.compiler.nodes.LogicNode;
import org.graalvm.compiler.nodes.NodeView;
import org.graalvm.compiler.nodes.SafepointNode;
import org.graalvm.compiler.nodes.StructuredGraph;
import org.graalvm.compiler.nodes.spi.NodeLIRBuilderTool;
import org.graalvm.compiler.nodes.spi.NodeValueMap;
import org.graalvm.compiler.options.OptionValues;
import org.graalvm.compiler.phases.Phase;
import org.graalvm.compiler.phases.common.AddressLoweringByUsePhase;
import org.graalvm.compiler.phases.tiers.SuitesProvider;
import org.graalvm.compiler.phases.util.Providers;
import org.graalvm.nativeimage.Feature;
import org.graalvm.nativeimage.ImageSingletons;
import org.graalvm.nativeimage.Platform;
import org.graalvm.nativeimage.Platforms;

import com.oracle.svm.core.FrameAccess;
import com.oracle.svm.core.SubstrateOptions;
import com.oracle.svm.core.annotate.AutomaticFeature;
import com.oracle.svm.core.deopt.DeoptimizedFrame;
import com.oracle.svm.core.deopt.Deoptimizer;
import com.oracle.svm.core.graal.code.SubstrateBackend;
import com.oracle.svm.core.graal.code.SubstrateBackendFactory;
import com.oracle.svm.core.graal.code.SubstrateCallingConventionType;
import com.oracle.svm.core.graal.code.SubstrateCompiledCode;
import com.oracle.svm.core.graal.code.SubstrateDataBuilder;
import com.oracle.svm.core.graal.code.SubstrateLIRGenerator;
import com.oracle.svm.core.graal.code.SubstrateNodeLIRBuilder;
import com.oracle.svm.core.graal.meta.SubstrateForeignCallLinkage;
import com.oracle.svm.core.graal.meta.SubstrateRegisterConfig;
import com.oracle.svm.core.graal.nodes.CGlobalDataLoadAddressNode;
import com.oracle.svm.core.heap.ReferenceAccess;
import com.oracle.svm.core.heap.SubstrateReferenceMapBuilder;
import com.oracle.svm.core.meta.CompressedNullConstant;
import com.oracle.svm.core.meta.SharedMethod;
import com.oracle.svm.core.meta.SharedType;
import com.oracle.svm.core.meta.SubstrateObjectConstant;
import com.oracle.svm.core.nodes.SafepointCheckNode;

import jdk.vm.ci.aarch64.AArch64;
import jdk.vm.ci.code.CallingConvention;
import jdk.vm.ci.code.CodeCacheProvider;
import jdk.vm.ci.code.CodeUtil;
import jdk.vm.ci.code.CompilationRequest;
import jdk.vm.ci.code.CompiledCode;
import jdk.vm.ci.code.Register;
import jdk.vm.ci.code.RegisterConfig;
import jdk.vm.ci.code.RegisterValue;
import jdk.vm.ci.code.StackSlot;
import jdk.vm.ci.meta.AllocatableValue;
import jdk.vm.ci.meta.Constant;
import jdk.vm.ci.meta.JavaConstant;
import jdk.vm.ci.meta.JavaKind;
import jdk.vm.ci.meta.JavaType;
import jdk.vm.ci.meta.ResolvedJavaMethod;
import jdk.vm.ci.meta.Value;

@AutomaticFeature
@Platforms(Platform.AArch64.class)
class SubstrateAArch64BackendFeature implements Feature {
    @Override
    public void afterRegistration(AfterRegistrationAccess access) {
        ImageSingletons.add(SubstrateBackendFactory.class, new SubstrateBackendFactory() {
            @Override
            public SubstrateBackend newBackend(Providers newProviders) {
                return new SubstrateAArch64Backend(newProviders);
            }
        });
    }
}

public class SubstrateAArch64Backend extends SubstrateBackend implements LIRGenerationProvider {

    public static final String MARK_PROLOGUE_DECD_RSP = "PROLOGUE_DECD_RSP";
    public static final String MARK_PROLOGUE_SAVED_REGS = "PROLOGUE_SAVED_REGS";
    public static final String MARK_PROLOGUE_END = "PROLOGUE_END";
    public static final String MARK_EPILOGUE_START = "EPILOGUE_START";
    public static final String MARK_EPILOGUE_INCD_RSP = "EPILOGUE_INCD_RSP";
    public static final String MARK_EPILOGUE_END = "EPILOGUE_END";

    protected static CompressEncoding getCompressEncoding() {
        return ImageSingletons.lookup(CompressEncoding.class);
    }

    public SubstrateAArch64Backend(Providers providers) {
        super(providers);
    }

    /**
     * A direct call, but without alignment nops for the call instruction. Used for fatal exception
     * calls.
     */
    @Opcode("CALL_DIRECT")
    public static class SubstrateAArch64DirectCallOp extends AArch64Call.DirectCallOp {
        public static final LIRInstructionClass<SubstrateAArch64DirectCallOp> TYPE = LIRInstructionClass.create(SubstrateAArch64DirectCallOp.class);

        public SubstrateAArch64DirectCallOp(ResolvedJavaMethod callTarget, Value result, Value[] parameters, Value[] temps, LIRFrameState state) {
            super(TYPE, callTarget, result, parameters, temps, state);
        }

        @Override
        public void emitCode(CompilationResultBuilder tasm, AArch64MacroAssembler masm) {
            AArch64Call.directCall(tasm, masm, callTarget, null, state);
        }
    }

    /**
     * Marks a point that is unreachable because a previous instruction never returns.
     */
    @Opcode("DEAD_END")
    public static class DeadEndOp extends LIRInstruction implements BlockEndOp {
        public static final LIRInstructionClass<DeadEndOp> TYPE = LIRInstructionClass.create(DeadEndOp.class);

        public DeadEndOp() {
            super(TYPE);
        }

        @Override
        public void emitCode(CompilationResultBuilder crb) {
            /*
             * We could add some code in debug builds here checking that it is really unreachable.
             */
        }
    }

    protected static final class SubstrateLIRGenerationResult extends LIRGenerationResult {

        private final SharedMethod method;

        public SubstrateLIRGenerationResult(CompilationIdentifier compilationId, LIR lir, FrameMapBuilder frameMapBuilder, CallingConvention callingConvention, SharedMethod method) {
            super(compilationId, lir, frameMapBuilder, callingConvention);
            this.method = method;

            if (method.canDeoptimize() || method.isDeoptTarget()) {
                ((FrameMapBuilderTool) frameMapBuilder).getFrameMap().reserveOutgoing(16);
            }
        }

        public SharedMethod getMethod() {
            return method;
        }
    }

    protected final class SubstrateAArch64LIRGenerator extends AArch64LIRGenerator implements SubstrateLIRGenerator {

        public SubstrateAArch64LIRGenerator(LIRKindTool lirKindTool, AArch64ArithmeticLIRGenerator arithmeticLIRGen, MoveFactory moveFactory, Providers providers, LIRGenerationResult lirGenRes) {
            super(lirKindTool, arithmeticLIRGen, moveFactory, providers, lirGenRes);
        }

        @Override
        public SubstrateLIRGenerationResult getResult() {
            return (SubstrateLIRGenerationResult) super.getResult();
        }

        @Override
        public SubstrateRegisterConfig getRegisterConfig() {
            return (SubstrateRegisterConfig) super.getRegisterConfig();
        }

        @Override
        protected void emitForeignCallOp(ForeignCallLinkage linkage, Value result, Value[] arguments, Value[] temps, LIRFrameState info) {
            SubstrateForeignCallLinkage callTarget = (SubstrateForeignCallLinkage) linkage;
            ResolvedJavaMethod targetMethod = callTarget.getMethod();
            append(new SubstrateAArch64DirectCallOp(targetMethod, result, arguments, temps, info));
        }

        @Override
        public void emitUnwind(Value operand) {
            throw shouldNotReachHere("handled by lowering");
        }

        @Override
        public void emitDeoptimize(Value actionAndReason, Value failedSpeculation, LIRFrameState state) {
            throw shouldNotReachHere("Substrate VM does not use deoptimization");
        }

        @Override
        public Value emitReadInstructionPointer() {
            throw unimplemented();
        }

        @Override
        public void emitFarReturn(AllocatableValue result, Value stackPointer, Value ip) {
            append(new AArch64FarReturnOp(asAllocatable(result), asAllocatable(stackPointer), asAllocatable(ip)));
        }

        @Override
        public void emitDeadEnd() {
            append(new DeadEndOp());
        }

        @Override
        public void emitPrefetchAllocate(Value address) {
            append(new AArch64PrefetchOp(asAddressValue(address), PrefetchMode.PSTL1KEEP));
        }

        @Override
        public Value emitCompress(Value pointer, CompressEncoding encoding, boolean isNonNull) {
            Variable result = newVariable(getLIRKindTool().getNarrowOopKind());
            boolean nonNull = useLinearPointerCompression() || isNonNull;
            append(new AArch64Move.CompressPointerOp(result, asAllocatable(pointer), getRegisterConfig().getHeapBaseRegister().asValue(), encoding, nonNull, getLIRKindTool()));
            return result;
        }

        @Override
        public Value emitUncompress(Value pointer, CompressEncoding encoding, boolean isNonNull) {
            assert pointer.getValueKind(LIRKind.class).getPlatformKind() == getLIRKindTool().getNarrowOopKind().getPlatformKind();
            Variable result = newVariable(getLIRKindTool().getObjectKind());
            boolean nonNull = useLinearPointerCompression() || isNonNull;
            append(new AArch64Move.UncompressPointerOp(result, asAllocatable(pointer), getRegisterConfig().getHeapBaseRegister().asValue(), encoding, nonNull, getLIRKindTool()));
            return result;
        }

        @Override
        public void emitCCall(long address, CallingConvention nativeCallingConvention, Value[] args) {
            throw unimplemented();
        }

        @Override
        public SaveRegistersOp createZapRegisters(Register[] zappedRegisters, JavaConstant[] zapValues) {
            throw unimplemented();
        }

        @Override
        public LIRInstruction createZapArgumentSpace(StackSlot[] zappedStack, JavaConstant[] zapValues) {
            throw unimplemented();
        }
    }

    public static final class SubstrateDebugInfoBuilder extends DebugInfoBuilder {
        public SubstrateDebugInfoBuilder(NodeValueMap nodeValueMap, DebugContext debug) {
            super(nodeValueMap, debug);
        }

        @Override
        protected JavaKind storageKind(JavaType type) {
            return ((SharedType) type).getStorageKind();
        }
    }

    public static final class SubstrateAArch64NodeLIRBuilder extends AArch64NodeLIRBuilder implements SubstrateNodeLIRBuilder {

        public SubstrateAArch64NodeLIRBuilder(StructuredGraph graph, LIRGeneratorTool gen, AArch64NodeMatchRules nodeMatchRules) {
            super(graph, gen, nodeMatchRules);
        }

        @Override
        public void visitSafepointNode(SafepointNode node) {
            throw shouldNotReachHere("handled by lowering");
        }

        @Override
        public void visitBreakpointNode(BreakpointNode node) {
            JavaType[] sig = new JavaType[node.arguments().size()];
            for (int i = 0; i < sig.length; i++) {
                sig[i] = node.arguments().get(i).stamp(NodeView.DEFAULT).javaType(gen.getMetaAccess());
            }

            CallingConvention convention = gen.getRegisterConfig().getCallingConvention(SubstrateCallingConventionType.JavaCall, null, sig, gen);
            append(new AArch64BreakpointOp(visitInvokeArguments(convention, node.arguments())));
        }

        @Override
        protected DebugInfoBuilder createDebugInfoBuilder(StructuredGraph graph, NodeValueMap nodeValueMap) {
            return new SubstrateDebugInfoBuilder(nodeValueMap, graph.getDebug());
        }

        @Override
        protected void emitDirectCall(DirectCallTargetNode callTarget, Value result, Value[] parameters, Value[] temps, LIRFrameState callState) {
            ResolvedJavaMethod targetMethod = callTarget.targetMethod();
            append(new SubstrateAArch64DirectCallOp(targetMethod, result, parameters, temps, callState));
        }

        @Override
        protected void emitIndirectCall(IndirectCallTargetNode callTarget, Value result, Value[] parameters, Value[] temps, LIRFrameState callState) {
            // The register allocator cannot handle variables at call sites, need a fixed register.
            Register targetRegister = AArch64.lr;
            AllocatableValue targetAddress = targetRegister.asValue(FrameAccess.getWordStamp().getLIRKind(getLIRGeneratorTool().getLIRKindTool()));
            gen.emitMove(targetAddress, operand(callTarget.computedAddress()));
            ResolvedJavaMethod targetMethod = callTarget.targetMethod();
            append(new AArch64Call.IndirectCallOp(targetMethod, result, parameters, temps, targetAddress, callState));
        }

        @Override
        public void emitBranch(LogicNode node, LabelRef trueSuccessor, LabelRef falseSuccessor, double trueSuccessorProbability) {
            if (node instanceof SafepointCheckNode) {
                append(new AArch64DecrementingSafepointCheckOp());
                append(new AArch64ControlFlow.BranchOp(AArch64Assembler.ConditionFlag.EQ, trueSuccessor, falseSuccessor, trueSuccessorProbability));
            } else {
                super.emitBranch(node, trueSuccessor, falseSuccessor, trueSuccessorProbability);
            }
        }

        @Override
        public void emitCGlobalDataLoadAddress(CGlobalDataLoadAddressNode node) {
            Variable result = gen.newVariable(gen.getLIRKindTool().getWordKind());
            append(new AArch64CGlobalDataLoadAddressOp(node.getDataInfo(), result));
            setResult(node, result);
        }

        @Override
        public Variable emitReadReturnAddress() {
            return getLIRGeneratorTool().emitMove(StackSlot.get(getLIRGeneratorTool().getLIRKind(FrameAccess.getWordStamp()), -FrameAccess.returnAddressSize() - FrameAccess.wordSize(), true));
        }
    }

    protected static class SubstrateAArch64FrameContext implements FrameContext {

        @Override
        public void enter(CompilationResultBuilder crb) {
            FrameMap frameMap = crb.frameMap;
            final int frameSize = frameMap.frameSize();
            final int totalFrameSize = frameMap.totalFrameSize();
            assert frameSize + 2 * crb.target.arch.getWordSize() == totalFrameSize : "total framesize should be framesize + 2 words";
            AArch64MacroAssembler masm = (AArch64MacroAssembler) crb.asm;
            crb.blockComment("[method prologue]");

            try (ScratchRegister sc = masm.getScratchRegister()) {
                int wordSize = crb.target.arch.getWordSize();
                Register rscratch1 = sc.getRegister();
                assert totalFrameSize > 0;
                if (frameSize < 1 << 9) {
                    masm.sub(64, sp, sp, totalFrameSize);
                    masm.stp(64, fp, lr, AArch64Address.createScaledImmediateAddress(sp, frameSize / wordSize));
                } else {
                    masm.stp(64, fp, lr, AArch64Address.createPreIndexedImmediateAddress(sp, -2));
                    if (frameSize < 1 << 12) {
                        masm.sub(64, sp, sp, totalFrameSize - 2 * wordSize);
                    } else {
                        masm.mov(rscratch1, totalFrameSize - 2 * wordSize);
                        masm.sub(64, sp, sp, rscratch1);
                    }
                }
            }
            if (ZapStackOnMethodEntry.getValue(crb.getOptions())) {
                try (ScratchRegister sc = masm.getScratchRegister()) {
                    Register scratch = sc.getRegister();
                    int longSize = 8;
                    masm.mov(64, scratch, sp);
                    AArch64Address address = AArch64Address.createPostIndexedImmediateAddress(scratch, longSize);
                    try (ScratchRegister sc2 = masm.getScratchRegister()) {
                        Register value = sc2.getRegister();
                        masm.mov(value, 0xBADDECAFFC0FFEEL);
                        for (int i = 0; i < frameSize; i += longSize) {
                            masm.str(64, value, address);
                        }
                    }

                }
            }
            crb.recordMark(MARK_PROLOGUE_DECD_RSP);
            crb.recordMark(MARK_PROLOGUE_END);
        }

        @Override
        public void leave(CompilationResultBuilder crb) {
            int frameSize = crb.frameMap.frameSize();

            crb.recordMark(MARK_EPILOGUE_START);
            AArch64MacroAssembler masm = (AArch64MacroAssembler) crb.asm;
            FrameMap frameMap = crb.frameMap;
            final int totalFrameSize = frameMap.totalFrameSize();

            crb.blockComment("[method epilogue]");
            try (ScratchRegister sc = masm.getScratchRegister()) {
                int wordSize = crb.target.arch.getWordSize();
                Register rscratch1 = sc.getRegister();
                assert totalFrameSize > 0;
                if (frameSize < 1 << 9) {
                    masm.ldp(64, fp, lr, AArch64Address.createScaledImmediateAddress(sp, frameSize / wordSize));
                    masm.add(64, sp, sp, totalFrameSize);
                } else {
                    if (frameSize < 1 << 12) {
                        masm.add(64, sp, sp, totalFrameSize - 2 * wordSize);
                    } else {
                        masm.mov(rscratch1, totalFrameSize - 2 * wordSize);
                        masm.add(64, sp, sp, rscratch1);
                    }
                    masm.ldp(64, fp, lr, AArch64Address.createPostIndexedImmediateAddress(sp, 2));
                }
            }
            if (frameSize != 0) {
                crb.recordMark(MARK_EPILOGUE_INCD_RSP);
            }
            crb.recordMark(MARK_EPILOGUE_END);
        }

        @Override
        public boolean hasFrame() {
            return true;
        }
    }

    /**
     * Generates the prolog of a {@link com.oracle.svm.core.deopt.Deoptimizer.StubType#EntryStub}
     * method.
     */
    protected static class DeoptEntryStubContext extends SubstrateAArch64FrameContext {
        @Override
        public void enter(CompilationResultBuilder tasm) {
            throw unimplemented();
        }
    }

    /**
     * Generates the epilog of a {@link com.oracle.svm.core.deopt.Deoptimizer.StubType#ExitStub}
     * method.
     */
    protected static class DeoptExitStubContext extends SubstrateAArch64FrameContext {
        @Override
        public void leave(CompilationResultBuilder tasm) {
            throw unimplemented();
        }
    }

    static class SubstrateReferenceMapBuilderFactory implements FrameMap.ReferenceMapBuilderFactory {
        @Override
        public ReferenceMapBuilder newReferenceMapBuilder(int totalFrameSize) {
            return new SubstrateReferenceMapBuilder(totalFrameSize);
        }
    }

    protected static class SubstrateAArch64MoveFactory extends AArch64MoveFactory {

        private final SharedMethod method;
        @SuppressWarnings("unused") private final LIRKindTool lirKindTool;
        @SuppressWarnings("unused") private final SubstrateAArch64RegisterConfig registerConfig;

        protected SubstrateAArch64MoveFactory(SharedMethod method, LIRKindTool lirKindTool, SubstrateAArch64RegisterConfig registerConfig) {
            super();
            this.method = method;
            this.lirKindTool = lirKindTool;
            this.registerConfig = registerConfig;
        }

        @Override
        public boolean allowConstantToStackMove(Constant constant) {
            if (constant instanceof SubstrateObjectConstant && method.isDeoptTarget()) {
                return false;
            }
            return super.allowConstantToStackMove(constant);
        }

        @Override
        public AArch64LIRInstruction createLoad(AllocatableValue dst, Constant src) {
            if (CompressedNullConstant.COMPRESSED_NULL.equals(src)) {
                return super.createLoad(dst, JavaConstant.INT_0);
            } else if (src instanceof SubstrateObjectConstant) {
                return loadObjectConstant(dst, (SubstrateObjectConstant) src);
            }
            return super.createLoad(dst, src);
        }

        @Override
        public LIRInstruction createStackLoad(AllocatableValue dst, Constant src) {
            if (CompressedNullConstant.COMPRESSED_NULL.equals(src)) {
                return super.createStackLoad(dst, JavaConstant.INT_0);
            } else if (src instanceof SubstrateObjectConstant) {
                return loadObjectConstant(dst, (SubstrateObjectConstant) src);
            }
            return super.createStackLoad(dst, src);
        }

        protected AArch64LIRInstruction loadObjectConstant(AllocatableValue dst, SubstrateObjectConstant constant) {
            if (ReferenceAccess.singleton().haveCompressedReferences()) {
                RegisterValue heapBase = registerConfig.getHeapBaseRegister().asValue();
                return new LoadCompressedObjectConstantOp(dst, constant, heapBase, getCompressEncoding(), lirKindTool);
            }
            return new AArch64Move.LoadInlineConstant(constant, dst);
        }


        /*
         * The constant denotes the result produced by this node. Thus if the constant is
         * compressed, the result must be compressed and vice versa. Both compressed and
         * uncompressed constants can be loaded by compiled code.
         *
         * Method getConstant() could uncompress the constant value from the node input. That would
         * require a few indirections and an allocation of an uncompressed constant. The allocation
         * could be eliminated if we stored uncompressed ConstantValue as input. But as this method
         * looks performance-critical, it is still faster to memorize the original constant in the
         * node.
         */
        public static final class LoadCompressedObjectConstantOp extends PointerCompressionOp implements LoadConstantOp {
            public static final LIRInstructionClass<LoadCompressedObjectConstantOp> TYPE = LIRInstructionClass.create(LoadCompressedObjectConstantOp.class);

            static JavaConstant asCompressed(SubstrateObjectConstant constant) {
                // We only want compressed references in code
                return constant.isCompressed() ? constant : constant.compress();
            }

            private final SubstrateObjectConstant constant;

            public LoadCompressedObjectConstantOp(AllocatableValue result, SubstrateObjectConstant constant, AllocatableValue baseRegister, CompressEncoding encoding, LIRKindTool lirKindTool) {
                super(TYPE, result, new ConstantValue(lirKindTool.getNarrowOopKind(), asCompressed(constant)), baseRegister, encoding, true, lirKindTool);
                this.constant = constant;
            }

            @Override
            public Constant getConstant() {
                return constant;
            }

            @Override
            public void emitCode(CompilationResultBuilder crb, AArch64MacroAssembler masm) {
                /*
                 * WARNING: must NOT have side effects. Preserve the flags register!
                 */
                Register resultReg = getResultRegister();
                int referenceSize = ConfigurationValues.getObjectLayout().getReferenceSize();
                Constant inputConstant = asConstantValue(getInput()).getConstant();
                if (masm.target.inlineObjects) {
                    crb.recordInlineDataInCode(inputConstant);
                    masm.mov(resultReg, 0xDEADDEADDEADDEADL);
                } else {
                    AArch64Address address = (AArch64Address) crb.recordDataReferenceInCode(inputConstant, referenceSize);
                    masm.loadAddress(resultReg, address, 1);
                }
                if (!constant.isCompressed()) { // the result is expected to be uncompressed
                    Register baseReg = getBaseRegister(crb);
                    assert !baseReg.equals(Register.None) || getShift() != 0 : "no compression in place";
                    masm.loadAddress(resultReg, AArch64Address.createRegisterOffsetAddress(baseReg, resultReg, true), getShift());
                }
            }
        }
    }

    public FrameMapBuilder newFrameMapBuilder(RegisterConfig registerConfig) {
        RegisterConfig registerConfigNonNull = registerConfig == null ? getCodeCache().getRegisterConfig() : registerConfig;
        return new AArch64FrameMapBuilder(newFrameMap(registerConfigNonNull), getCodeCache(), registerConfigNonNull);
    }

    public FrameMap newFrameMap(RegisterConfig registerConfig) {
        return new AArch64FrameMap(getProviders().getCodeCache(), registerConfig, new SubstrateReferenceMapBuilderFactory());
    }

    @Override
    public CompilationResultBuilder newCompilationResultBuilder(LIRGenerationResult lirGenResult, FrameMap frameMap, CompilationResult compilationResult, CompilationResultBuilderFactory factory) {
        Assembler masm = new AArch64MacroAssembler(getTarget());
        SharedMethod method = ((SubstrateLIRGenerationResult) lirGenResult).getMethod();
        Deoptimizer.StubType stubType = method.getDeoptStubType();
        DataBuilder dataBuilder = new SubstrateDataBuilder();
        final FrameContext frameContext;
        if (stubType == Deoptimizer.StubType.EntryStub) {
            frameContext = new DeoptEntryStubContext();
        } else if (stubType == Deoptimizer.StubType.ExitStub) {
            frameContext = new DeoptExitStubContext();
        } else {
            frameContext = new SubstrateAArch64FrameContext();
        }
        LIR lir = lirGenResult.getLIR();
        OptionValues options = lir.getOptions();
        DebugContext debug = lir.getDebug();
        Register nullRegister = useLinearPointerCompression() ? getHeapBaseRegister(lirGenResult) : Register.None;
        CompilationResultBuilder tasm = factory.createBuilder(getCodeCache(), getForeignCalls(), lirGenResult.getFrameMap(), masm, dataBuilder, frameContext, options, debug, compilationResult,
                        nullRegister);
        tasm.setTotalFrameSize(lirGenResult.getFrameMap().totalFrameSize());
        return tasm;
    }

    protected AArch64ArithmeticLIRGenerator createArithmeticLIRGen() {
        return new AArch64ArithmeticLIRGenerator();
    }

    protected static SubstrateAArch64RegisterConfig getRegisterConfig(LIRGenerationResult lirGenRes) {
        return (SubstrateAArch64RegisterConfig) lirGenRes.getRegisterConfig();
    }

    private static Register getHeapBaseRegister(LIRGenerationResult lirGenRes) {
        return getRegisterConfig(lirGenRes).getHeapBaseRegister();
    }

    protected AArch64MoveFactory createMoveFactory(LIRGenerationResult lirGenRes) {
        SharedMethod method = ((SubstrateLIRGenerationResult) lirGenRes).getMethod();
        return new SubstrateAArch64MoveFactory(method, createLirKindTool(), getRegisterConfig(lirGenRes));
    }

    protected static class SubstrateAArch64LIRKindTool extends AArch64LIRKindTool {

    }

    protected LIRKindTool createLirKindTool() {
        return new SubstrateAArch64LIRKindTool();
    }

    @Override
    public LIRGeneratorTool newLIRGenerator(LIRGenerationResult lirGenRes) {
        AArch64ArithmeticLIRGenerator arithmeticLIRGen = createArithmeticLIRGen();
        AArch64MoveFactory moveFactory = createMoveFactory(lirGenRes);
        return new SubstrateAArch64LIRGenerator(createLirKindTool(), arithmeticLIRGen, moveFactory, getProviders(), lirGenRes);
    }

    protected AArch64NodeMatchRules createMatchRules(LIRGeneratorTool lirGen) {
        return new AArch64NodeMatchRules(lirGen);
    }

    @Override
    public NodeLIRBuilderTool newNodeLIRBuilder(StructuredGraph graph, LIRGeneratorTool lirGen) {
        AArch64NodeMatchRules nodeMatchRules = createMatchRules(lirGen);
        return new SubstrateAArch64NodeLIRBuilder(graph, lirGen, nodeMatchRules);
    }

    private static boolean useLinearPointerCompression() {
        return SubstrateOptions.SpawnIsolates.getValue();
    }

    @Override
    public RegisterAllocationConfig newRegisterAllocationConfig(RegisterConfig registerConfig, String[] allocationRestrictedTo) {
        RegisterConfig registerConfigNonNull = registerConfig == null ? getCodeCache().getRegisterConfig() : registerConfig;
        return new RegisterAllocationConfig(registerConfigNonNull, allocationRestrictedTo);
    }

    @Override
    public CompilationResult createJNITrampolineMethod(ResolvedJavaMethod method, CompilationIdentifier identifier, RegisterValue methodIdArg, int offset) {
        CompilationResult result = new CompilationResult(identifier);
        AArch64MacroAssembler asm = new AArch64MacroAssembler(getTarget());
        try (ScratchRegister scratch = asm.getScratchRegister()) {
            Register scratchRegister = scratch.getRegister();
            asm.loadAddress(scratchRegister, AArch64Address.createUnscaledImmediateAddress(methodIdArg.getRegister(), offset), 8);
            asm.jmp(scratchRegister);
        }
        result.recordMark(asm.position(), SubstrateAArch64Backend.MARK_PROLOGUE_DECD_RSP);
        result.recordMark(asm.position(), SubstrateAArch64Backend.MARK_PROLOGUE_END);
        byte[] instructions = asm.close(true);
        result.setTargetCode(instructions, instructions.length);
        result.setTotalFrameSize(getTarget().wordSize); // not really, but 0 not allowed
        return result;
    }

    @Override
    protected CompiledCode createCompiledCode(ResolvedJavaMethod method, CompilationRequest compilationRequest, CompilationResult compilationResult, boolean isDefault, OptionValues options) {
        return new SubstrateCompiledCode(compilationResult);
    }

    @Override
    public void emitCode(CompilationResultBuilder crb, LIR lir, ResolvedJavaMethod installedCodeOwner) {
        crb.buildLabelOffsets(lir);
        crb.emit(lir);
    }

    @Override
    public SuitesProvider getSuites() {
        throw unimplemented();
    }

    /**
     * Returns the amount of scratch space which must be reserved for return value registers in
     * {@link DeoptimizedFrame}.
     */
    public static int getDeoptScratchSpace() {
        // Space for two 64-bit registers: rax and xmm0
        return 2 * 8;
    }

    @Override
    public LIRGenerationResult newLIRGenerationResult(CompilationIdentifier compilationId, LIR lir, RegisterConfig registerConfig, StructuredGraph graph, Object stub) {
        SharedMethod method = (SharedMethod) graph.method();
        CallingConvention callingConvention = CodeUtil.getCallingConvention(getCodeCache(), method.isEntryPoint() ? SubstrateCallingConventionType.NativeCallee
                        : SubstrateCallingConventionType.JavaCallee, method, this);
        return new SubstrateLIRGenerationResult(compilationId, lir, newFrameMapBuilder(registerConfig), callingConvention, method);
    }

    @Override
    public Phase newAddressLoweringPhase(CodeCacheProvider codeCache) {
        return new AddressLoweringByUsePhase(new SubstrateAArch64AddressLowering());
    }
}
