// #include "BitWriter_3_2/ReaderWriter_3_2.h"
#include "CodeGen_RS_Dev.h"
#include "CodeGen_Internal.h"
#include "IROperator.h"
#include "IRPrinter.h"
#include "Debug.h"
#include "Target.h"
#include "LLVM_Headers.h"
#include "LLVM_Runtime_Linker.h"

namespace Halide {
namespace Internal {

using std::vector;
using std::string;

using namespace llvm;

// Sniff the contents of a kernel to extracts the bounds of all the
// thread indices (so we know how many threads to launch), and the
// amount of shared memory to allocate.
class ExtractBoundsNames : public IRVisitor {
public:
    string names[4];

    ExtractBoundsNames() {
        for (int i = 0; i < 4; i++) {
            names[i] = "";
        }
    }

private:
    using IRVisitor::visit;

    void visit(const For *op) {
        if (CodeGen_GPU_Dev::is_gpu_var(op->name)) {
            internal_assert(is_zero(op->min));
        }

        if (ends_with(op->name, ".__block_id_x")) {
            names[0] = op->name;
        } else if (ends_with(op->name, ".__block_id_y")) {
            names[1] = op->name;
        } else if (ends_with(op->name, ".__block_id_z")) {
            names[2] = op->name;
        } else if (ends_with(op->name, ".__block_id_w")) {
            names[3] = op->name;
        }
        op->body.accept(this);
    }
};


CodeGen_RS_Dev::CodeGen_RS_Dev(Target host) : CodeGen_LLVM(host) {
    debug(2) << "Created CodeGen_RS_Dev for target " << host.to_string().c_str()
             << "\n";
#if !(WITH_RS)
    user_error << "rs not enabled for this build of Halide.\n";
#endif
    context = new llvm::LLVMContext();
}

CodeGen_RS_Dev::~CodeGen_RS_Dev() {
    delete context;
}

void CodeGen_RS_Dev::add_kernel(Stmt stmt, const std::string &name,
                                const std::vector<GPU_Argument> &args) {
    internal_assert(module != NULL);

    // Use [name] as the function name.
    debug(2) << "In CodeGen_RS_Dev::add_kernel name=" << name << "\n";

    ExtractBoundsNames bounds_names;
    stmt.accept(&bounds_names);

    // Type Definitions
    StructType *StructTy_struct_rs_allocation =
        module->getTypeByName("struct.rs_allocation");

    // Put the arguments in the symbol table
    vector<std::tuple<string, Value *> > globals_sym_names;

    // Constant Definitions
    ConstantAggregateZero *const_empty_allocation_struct =
        ConstantAggregateZero::get(StructTy_struct_rs_allocation);
    ConstantInt *const_0 =
        ConstantInt::get(*context, APInt(32, StringRef("0"), 10));

    auto rs_export_var = module->getOrInsertNamedMetadata("#rs_export_var");
    auto rs_objects_slots =
        module->getOrInsertNamedMetadata("#rs_object_slots");

    // Now deduce the types of the arguments to our function
    vector<llvm::Type *> arg_types_1(args.size());
    llvm::Type *argument_type = NULL;
    for (size_t i = 0; i < args.size(); i++) {
        string arg_name = args[i].name;
        debug(1) << "CodeGen_RS_Dev arg[" << i << "].name=" << arg_name << "\n";
        if (!args[i].is_buffer) {
            GlobalVariable *gvar =
                new GlobalVariable(*module, llvm::Type::getInt32Ty(*context),
                                   false,  // isConstant
                                   GlobalValue::CommonLinkage,
                                   0,  // has initializer, specified below
                                   arg_name);
            gvar->setAlignment(4);
            gvar->setInitializer(const_0);
            globals_sym_names.push_back(std::make_tuple(arg_name, gvar));

            rs_export_var->addOperand(MDNode::get(
                *context,
                vec<LLVMMDNodeArgumentType>(MDString::get(*context, arg_name),
                                            MDString::get(*context, "6"))));

        } else {
            argument_type =
                VectorType::get(llvm_type_of(UInt(8)), args[i].type.width);

            GlobalVariable *gvar =
                new GlobalVariable(*module, StructTy_struct_rs_allocation,
                                   false,  // isConstant
                                   GlobalValue::CommonLinkage,
                                   0,  // has initializer, specified below
                                   arg_name);
            gvar->setAlignment(4);
            globals_sym_names.push_back(std::make_tuple(arg_name, gvar));
            gvar->setInitializer(const_empty_allocation_struct);

            rs_export_var->addOperand(MDNode::get(
                *context,
                vec<LLVMMDNodeArgumentType>(MDString::get(*context, arg_name),
                                            MDString::get(*context, "20"))));
            rs_objects_slots->addOperand(
                MDNode::get(*context, vec<LLVMMDNodeArgumentType>(MDString::get(
                                          *context, std::to_string(i)))));

            // } else {
            //     arg_types_1[i] = llvm_type_of(args[i].type);
        }
        Halide::Type type = args[i].type;
        debug(2) << "args[" << i << "] = {"
                 << "name=" << args[i].name
                 << " is_buffer=" << args[i].is_buffer
                 << " dimensions=" << args[i].dimensions << " type.code="
                 << (type.code == Halide::Type::TypeCode::Int
                         ? "Int"
                         : type.code == Halide::Type::TypeCode::UInt
                               ? "UInt"
                               : type.code == Halide::Type::TypeCode::Float
                                     ? "Float"
                                     : type.code ==
                                               Halide::Type::TypeCode::Handle
                                           ? "Handle"
                                           : "???") << ";bits=" << type.bits
                 << ";width=" << type.width << " llvm_type=";
        // arg_types_1[i]->dump();
        debug(2) << "\n";
    }

    // Make our function
    // Arguments for kernel are defined per Renderscript conventions
    // (in_type in, i32 x, i32 y)

    vector<llvm::Type *> arg_types;
    arg_types.push_back(argument_type);  // in
    // Add kernel function's parameters to the scope.
    for (int i = 0; i < 4; i++) {
        debug(2) << "  adding argument type at " << i << ": "
                 << bounds_names.names[i]
                 << "\n";
        if (!bounds_names.names[i].empty()) {
            arg_types.push_back(i32);
        }
    }

    // FunctionType *func_t = FunctionType::get(argument_type, arg_types,
    // false);
    FunctionType *func_t = FunctionType::get(void_t, arg_types, false);
    function = llvm::Function::Create(func_t, llvm::Function::ExternalLinkage,
                                      name, module);

    vector<string> arg_sym_names;

    // Add kernel function's parameters to the scope.
    auto input_arg = function->arg_begin();
    input_arg++;  // skip in buffer
    for (int i = 0; i < 4; i++, input_arg++) {
        string bounds_name = bounds_names.names[i];
        if (!bounds_name.empty()) {
            input_arg->setName(bounds_name);
            sym_push(bounds_name, input_arg);
            debug(2) << "  adding kernel function parameter " << bounds_name
                     << " with type ";
            input_arg->getType()->dump();
            arg_sym_names.push_back(bounds_name);
        }
    }

    // Mark the buffer args as no alias
    // for (size_t i = 0; i < args.size(); i++) {
    //     if (args[i].is_buffer) {
    //         function->setDoesNotAlias(i + 1);
    //     }
    // }

    // Make the initial basic block
    entry_block = BasicBlock::Create(*context, "entry", function);
    builder->SetInsertPoint(entry_block);

    // We won't end the entry block yet, because we'll want to add
    // some allocas to it later if there are local allocations. Start
    // a new block to put all the code.
    BasicBlock *body_block = BasicBlock::Create(*context, "body", function);
    builder->SetInsertPoint(body_block);

    // Global symbols are pointers to the values, so
    // we need to dereference and load actual values.

    for (auto name_and_value : globals_sym_names) {
        string name = std::get<0>(name_and_value);
        debug(2) << "Pushing global symbol " << name << " into sym table\n";

        Value *value = std::get<1>(name_and_value);

        if (name.compare("input") == 0 || name.compare("result") == 0) {
            //   %9 = load [1 x i32]* bitcast (%struct.rs_allocation* @alloc_in
            //   to [1 x i32]*), align 4
            value = builder->CreateBitCast(
                value,
                PointerType::get(
                    ArrayType::get(IntegerType::get(*context, 32), 1), 0));
        }

        value = builder->CreateAlignedLoad(value, 4 /*alignment*/);
        sym_push(name, value);
        arg_sym_names.push_back(name);
    }

    debug(1) << "Generating llvm bitcode for kernel...\n";
    // Ok, we have a module, function, context, and a builder
    // pointing at a brand new basic block. We're good to go.
    stmt.accept(this);

    builder->CreateRetVoid();
    // Make the entry block point to the body block
    builder->SetInsertPoint(entry_block);
    builder->CreateBr(body_block);

    auto meta_llvm_module_flags =
        module->getOrInsertNamedMetadata("llvm.module.flags");
    meta_llvm_module_flags->addOperand(MDNode::get(
        *context, vec<LLVMMDNodeArgumentType>(
                      value_as_metadata_type(ConstantInt::get(i32, 1)),
                      MDString::get(*context, "wchar_size"),
                      value_as_metadata_type(ConstantInt::get(i32, 4)))));
    meta_llvm_module_flags->addOperand(MDNode::get(
        *context, vec<LLVMMDNodeArgumentType>(
                      value_as_metadata_type(ConstantInt::get(i32, 1)),
                      MDString::get(*context, "min_enum_size"),
                      value_as_metadata_type(ConstantInt::get(i32, 4)))));

    module->getOrInsertNamedMetadata("llvm.ident")
        ->addOperand(
            MDNode::get(*context, vec<LLVMMDNodeArgumentType>(MDString::get(
                                      *context, "clang version 3.6 "))));

    auto meta_pragma = module->getOrInsertNamedMetadata("#pragma");
    meta_pragma->addOperand(MDNode::get(
        *context,
        vec<LLVMMDNodeArgumentType>(MDString::get(*context, "version"),
                                    MDString::get(*context, "1"))));
    meta_pragma->addOperand(MDNode::get(
        *context,
        vec<LLVMMDNodeArgumentType>(
            MDString::get(*context, "java_package_name"),
            MDString::get(*context, "com.example.android.basicrenderscript"))));
    meta_pragma->addOperand(MDNode::get(
        *context,
        vec<LLVMMDNodeArgumentType>(MDString::get(*context, "rs_fp_relaxed"),
                                    MDString::get(*context, ""))));

    auto rs_export_foreach_name =
        module->getOrInsertNamedMetadata("#rs_export_foreach_name");
    rs_export_foreach_name->addOperand(MDNode::get(
        *context,
        vec<LLVMMDNodeArgumentType>(MDString::get(*context, "root"))));
    rs_export_foreach_name->addOperand(MDNode::get(
        *context, vec<LLVMMDNodeArgumentType>(MDString::get(
                      *context, "kernel_result_s0_y___block_id_y"))));

    auto rs_export_foreach =
        module->getOrInsertNamedMetadata("#rs_export_foreach");
    rs_export_foreach->addOperand(MDNode::get(
        *context, vec<LLVMMDNodeArgumentType>(MDString::get(*context, "0"))));
    rs_export_foreach->addOperand(MDNode::get(
        *context, vec<LLVMMDNodeArgumentType>(MDString::get(*context, "57"))));

    // Now verify the function is ok
    verifyFunction(*function);

    // Finally, verify the module is ok
    verifyModule(*module);

    debug(2) << "Done generating llvm bitcode for RS\n";

    // Clear the symbol table
    for (size_t i = 0; i < arg_sym_names.size(); i++) {
        sym_pop(arg_sym_names[i]);
    }
}

void CodeGen_RS_Dev::init_module() {
    debug(2) << "CodeGen_RS_Dev::init_module\n";
    init_context();
    debug(2) << "CodeGen_RS_Dev::module is " << module << "\n";

//    module = new llvm::Module("rs_module", *context);
#ifdef WITH_RS
    delete module;
    module = get_initial_module_for_rs_device(target, context);
#endif
}

llvm::Triple CodeGen_LLVM::get_target_triple() const {
    return Triple(Triple::normalize("armv7-none-linux-gnueabi"));
}

llvm::DataLayout CodeGen_RS_Dev::get_data_layout() const {
    return llvm::DataLayout("e-m:e-p:32:32-i64:64-v128:64:128-a:0:32-n32-S64");
}

//
// Loops become kernels. There should be no explicit loops in
// generated RenderScript code.
//
void CodeGen_RS_Dev::visit(const For *loop) {
    debug(2) << "RS: Visiting for loop, loop->name is " << loop->name << "\n";
    if (is_gpu_var(loop->name)) {
        //        Expr simt_idx = Call::make(Int(32), loop->name,
        //        std::vector<Expr>(), Call::Extern);
        if (is_gpu_thread_var(loop->name)) {
            debug(2) << "RS: ignore loop over gpu_thread_var" << loop->name
                     << "\n";
            // ignore thread-parallelization since it's taken care of by
            // Renderscript.
            loop->body.accept(this);
        } else {
            debug(2) << "RS: do nothing with loop over non-gpu_thread_var"
                     << loop->name << "\n";
            loop->body.accept(this);
        }
    } else {
        user_assert(loop->for_type != ForType::Parallel)
            << "Cannot use loops inside RS kernel\n";
        CodeGen_LLVM::visit(loop);
    }
    debug(2) << "RS: Done with for loop\n";
}

void CodeGen_RS_Dev::visit(const Allocate *alloc) {
    debug(2) << "RS: Allocate " << alloc->name << " on device\n";
    codegen(alloc->body);
}

void CodeGen_RS_Dev::visit(const Free *f) {
    // TODO(aam): Implement this.
    debug(2) << "RS: Free on device\n";
}

void CodeGen_RS_Dev::visit(const Call *op) {
    if (op->call_type == Call::Intrinsic) {

    // coordinates_store(
    //     x4("result"),
    //     x4(result.buffer),
    //     x4((result.s0.x.__block_id_x + result.min.0)),
    //     x4((result.s0.y.__block_id_y + result.min.1)),
    //     ramp(0, 1, 4),
    //     (((
    //         (
    //             (
    //                 (
    //                     coordinates_load(
    //                         x4("input"),
    //                         x4("input"),
    //                         x4(input.buffer),
    //                         x4((t22.s - input.min.0)),
    //                         x4((t23.s - input.min.1)),
    //                         ramp(0, 1, 4)
    //                     ) 
    //                     + coordinates_load(
    //                         x4("input"),
    //                         x4("input"),
    //                         x4(input.buffer),
    //                         x4((t24.s - input.min.0)),
    //                         x4((t23.s - input.min.1)),
    //                         ramp(0, 1, 4))
    //                 )
    //                 + coordinates_load(
    //                     x4("input"),
    //                     x4("input"),
    //                     x4(input.buffer),
    //                     x4((t25.s - input.min.0)),
    //                     x4((t23.s - input.min.1)),
    //                     ramp(0, 1, 4)
    //                 )
    //             )/x4(uint8(3))
    //         ) 
    //         + 
    //         (((coordinates_load(x4("input"), x4("input"), x4(input.buffer), x4((t22.s - input.min.0)), x4((t26.s - input.min.1)), ramp(0, 1, 4)) + coordinates_load(x4("input"), x4("input"), x4(input.buffer), x4((t24.s - input.min.0)), x4((t26.s - input.min.1)), ramp(0, 1, 4))) + coordinates_load(x4("input"), x4("input"), x4(input.buffer), x4((t25.s - input.min.0)), x4((t26.s - input.min.1)), ramp(0, 1, 4)))/x4(uint8(3)))) + (((coordinates_load(x4("input"), x4("input"), x4(input.buffer), x4((t22.s - input.min.0)), x4((t27.s - input.min.1)), ramp(0, 1, 4)) + coordinates_load(x4("input"), x4("input"), x4(input.buffer), x4((t24.s - input.min.0)), x4((t27.s - input.min.1)), ramp(0, 1, 4))) + coordinates_load(x4("input"), x4("input"), x4(input.buffer), x4((t25.s - input.min.0)), x4((t27.s - input.min.1)), ramp(0, 1, 4)))/x4(uint8(3))))/x4(uint8(3))))


        if (op->name == Call::coordinates_load) {
            // This intrinsic takes four arguments
            // coordinates_load(<tex name>, <name>, <buffer>, <x>, <y>, <c>)
            internal_assert(op->args.size() == 6);

            // The argument to the call is either a StringImm or a broadcasted
            // StringImm if this is part of a vectorized expression
            internal_assert(
                op->args[0].as<StringImm>() ||
                (op->args[0].as<Broadcast>() &&
                 op->args[0].as<Broadcast>()->value.as<StringImm>()));

            const StringImm *string_imm = op->args[0].as<StringImm>();
            if (!string_imm) {
                string_imm = op->args[0].as<Broadcast>()->value.as<StringImm>();
            }

            // Determine the halide buffer associated with this load
            string buffername = string_imm->value;

            internal_assert(
                (op->type.code == Type::UInt || op->type.code == Type::Float) &&
                (op->type.width >= 1 && op->type.width <= 4));

            Expr x = op->args[3];
            if (const Broadcast *b = x.as<Broadcast>()) {
                x = b->value;
            }
            debug(2) << "x=" << x << "\n";
            Expr y = op->args[4];
            if (const Broadcast *b = y.as<Broadcast>()) {
                y = b->value;
            }

            // internal_assert(op->args[2].type().width == 1)
            //     << "coordinates_load argument 2 is not scalar";
            // internal_assert(op->args[3].type().width == 1)
            //     << "coordinates_load argument 3 is not scalar";

            debug(2) << "rsGetElementAt_uchar4(input, " << x << ", " << y
                     << ")\n";

            Expr c = op->args[5];
            // TOOD: ramp over c

            llvm::Function *rsGetElementAt_uchar4 = module->getFunction(
                "_Z21rsGetElementAt_uchar413rs_allocationjj");
            internal_assert(rsGetElementAt_uchar4)
                << "Cant' find _Z21rsGetElementAt_uchar413rs_allocationjj "
                   "function";
            llvm::Function *rsGetElementAt_uchar = module->getFunction(
                "_Z20rsGetElementAt_uchar13rs_allocationjj");
            internal_assert(rsGetElementAt_uchar)
                << "Cant' find _Z20rsGetElementAt_uchar13rs_allocationjj "
                   "function";

            vector<Value *> args =
                vec(sym_get("input"), codegen(x), codegen(y));

            debug(2) << "Generating " << op->type.width
                     << "byte-wide GetElement call with " << args.size()
                     << " args:\n";
            int i = 1;
            for (Value *arg : args) {
                debug(2) << " #" << i++ << ":";
                arg->getType()->dump();
                arg->dump();
            }
            value =
                builder->CreateCall(op->type.width == 1 ? rsGetElementAt_uchar
                                                        : rsGetElementAt_uchar4,
                                    args);
        } else if (op->name == Call::coordinates_store) {

            llvm::Function *rsSetElementAt_uchar4 = module->getFunction(
                "_Z21rsSetElementAt_uchar413rs_allocationDv4_hjj");
            internal_assert(rsSetElementAt_uchar4)
                << "Cant' find _Z21rsSetElementAt_uchar413rs_allocationDv4_hjj "
                   "function";
            llvm::Function *rsSetElementAt_uchar = module->getFunction(
                "_Z20rsSetElementAt_uchar13rs_allocationhjj");
            internal_assert(rsSetElementAt_uchar)
                << "Cant' find _Z20rsSetElementAt_uchar13rs_allocationhjj "
                   "function";

            debug(2) << "SetElement coordinates x:" << op->args[2]
                     << " y:" << op->args[3] << "\n";
            Expr x = op->args[2];
            if (const Broadcast *b = x.as<Broadcast>()) {
                x = b->value;
            }
            debug(2) << "x=" << x << "\n";
            Expr y = op->args[3];
            if (const Broadcast *b = y.as<Broadcast>()) {
                y = b->value;
            }
            debug(2) << "y=" << y << "\n";
            Expr c = op->args[4];
            // TOOD: ramp over c
            Expr set_value = op->args[5];

            vector<Value *> args = vec(sym_get("result"), codegen(set_value),
                                       codegen(x), codegen(y));

            debug(2) << "Generating " << op->type.width
                     << "byte-wide SetElement call with " << args.size()
                     << " args:\n";
            int i = 1;
            for (Value *arg : args) {
                debug(2) << " #" << i++ << ":";
                arg->getType()->dump();
                arg->dump();
            }

            value =
                builder->CreateCall(op->type.width == 1 ? rsSetElementAt_uchar
                                                        : rsSetElementAt_uchar4,
                                    args);
            return;
        }
    }
}

string CodeGen_RS_Dev::march() const {
    return "armv7";
}

string CodeGen_RS_Dev::mcpu() const { return "none"; }

string CodeGen_RS_Dev::mattrs() const { return "linux-gnueabi"; }

bool CodeGen_RS_Dev::use_soft_float_abi() const { return false; }

llvm::Triple CodeGen_RS_Dev::get_target_triple() const {
    return Triple(Triple::normalize("armv7-none-linux-gnueabi"));
}

struct AndroidBitcodeWrapper {
    uint32_t Magic;
    uint32_t Version;
    uint32_t BitcodeOffset;
    uint32_t BitcodeSize;
    uint32_t HeaderVersion;
    uint32_t TargetAPI;
    uint32_t PNaClVersion;
    uint16_t CompilerVersionTag;
    uint16_t CompilerVersionLen;
    uint32_t CompilerVersion;
    uint16_t OptimizationLevelTag;
    uint16_t OptimizationLevelLen;
    uint32_t OptimizationLevel;
};

class BCHeaderField {
public:
    typedef enum {
        kInvalid = 0,
        kBitcodeHash = 1,
        kAndroidCompilerVersion = 0x4001,
        kAndroidOptimizationLevel = 0x4002
    } Tag;
};

/**
 * Helper function to emit just the bitcode wrapper returning the number of
 * bytes that were written.
 *
 * \param wrapper - where to write header information into.
 * \param bitcodeSize - size of bitcode in bytes.
 * \param targetAPI - target API version for this bitcode.
 * \param compilerVersion - compiler version that generated this bitcode.
 * \param optimizationLevel - compiler optimization level for this bitcode.
 *
 * \return number of wrapper bytes written into the \p buffer.
 */
static inline size_t writeAndroidBitcodeWrapper(AndroidBitcodeWrapper *wrapper,
                                                size_t bitcodeSize,
                                                uint32_t targetAPI,
                                                uint32_t compilerVersion,
                                                uint32_t optimizationLevel) {
    if (!wrapper) {
        return 0;
    }

    wrapper->Magic = 0x0B17C0DE;
    wrapper->Version = 0;
    wrapper->BitcodeOffset = sizeof(*wrapper);  // 0x2c
    wrapper->BitcodeSize = bitcodeSize;
    wrapper->HeaderVersion = 0;
    wrapper->TargetAPI = targetAPI;  // 0x00000015
    wrapper->PNaClVersion = 0;
    wrapper->CompilerVersionTag =
        BCHeaderField::kAndroidCompilerVersion;  // 0x40001
    wrapper->CompilerVersionLen = 4;
    wrapper->CompilerVersion = compilerVersion;  // 0x000076d
    wrapper->OptimizationLevelTag = BCHeaderField::kAndroidOptimizationLevel;
    wrapper->OptimizationLevelLen = 4;
    wrapper->OptimizationLevel = optimizationLevel;  // 3

    return sizeof(*wrapper);
}

vector<char> CodeGen_RS_Dev::compile_to_src() {
    // Generic llvm optimizations on the module.
    optimize_module();

    debug(1) << "CodeGen_RS_Dev::compile_to_src resultant module:\n";
    module->dump();

    std::string str;
    llvm::raw_string_ostream OS(str);
    // llvm_3_2::WriteBitcodeToFile(module, OS);
    OS.flush();

    /**
     * The minimum version which does not require translation (i.e. is already
     * compatible with LLVM's default bitcode reader).
     */
    const unsigned int kMinimumUntranslatedVersion = 21;

    // BitcodeWrapper BCWrapper(str.c_str(), str.size());

    AndroidBitcodeWrapper wrapper;
    size_t actualWrapperLen = writeAndroidBitcodeWrapper(
        &wrapper, str.size(), kMinimumUntranslatedVersion,
        0x000076d /*BCWrapper.getCompilerVersion()*/,
        3 /*BCWrapper.getOptimizationLevel()*/);
    if (!actualWrapperLen) {
        debug(1) << "Couldn't produce bitcode wrapper!\n";
        return vector<char>();
    }

    size_t mTranslatedBitcodeSize = actualWrapperLen + str.size();
    char *c = new char[mTranslatedBitcodeSize];
    memcpy(c, &wrapper, actualWrapperLen);
    memcpy(c + actualWrapperLen, str.c_str(), str.size());

    // string str = outs.str();
    debug(1) << "RS kernel:\n" << str.c_str() << "\n";
    vector<char> buffer(c, c + mTranslatedBitcodeSize);
    return buffer;
}

int CodeGen_RS_Dev::native_vector_bits() const {
    // TODO(aam): Copied from CodeGen_PTX_Dev. Does this make sense?
    return 64;
}

string CodeGen_RS_Dev::get_current_kernel_name() { return function->getName(); }

void CodeGen_RS_Dev::dump() { module->dump(); }

std::string CodeGen_RS_Dev::print_gpu_name(const std::string &name) {
    return name;
}
}
}