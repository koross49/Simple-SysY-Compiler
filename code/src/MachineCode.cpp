#include "MachineCode.h"
#include "Type.h"
extern FILE* yyout;

int MachineBlock::spilt_label = 0;

MachineOperand::MachineOperand(int tp, int val, bool is_float, float fval)
{
    this->type = tp;
    if(tp == MachineOperand::IMM)
        this->val = val;
    else 
        this->reg_no = val;
    this->is_float = is_float;
    if(is_float) {
        this->fval = fval;
    }
}

MachineOperand::MachineOperand(std::string label, bool is_func, bool flt)
{
    this->type = MachineOperand::LABEL;
    this->label = label;
    is_funct = is_func;
    is_float = flt;
}

bool MachineOperand::operator==(const MachineOperand&a) const
{
    if (this->type != a.type)
        return false;
    if (this->type == IMM)
        return this->val == a.val;
    return this->reg_no == a.reg_no;
}

bool MachineOperand::operator<(const MachineOperand&a) const
{
    if(this->type == a.type)
    {
        if(this->type == IMM)
            return this->val < a.val;
        return this->reg_no < a.reg_no;
    }
    return this->type < a.type;

    if (this->type != a.type)
        return false;
    if (this->type == IMM)
        return this->val == a.val;
    return this->reg_no == a.reg_no;
}

void MachineOperand::PrintReg()
{
    switch (reg_no)
    {
    case 11:
        fprintf(yyout, "fp");
        break;
    case 13:
        fprintf(yyout, "sp");
        break;
    case 14:
        fprintf(yyout, "lr");
        break;
    case 15:
        fprintf(yyout, "pc");
        break;
    default:
        if(is_float) {
            int freg_no = reg_no - 16;
            if(freg_no <= 31) {
                fprintf(yyout, "s%d", freg_no);
            }
            else {
                fprintf(yyout, "FPSCR");
            }
        }
        else {
            fprintf(yyout, "r%d", reg_no);
        }
        break;
    }
}

void MachineOperand::output() 
{
    /* HINT：print operand
    * Example:
    * immediate num 1 -> print #1;
    * register 1 -> print r1;
    * lable addr_a -> print addr_a; */
    switch (this->type)
    {
    case IMM:
        if(is_float) {
            uint32_t temp = reinterpret_cast<uint32_t&>(this->fval);
            fprintf(yyout, "#%u", temp);
        }
        else {
            fprintf(yyout, "#%d", this->val);
        }
        break;
    case VREG:
        fprintf(yyout, "v%d", this->reg_no);
        break;
    case REG:
        PrintReg();
        break;
    case LABEL:
        if (this->label.substr(0, 2) == ".L" || is_funct)
            fprintf(yyout, "%s", this->label.c_str());
        else
            fprintf(yyout, "addr_%s_%d", this->label.c_str(), parent->getParent()->getParent()->getParent()->getN());
    default:
        break;
    }
}

void MachineInstruction::PrintCond()
{
    switch (cond)
    {
    case EQ:
        fprintf(yyout, "eq");
        break;
    case NE:
        fprintf(yyout, "ne");
        break;
    case LT:
        fprintf(yyout, "lt");
        break;
    case LE:
        fprintf(yyout, "le");
        break;
    case GT:
        fprintf(yyout, "gt");
        break;
    case GE:
        fprintf(yyout, "ge");
        break;
    default:
        break;
    }
}

BinaryMInstruction::BinaryMInstruction(
    MachineBlock* p, int op, 
    MachineOperand* dst, MachineOperand* src1, MachineOperand* src2, 
    int cond)
{
    this->parent = p;
    this->type = MachineInstruction::BINARY;
    this->op = op;
    this->cond = cond;
    this->def_list.push_back(dst);
    this->use_list.push_back(src1);
    this->use_list.push_back(src2);
    dst->setParent(this);
    src1->setParent(this);
    src2->setParent(this);
}

void BinaryMInstruction::output() 
{
    switch (this->op)
    {
    case BinaryMInstruction::ADD:
        fprintf(yyout, "\tadd ");
        break;
    case BinaryMInstruction::SUB:
        fprintf(yyout, "\tsub ");
        break;
    case BinaryMInstruction::MUL:
        fprintf(yyout, "\tmul ");
        break;
    case BinaryMInstruction::DIV:
        fprintf(yyout, "\tsdiv ");
        break;
    case BinaryMInstruction::AND:
        fprintf(yyout, "\tand ");
        break;
    case BinaryMInstruction::OR:
        fprintf(yyout, "\tor ");
        break;
    case BinaryMInstruction::VADD:
        fprintf(yyout, "\tvadd.f32 ");//单精度浮点数
        break;
    case BinaryMInstruction::VSUB:
        fprintf(yyout, "\tvsub.f32 ");
        break;
    case BinaryMInstruction::VMUL:
        fprintf(yyout, "\tvmul.f32 ");
        break;
    case BinaryMInstruction::VDIV:
        fprintf(yyout, "\tvdiv.f32 ");
        break;
    default:
        break;
    }
    this->PrintCond();
    this->def_list[0]->output();
    fprintf(yyout, ", ");
    this->use_list[0]->output();
    fprintf(yyout, ", ");
    this->use_list[1]->output();
    fprintf(yyout, "\n");
}

LoadMInstruction::LoadMInstruction(MachineBlock* p,
    MachineOperand* dst, MachineOperand* src1, MachineOperand* src2,
    int op, int cond)
{
    this->parent = p;
    this->type = MachineInstruction::LOAD;
    this->op = op;
    this->cond = cond;
    this->def_list.push_back(dst);
    this->use_list.push_back(src1);
    if (src2)
        this->use_list.push_back(src2);
    dst->setParent(this);
    src1->setParent(this);
    if (src2)
        src2->setParent(this);
}

void LoadMInstruction::output()
{
    switch(this->op) {
        case LoadMInstruction::LDR:
            fprintf(yyout, "\tldr ");
            break;
        case LoadMInstruction::VLDR:
            fprintf(yyout, "\tvldr.32 ");
            break;
        default:
            break;
    }
    this->def_list[0]->output();
    fprintf(yyout, ", ");

    // Load immediate num, eg: ldr r1, =8
    if(this->use_list[0]->isImm())
    {
        if(this->use_list[0]->isFloat()) {
            float val = this->use_list[0]->getFVal();
            uint32_t val_int =  reinterpret_cast<uint32_t&>(val);
            fprintf(yyout, "=%u\n", val_int);
        }
        else {
            fprintf(yyout, "=%d\n", this->use_list[0]->getVal());
        }
        return;
    }

    // Load address
    if(this->use_list[0]->isReg()||this->use_list[0]->isVReg())
        fprintf(yyout, "[");

    this->use_list[0]->output();
    if( this->use_list.size() > 1 )
    {
        fprintf(yyout, ", ");
        this->use_list[1]->output();
    }

    if(this->use_list[0]->isReg()||this->use_list[0]->isVReg())
        fprintf(yyout, "]");
    fprintf(yyout, "\n");
}

StoreMInstruction::StoreMInstruction(MachineBlock* p,
    MachineOperand* src1, MachineOperand* src2, MachineOperand* src3, 
    int op, int cond)
{
    this->parent = p;
    this->type = MachineInstruction::STORE;
    this->op = op;
    this->cond = cond;
    this->use_list.push_back(src1);
    this->use_list.push_back(src2);
    if (src3)
        this->use_list.push_back(src3);
    src1->setParent(this);
    src2->setParent(this);
    if (src3)
        src3->setParent(this);
}

void StoreMInstruction::output()
{
    switch(this->op)
    {
        case StoreMInstruction::STR: // str r1, [r2]
            fprintf(yyout, "\tstr ");
            break;
        case StoreMInstruction::VSTR:
            fprintf(yyout, "\tvstr.32 "); // vstr.32 s0, [r3]
            break;
    }
    this->use_list[0]->output();
    fprintf(yyout, ", ");

    
    if(this->use_list[1]->isReg()||this->use_list[1]->isVReg())
        fprintf(yyout, "[");

    this->use_list[1]->output();
    if( this->use_list.size() > 2 )
    {
        fprintf(yyout, ", ");
        this->use_list[2]->output();
    }

    if(this->use_list[1]->isReg()||this->use_list[1]->isVReg())
        fprintf(yyout, "]");
    fprintf(yyout, "\n");
}

MovMInstruction::MovMInstruction(MachineBlock* p, int op, 
    MachineOperand* dst, MachineOperand* src,
    int cond)
{
    this->parent = p;
    this->type = MachineInstruction::MOV;
    this->op = op;
    this->cond = cond;
    this->def_list.push_back(dst);
    this->use_list.push_back(src);
    dst->setParent(this);
    src->setParent(this);
}

// ; 当 op 为 MOV 时
//     mov r1, r2
// ; 当 op 为 MOVT 时
//     movt r3, #123
// ; 当 op 为 VMOV 时
//     vmov s0, s1
// ; 当 op 为 VMOVF32 时
//     vmov.f32 s2, #3.14

void MovMInstruction::output() 
{
    switch(this->op) {
        case MovMInstruction::MOV:
            fprintf(yyout, "\tmov");
            break;
        case MovMInstruction::MOVT:
            fprintf(yyout, "\tmovt");
            break;
        case MovMInstruction::VMOV:
            fprintf(yyout, "\tvmov");
            break;
        case MovMInstruction::VMOVF32:
            fprintf(yyout, "\tvmov.f32");
            break;
        default:
            break;
    }
    PrintCond();
    fprintf(yyout, " ");
    this->def_list[0]->output();
    fprintf(yyout, ", ");
    this->use_list[0]->output();
    fprintf(yyout, "\n");
}

BranchMInstruction::BranchMInstruction(MachineBlock* p, int op, 
    MachineOperand* dst, 
    int cond)
{
    this->parent = p;
    this->type = MachineInstruction::BRANCH;
    this->op = op;
    this->cond = cond;
    this->def_list.push_back(dst);
    dst->setParent(this);
}

void BranchMInstruction::output()
{
    switch(op){
    case B:
        fprintf(yyout, "\tb");
        PrintCond();
        fprintf(yyout, " ");
        break;
    case BL:
        fprintf(yyout, "\tbl ");
        break;
    case BX:
        fprintf(yyout, "\tbx ");
        break;
    }
    this->def_list[0]->output();
    fprintf(yyout, "\n");
}

CmpMInstruction::CmpMInstruction(MachineBlock* p, 
    MachineOperand* src1, MachineOperand* src2, 
    int cond, int optype)
{
    this->parent = p;
    this->type = optype;
    this->cond = cond;
    this->use_list.push_back(src1);
    this->use_list.push_back(src2);
    src1->setParent(this);
    src2->setParent(this);
}

void CmpMInstruction::output()
{
    switch(this->type) {
        case CMP:
            fprintf(yyout, "\tcmp ");
            break;
        case VCMP:
            fprintf(yyout, "\tvcmp.f32 ");
            break;
        default:
            break;
    }
    this->use_list[0]->output();
    fprintf(yyout, ", ");
    this->use_list[1]->output();
    fprintf(yyout, "\n");
}

StackMInstruction::StackMInstruction(MachineBlock* p, int op, 
    std::vector<MachineOperand*> src,
    int cond)
{
    this->parent = p;
    this->type = MachineInstruction::STACK;
    this->op = op;
    this->cond = cond;
    this->use_list = src;
    for(auto reg : use_list){
        reg->setParent(this);
    }
}

void StackMInstruction::output()
{
    switch(op){// push{r1, r2, r3}
    case PUSH:
        fprintf(yyout, "\tpush {");
        break;
    case POP:
        fprintf(yyout, "\tpop {");
        break;
    case VPUSH:
        fprintf(yyout, "\tvpush {");
        break;
    case VPOP:
        fprintf(yyout, "\tvpop {");
        break;
    }
    if(use_list.size() <= 16) {
        this->use_list[0]->output();
        for (long unsigned int i = 1; i < use_list.size(); i++) {
            fprintf(yyout, ", ");
            this->use_list[i]->output();
        }
    }
    // 浮点寄存器可能会很多 每次只能push/pop16个
    else {
        this->use_list[0]->output();
        for (long unsigned int i = 1; i < 16; i++) {
            fprintf(yyout, ", ");
            this->use_list[i]->output();
        }
        fprintf(yyout, "}\n");
        if(op == VPUSH) {
            fprintf(yyout, "\tvpush ");
        }
        else if(op == VPOP){
            fprintf(yyout, "\tvpop ");
        }
        fprintf(yyout, "{");
        this->use_list[16]->output();
        for (long unsigned int i = 17; i < use_list.size(); i++) {
            fprintf(yyout, ", ");
            this->use_list[i]->output();
        }
    }
    fprintf(yyout, "}\n");
}


ZextMInstruction::ZextMInstruction(MachineBlock *p, MachineOperand *dst, MachineOperand *src, int cond) {
    this->parent = p;
    this->type = MachineInstruction::ZEXT;
    this->cond = cond;
    this->def_list.push_back(dst);
    this->use_list.push_back(src);
    dst->setParent(this);
    src->setParent(this);
}

// 零扩展 uxtb r1, r2
void ZextMInstruction::output() {
    fprintf(yyout, "\tuxtb ");
    def_list[0]->output();
    fprintf(yyout, ", ");
    use_list[0]->output();
    fprintf(yyout, "\n");
}

VcvtMInstruction::VcvtMInstruction(MachineBlock* p,
                                   int op,
                                   MachineOperand* dst,
                                   MachineOperand* src,
                                   int cond) {
    this->parent = p;
    this->type = MachineInstruction::VCVT;
    this->op = op;
    this->cond = cond;
    this->def_list.push_back(dst);
    this->use_list.push_back(src);
    dst->setParent(this);
    src->setParent(this);
//    dst->setDef(this);
}

void VcvtMInstruction::output() {
    switch (this->op) {
        case VcvtMInstruction::F2S:
            fprintf(yyout, "\tvcvt.s32.f32 ");// 单精度浮点数转为有符号整数
            break;
        case VcvtMInstruction::S2F:
            fprintf(yyout, "\tvcvt.f32.s32 ");// 有符号整数转单精度浮点
            break;
        default:
            break;
    }
    PrintCond();
    fprintf(yyout, " ");
    this->def_list[0]->output();
    fprintf(yyout, ", ");
    this->use_list[0]->output();
    fprintf(yyout, "\n");
}

VmrsMInstruction::VmrsMInstruction(MachineBlock* p) {
    this->parent = p;
    this->type = MachineInstruction::VMRS;
}

void VmrsMInstruction::output() {
    fprintf(yyout, "\tvmrs APSR_nzcv, FPSCR\n");
}

MachineFunction::MachineFunction(MachineUnit* p, SymbolEntry* sym_ptr) 
{ 
    this->parent = p; 
    this->sym_ptr = sym_ptr; 
    this->stack_size = 0;
};

std::vector<MachineOperand*> MachineFunction::getSavedRRegs()
{
    std::vector<MachineOperand*> ret;
    for(auto no : saved_regs){
        MachineOperand* reg = nullptr;
        if(no < 16) {
            reg = new MachineOperand(MachineOperand::REG, no);
            ret.push_back(reg);
        }
    }
    return ret;
};

std::vector<MachineOperand*> MachineFunction::getSavedFRegs()
{
    std::vector<MachineOperand*> ret;
    for(auto no : saved_regs){
        MachineOperand* reg = nullptr;
        if(no >= 16) {
            reg = new MachineOperand(MachineOperand::REG, no, true);
            ret.push_back(reg);
        }
    }
    return ret;
};

void MachineBlock::output()
{
    // 输出汇编代码的标签，标号使用块的编号
    fprintf(yyout, ".L%d:\n", this->no);
    // 计数器，用于限制每个块中指令的数量
    int cnt = 0;
    // 遍历块内的指令列表
    for(auto iter : inst_list) {
        // 输出当前指令的汇编代码
        iter->output();
        cnt++;
        // 当指令数量达到500时，进行代码分裂
        if(cnt % 500 == 0) {
            // 输出分裂时的跳转指令，跳转到新的标号对应的代码块
            fprintf(yyout, "\tb .B%d\n", spilt_label);
            // 输出.ltorg指令，用于处理位置依赖的指令
            fprintf(yyout, ".LTORG\n");
            // 输出全局变量
            parent->getParent()->PrintGlobal();
            // 输出新的代码块标签
            fprintf(yyout, ".B%d:\n", spilt_label++);
        }
    }
}


void MachineBlock::insertBefore(MachineInstruction* at, MachineInstruction* src)
{
    std::vector<MachineInstruction*>::iterator pos = find(inst_list.begin(), inst_list.end(), at);
    inst_list.insert(pos, src);
}

void MachineBlock::insertAfter(MachineInstruction* at, MachineInstruction* src)
{
    std::vector<MachineInstruction*>::iterator pos = find(inst_list.begin(), inst_list.end(), at);
    // 如果是最后一条
    if(pos == inst_list.end())
        inst_list.push_back(src);
    else
        inst_list.insert(pos+1, src);
}

void MachineFunction::output()
{

    fprintf(yyout, "\t.global %s\n", this->sym_ptr->toStr().c_str() + 1);
    fprintf(yyout, "\t.type %s , %%function\n", this->sym_ptr->toStr().c_str() + 1);
    fprintf(yyout, "%s:\n", this->sym_ptr->toStr().c_str() + 1);
    // 保存寄存器
    //3. Save callee saved register
    fprintf(yyout, "\tpush {");
    for(auto reg : getSavedRRegs()){
        reg->output();
        fprintf(yyout, ", ");
    }
    //1. Save fp
    fprintf(yyout, "fp, lr}\n");

    // 保存浮点寄存器
    std::vector<MachineOperand*> fregs = getSavedFRegs();
    if(!fregs.empty()) {
        fprintf(yyout, "\tvpush {");
        fregs[0]->output();
        for (int i = 1; i < int(fregs.size()); i++) {
            fprintf(yyout, ", ");
            fregs[i]->output();
        }
        fprintf(yyout, "}\n");
    }
    // 调整 additional_args 中的偏移
    for(auto param : this->saved_params_offset) {
        param->setVal(4 * (this->saved_regs.size() + 2) + param->getVal());
    }
    //2. 设置fp = sp
    fprintf(yyout, "\tmov fp, sp\n");
    //4. 给局部变量分配栈空间
    if(stack_size!=0){
        if(stack_size > 255) {
            fprintf(yyout, "\tldr r4,=%d\n", stack_size);
            fprintf(yyout, "\tsub sp, sp, r4\n");
        }
        else {
            fprintf(yyout, "\tsub sp, sp, #%d\n", stack_size);
        }
    }
    // 遍历代码块生成代码(广度优先搜索)
    std::queue<MachineBlock*> q;
    std::set<MachineBlock*> v;
    q.push(block_list[0]);
    v.insert(block_list[0]);
    int cnt = 0;
    while(!q.empty()) {
        MachineBlock* cur = q.front();
        q.pop();
        cur->output();
        cnt += int(cur->getInsts().size());
        if(cnt > 160) {
            fprintf(yyout, "\tb .F%d\n", parent->getN());
            parent->PrintGlobal();
            fprintf(yyout, ".F%d:\n", parent->getN()-1);
            cnt = 0;
        }
        for(auto iter : cur->getSuccs()) {
            if(v.find(iter) == v.end()) {
                q.push(iter);
                v.insert(iter);
            }
        }
    }
    // output label .LEND
    fprintf(yyout, ".L%s_END:\n", this->sym_ptr->toStr().erase(0,1).c_str());
    // 恢复寄存器和sp fp
    //2. Restore callee saved registers and sp, fp
    if(stack_size!=0){
        if(stack_size > 255) {
            fprintf(yyout, "\tldr r4,=%d\n", stack_size);
            fprintf(yyout, "\tadd sp, sp, r4\n");
        }
        else {
            fprintf(yyout, "\tadd sp, sp, #%d\n", stack_size);
        }
    }
    //恢复浮点寄存器
    if(!fregs.empty()) {
        fprintf(yyout, "\tvpop {");
        fregs[0]->output();
        for (int i = 1; i < int(fregs.size()); i++) {
            fprintf(yyout, ", ");
            fregs[i]->output();
        }
        fprintf(yyout, "}\n");
    }
    fprintf(yyout, "\tpop {");
    for(auto reg : getSavedRRegs()){
        reg->output();
        fprintf(yyout, ", ");
    }
    fprintf(yyout, "fp, lr}\n");
    // 3. bx指令返回
    fprintf(yyout, "\tbx lr\n\n");
}

void MachineUnit::PrintGlobalDecl()
{
    if(global_var_list.empty()) {
        return;
    }
    fprintf(yyout, "\t.data\n");
    for(auto var : global_var_list) {
        if(var->getType()->isArray()) {
            if(var->arrayValues.empty()) {
                fprintf(yyout, "\t.comm\t%s,%d,4\n", var->toStr().erase(0,1).c_str(), var->getType()->getSize());
            }
            else {
                fprintf(yyout, "\t.global %s\n", var->toStr().erase(0,1).c_str());
                fprintf(yyout, "\t.align 4\n");
                fprintf(yyout,"\t.size %s, %d\n", var->toStr().erase(0,1).c_str(), var->getType()->getSize());
                fprintf(yyout,"%s:\n", var->toStr().erase(0,1).c_str());
                if(var->getType()->isIntArray() || var->getType()->isConstIntArray()) {
                    for (auto value: var->arrayValues) {
                        fprintf(yyout, "\t.word %d\n", int(value));
                    }
                }
                else {
                    for (auto value: var->arrayValues) {
                        auto tmp_value = float(value);
                        uint32_t temp = reinterpret_cast<uint32_t&>(tmp_value);
                        fprintf(yyout, "\t.word %u\n", temp);
                    }
                }
            }
        }
        else {
            fprintf(yyout, "\t.global %s\n", var->toStr().erase(0,1).c_str());
            fprintf(yyout, "\t.align 4\n");
            fprintf(yyout,"\t.size %s, %d\n", var->toStr().erase(0,1).c_str(), var->getType()->getSize());
            fprintf(yyout,"%s:\n", var->toStr().erase(0,1).c_str());
            if(var->getType()->isInt()) {
                fprintf(yyout, "\t.word %d\n", int(var->value));
            } else {
                auto value = float(var->value);
                uint32_t temp = reinterpret_cast<uint32_t&>(value);
                fprintf(yyout, "\t.word %u\n", temp);
            }
        }
    }
}

void MachineUnit::output()
{
    // TODO
    /* Hint:
    * 1. You need to print global variable/const declarition code;
    * 2. Traverse all the function in func_list to print assembly code;
    * 3. Don't forget print bridge label at the end of assembly code!! */
    fprintf(yyout, "\t.arch armv8-a\n");
    fprintf(yyout, "\t.arch_extension crc\n");
    //fprintf(yyout, "\t.fpu neon-vfpv4\n"); // 启用NEON和VFPv4浮点单元
    fprintf(yyout, "\t.arm\n");
    PrintGlobalDecl();
    for(auto iter : func_list)
        iter->output();
    PrintGlobal();
}

void MachineUnit::insertGlobalVar(IdentifierSymbolEntry *sym_ptr) {
    global_var_list.push_back(sym_ptr);
}

// 全局变量
void MachineUnit::PrintGlobal() {
    for (auto sym_ptr: global_var_list) {
        fprintf(yyout, "addr_%s_%d:\n", sym_ptr->toStr().erase(0,1).c_str(), n);
        fprintf(yyout, "\t.word %s\n", sym_ptr->toStr().erase(0,1).c_str());
    }
    n++;
}
