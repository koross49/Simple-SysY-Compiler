#include "Function.h"
#include "Unit.h"
#include "Type.h"
#include <list>

extern FILE* yyout;

Function::Function(Unit *u, SymbolEntry *s)
{
    u->insertFunc(this);
    entry = new BasicBlock(this);
    sym_ptr = s;
    parent = u;
}

Function::~Function()
{
    auto delete_list = block_list;
    for (auto &i : delete_list)
        delete i;
    // parent->removeFunc(this);
}

// remove the basicblock bb from its block_list.
void Function::remove(BasicBlock *bb)
{
    block_list.erase(std::find(block_list.begin(), block_list.end(), bb));
}

// 函数声明
void Function::output() const
{
    FunctionType* funcType = dynamic_cast<FunctionType*>(sym_ptr->getType());
    Type *retType = funcType->getRetType();
    fprintf(yyout, "define %s %s(", retType->toStr().c_str(), sym_ptr->toStr().c_str());
    //打印参数列表
    bool first = true;
    for(auto param : params_list){
        if(first){
            first = false;
        }
        else fprintf(yyout, ", ");
        fprintf(yyout, "%s %s", param->getType()->toStr().c_str(), param->toStr().c_str());
    }
    fprintf(yyout, "){\n");
    //打印该函数内所有基本块
    std::set<BasicBlock *> v;
    std::list<BasicBlock *> q;
    q.push_back(entry);
    v.insert(entry);
    while (!q.empty())
    {
        auto bb = q.front();
        q.pop_front();
        bb->output();
        for (auto succ = bb->succ_begin(); succ != bb->succ_end(); succ++)
        {
            if (v.find(*succ) == v.end())
            {
                v.insert(*succ);
                q.push_back(*succ);
            }
        }
    }
    fprintf(yyout, "}\n");
}

void Function::genMachineCode(AsmBuilder* builder) 
{
    auto cur_unit = builder->getUnit();
    auto cur_func = new MachineFunction(cur_unit, this->sym_ptr);
    builder->setFunction(cur_func);
    std::map<BasicBlock*, MachineBlock*> map;
    for(auto block : block_list)
    {
        block->genMachineCode(builder);
        map[block] = builder->getBlock();
    }
    // Add pred and succ for every block
    for(auto block : block_list)
    {
        auto mblock = map[block];
        for (auto pred = block->pred_begin(); pred != block->pred_end(); pred++)
            mblock->addPred(map[*pred]);
        for (auto succ = block->succ_begin(); succ != block->succ_end(); succ++)
            mblock->addSucc(map[*succ]);
    }
    cur_unit->InsertFunc(cur_func);

}

void Function::insertParam(Operand* param) {
    if(param->getType()->isFloat()) {
        fparams_list.push_back(param);
    }
    else {
        iparams_list.push_back(param);
    }
}

int Function::getParamId(Operand *param) {
    int i = 0;
    for(auto p : iparams_list){
        if(p == param) return i;
        i++;
    }
    i = 0;
    for(auto p : fparams_list){
        if(p == param) return i;
        i++;
    }
    return -1;
}

int Function::getIParamId(Operand* param) {
    int i = 0;
    for(auto p : iparams_list){
        if(p == param) return i;
        i++;
    }
    return -1;
}


int Function::getFParamId(Operand* param) {
    int i = 0;
    for(auto p : fparams_list){
        if(p == param) return i;
        i++;
    }
    return -1;
}