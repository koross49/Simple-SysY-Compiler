#include "BasicBlock.h"
#include "Function.h"
#include <algorithm>

extern FILE* yyout;

//插到基本块开头,head始终指向的是一个空指令
void BasicBlock::insertFront(Instruction *inst)
{
    insertBefore(inst, head->getNext());
}

// 插到基本块末尾(双向链表)
void BasicBlock::insertBack(Instruction *inst) 
{
    insertBefore(inst, head);
}

//在src前插入dst
void BasicBlock::insertBefore(Instruction *dst, Instruction *src)
{
    Instruction *i = head;
    do{
        if(i==src){
            src->getPrev()->setNext(dst);
            dst->setPrev(src->getPrev());
            dst->setNext(src);
            src->setPrev(dst);
        }
        i = i->getNext();
    }while(i!=head);
    dst->setParent(this);
}

// 移除指令
void BasicBlock::remove(Instruction *inst)
{
    inst->getPrev()->setNext(inst->getNext());
    inst->getNext()->setPrev(inst->getPrev());
}

// 打印基本块
void BasicBlock::output() const
{
    fprintf(yyout, "B%d:", no);

    // 注释打印前驱列表
    if (!pred.empty())
    {
        fprintf(yyout, "%*c; preds = %%B%d", 32, '\t', pred[0]->getNo());
        for (auto i = pred.begin() + 1; i != pred.end(); i++)
            fprintf(yyout, ", %%B%d", (*i)->getNo());
    }
    // 注释打印后继列表
    if(!succ.empty())
    {
        fprintf(yyout, "%*c; succs = %%B%d", 32, '\t', succ[0]->getNo());
        for (auto i = succ.begin() + 1; i != succ.end(); i++)
            fprintf(yyout, ", %%B%d", (*i)->getNo());
    }
    fprintf(yyout, "\n");
    // 打印指令
    for (auto i = head->getNext(); i != head; i = i->getNext()) {
        i->output();
    }
}


// 添加删除前驱块
void BasicBlock::addSucc(BasicBlock *bb)
{
    succ.push_back(bb);
}

// remove the successor basicclock bb.
void BasicBlock::removeSucc(BasicBlock *bb)
{
    succ.erase(std::find(succ.begin(), succ.end(), bb));
}

//添加删除后继块
void BasicBlock::addPred(BasicBlock *bb)
{
    pred.push_back(bb);
}

// remove the predecessor basicblock bb.
void BasicBlock::removePred(BasicBlock *bb)
{
    pred.erase(std::find(pred.begin(), pred.end(), bb));
}

BasicBlock::BasicBlock(Function *f)
{
    this->no = SymbolTable::getLabel();//获取一个新的基本块号
    f->insertBlock(this);// 插入到函数的基本块列表中
    parent = f;
    head = new DummyInstruction();//head始终是一个空指令
    head->setParent(this);
}

BasicBlock::~BasicBlock()
{
    Instruction *inst;
    inst = head->getNext();
    while (inst != head)
    {
        Instruction *t;
        t = inst;
        inst = inst->getNext();
        delete t;
    }
    for(auto &bb:pred)
        bb->removeSucc(this);
    for(auto &bb:succ)
        bb->removePred(this);
    parent->remove(this);
}

void BasicBlock::genMachineCode(AsmBuilder* builder) 
{
    auto cur_func = builder->getFunction();
    auto cur_block = new MachineBlock(cur_func, no);
    builder->setBlock(cur_block);
    for (auto i = head->getNext(); i != head; i = i->getNext())
    {
        i->genMachineCode(builder);
    }
    cur_func->InsertBlock(cur_block);
}
