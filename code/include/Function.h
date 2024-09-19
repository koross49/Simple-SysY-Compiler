#ifndef __FUNCTION_H__
#define __FUNCTION_H__

#include <vector>
#include <map>
#include <set>
#include <algorithm>
#include <iostream>
#include "BasicBlock.h"
#include "SymbolTable.h"
#include "AsmBuilder.h"

class Unit;

class Function
{
    typedef std::vector<BasicBlock *>::iterator iterator;//正向迭代
    typedef std::vector<BasicBlock *>::reverse_iterator reverse_iterator;//反向迭代

private:
    std::vector<BasicBlock *> block_list;//基本块
    SymbolEntry *sym_ptr;//符号表
    BasicBlock *entry;//函数入口块
    Unit *parent;//编译单元
    std::vector<Operand*> params_list;//参数列表
    std::vector<Operand*> iparams_list;
    std::vector<Operand*> fparams_list;

public:
    Function(Unit *, SymbolEntry *);
    ~Function();
    void insertBlock(BasicBlock *bb) { block_list.push_back(bb); };
    BasicBlock *getEntry() { return entry; };
    void remove(BasicBlock *bb);
    void output() const;
    std::vector<BasicBlock *> &getBlockList(){return block_list;};
    iterator begin() { return block_list.begin(); };
    iterator end() { return block_list.end(); };
    reverse_iterator rbegin() { return block_list.rbegin(); };
    reverse_iterator rend() { return block_list.rend(); };
    SymbolEntry *getSymPtr() { return sym_ptr; };
    void insertParam(Operand* param);
    void genMachineCode(AsmBuilder*);
    int getParamId(Operand* param);
    int getIParamId(Operand* param);
    int getFParamId(Operand* param);
};

#endif
