#ifndef __BASIC_BLOCK_H__
#define __BASIC_BLOCK_H__
#include <vector>
#include <set>
#include "Instruction.h"
#include "AsmBuilder.h"

class Function;

class BasicBlock
{
    typedef std::vector<BasicBlock *>::iterator bb_iterator;//基本迭代块

private:
    std::vector<BasicBlock *> pred, succ;//前驱后继基本块列表
    Instruction *head;//头指令
    Function *parent;
    int no;

public:
    BasicBlock(Function *);
    ~BasicBlock();
    void insertFront(Instruction *);
    void insertBack(Instruction *);
    void insertBefore(Instruction *, Instruction *);
    void remove(Instruction *);
    bool empty() const { return head->getNext() == head;}
    void output() const;
    bool succEmpty() const { return succ.empty(); };
    bool predEmpty() const { return pred.empty(); };
    void addSucc(BasicBlock *);
    void removeSucc(BasicBlock *);
    void addPred(BasicBlock *);
    void removePred(BasicBlock *);
    int getNo() { return no; };
    Function *getParent() { return parent; };
    Instruction* begin() { return head->getNext();};
    Instruction* end() { return head;};
    Instruction* rbegin() { return head->getPrev();};
    Instruction* rend() { return head;};
    bb_iterator succ_begin() { return succ.begin(); };
    bb_iterator succ_end() { return succ.end(); };
    bb_iterator pred_begin() { return pred.begin(); };
    bb_iterator pred_end() { return pred.end(); };
    int getNumOfPred() const { return pred.size(); };
    int getNumOfSucc() const { return succ.size(); };
    void genMachineCode(AsmBuilder*);
};

#endif