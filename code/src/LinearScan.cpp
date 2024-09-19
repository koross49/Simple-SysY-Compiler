#include <algorithm>
#include "LinearScan.h"
#include "MachineCode.h"
#include "LiveVariableAnalysis.h"

LinearScan::LinearScan(MachineUnit *unit)
{
    this->unit = unit;
    for (int i = 4; i < 11; i++)
        regs.push_back(i);
    for (int i = 21; i < 48; i++)
        fregs.push_back(i);
}

void LinearScan::allocateRegisters()
{
    // 遍历所有函数
    for (auto &f : unit->getFuncs())
    {
        func = f;
        bool success;
        success = false;
        // 重复直到所有虚拟寄存器都能映射
        while (!success)
        {
            computeLiveIntervals();
            success = linearScanRegisterAllocation();
            if (success)
            {
                // 所有虚拟寄存器都能映射到实际寄存器
                modifyCode();
            }
            else
            {
                // 无法映射到实际寄存器的虚拟寄存器溢出
                genSpillCode();
            }
        }
    }
}


// 构建数据流图
void LinearScan::makeDuChains()
{
    // 创建活跃变量分析对象
    LiveVariableAnalysis lva;
    lva.pass(func);  // 对函数进行活跃变量分析

    du_chains.clear();  // 清空数据流图

    int i = 0;
    std::map<MachineOperand, std::set<MachineOperand *>> liveVar;  // 存储活跃变量的map

    // 遍历函数的基本块
    for (auto &bb : func->getBlocks())
    {
        liveVar.clear();  // 清空当前基本块的活跃变量信息

        // 初始化当前基本块的活跃变量
        for (auto &t : bb->getLiveOut())
            liveVar[*t].insert(t);

        int no;
        no = i = bb->getInsts().size() + i;

        // 逆序遍历基本块的指令
        for (auto inst = bb->getInsts().rbegin(); inst != bb->getInsts().rend(); inst++)
        {
            (*inst)->setNo(no--);  // 设置指令的编号

            // 处理定义的变量
            for (auto &def : (*inst)->getDef())
            {
                if (def->isVReg())
                {
                    auto &uses = liveVar[*def];
                    du_chains[def].insert(uses.begin(), uses.end());

                    auto &kill = lva.getAllUses()[*def];

                    // 计算差集，得到活跃变量
                    std::set<MachineOperand *> res;
                    std::set_difference(uses.begin(), uses.end(), kill.begin(), kill.end(), std::inserter(res, res.end()));
                    liveVar[*def] = res;
                }
            }

            // 处理使用的变量
            for (auto &use : (*inst)->getUse())
            {
                if (use->isVReg())
                    liveVar[*use].insert(use);
            }
        }
    }
}

// 活跃区间分析
void LinearScan::computeLiveIntervals()
{
    makeDuChains();  // 构建数据流图

    intervals.clear();  // 清空活跃区间列表

    // 遍历数据流图的每一个数据使用链
    for (auto &du_chain : du_chains)
    {
        int t = -1;

        // 计算数据使用链中的最大编号
        for (auto &use : du_chain.second)
            t = std::max(t, use->getParent()->getNo());

        // 创建新的活跃区间
        Interval *interval = new Interval({du_chain.first->getParent()->getNo(), t, false, 0, 0, du_chain.first->isFloat(), {du_chain.first}, du_chain.second});
        
        // 将新的活跃区间加入列表
        intervals.push_back(interval);
    }

    // 遍历活跃区间列表
    for (auto& interval : intervals) {
        auto uses = interval->uses;
        auto begin = interval->start;
        auto end = interval->end;

        // 遍历函数的基本块
        for (auto block : func->getBlocks()) {
            auto liveIn = block->getLiveIn();
            auto liveOut = block->getLiveOut();
            bool in = false;
            bool out = false;

            // 检查活跃区间是否在基本块的liveIn和liveOut集合中
            for (auto use : uses)
                if (liveIn.count(use)) {
                    in = true;
                    break;
                }
            for (auto use : uses)
                if (liveOut.count(use)) {
                    out = true;
                    break;
                }

            // 根据情况更新活跃区间的起始和结束位置
            if (in && out) {
                begin = std::min(begin, (*(block->begin()))->getNo());
                end = std::max(end, (*(block->rbegin()))->getNo());
            } else if (!in && out) {
                for (auto i : block->getInsts())
                    if (i->getDef().size() > 0 &&
                        i->getDef()[0] == *(uses.begin())) {
                        begin = std::min(begin, i->getNo());
                        break;
                    }
                end = std::max(end, (*(block->rbegin()))->getNo());
            } else if (in && !out) {
                begin = std::min(begin, (*(block->begin()))->getNo());
                int temp = 0;
                for (auto use : uses)
                    if (use->getParent()->getParent() == block)
                        temp = std::max(temp, use->getParent()->getNo());
                end = std::max(temp, end);
            }
        }

        // 更新活跃区间的起始和结束位置
        interval->start = begin;
        interval->end = end;
    }

    bool change;
    change = true;

    // 进行合并相交区间的迭代，直到没有变化为止
    while (change)
    {
        change = false;
        std::vector<Interval *> t(intervals.begin(), intervals.end());

        // 遍历活跃区间列表
        for (size_t i = 0; i < t.size(); i++)
            for (size_t j = i + 1; j < t.size(); j++)
            {
                Interval *w1 = t[i];
                Interval *w2 = t[j];

                // 检查两个区间是否有相同的定义变量
                if (**w1->defs.begin() == **w2->defs.begin())
                {
                    std::set<MachineOperand *> temp;

                    // 计算两个区间的使用变量的交集
                    set_intersection(w1->uses.begin(), w1->uses.end(), w2->uses.begin(), w2->uses.end(), inserter(temp, temp.end()));

                    // 如果交集非空，进行区间合并
                    if (!temp.empty())
                    {
                        change = true;

                        // 将w2的定义和使用变量合并到w1中
                        w1->defs.insert(w2->defs.begin(), w2->defs.end());
                        w1->uses.insert(w2->uses.begin(), w2->uses.end());

                        // 更新w1的起始和结束位置
                        auto w1Min = std::min(w1->start, w1->end);
                        auto w1Max = std::max(w1->start, w1->end);
                        auto w2Min = std::min(w2->start, w2->end);
                        auto w2Max = std::max(w2->start, w2->end);
                        w1->start = std::min(w1Min, w2Min);
                        w1->end = std::max(w1Max, w2Max);

                        // 从区间列表中移除w2
                        auto it = std::find(intervals.begin(), intervals.end(), w2);
                        if (it != intervals.end())
                            intervals.erase(it);
                    }
                }
            }
    }

    // 按照活跃区间的起始位置排序
    sort(intervals.begin(), intervals.end(), compareStart);
}

// 寄存器分配
bool LinearScan::linearScanRegisterAllocation()
{
    bool retValue = true;
    active.clear();
    regs.clear();
    fregs.clear();
    for (int i = 4; i < 11; i++)//初始化reg堆
        regs.push_back(i);
    for (int i = 21; i < 48; i++)//初始化reg堆
        fregs.push_back(i);
    // 遍历活动区间列表
    for(auto &interval : intervals){
        expireOldIntervals(interval);// 处理过期的活动区间

        if(interval->freg){
            //判断 active 列表中 interval 的数目和可用的物理寄存器数目是否相等
            if(fregs.size() == 0){
                spillAtInterval(interval);// 溢出
                retValue = false;
            }
            else{//当前有可用于分配的物理寄存器
                interval->rreg = fregs[fregs.size()-1];//为 unhandled interval 分配物理寄存器
                fregs.pop_back();
                active.push_back(interval);
                sort(active.begin(), active.end(), insertComp);
            }
        }
        else{
            //判断 active 列表中 interval 的数目和可用的物理寄存器数目是否相等
            if(regs.size() == 0){
                spillAtInterval(interval);// 溢出
                retValue = false;
            }
            else{//当前有可用于分配的物理寄存器
                interval->rreg = regs[regs.size()-1];//为 unhandled interval 分配物理寄存器
                regs.pop_back();
                active.push_back(interval);
                sort(active.begin(), active.end(), insertComp);
            }
        }
    }
    return retValue;
}

void LinearScan::modifyCode()
{
    for (auto &interval : intervals)
    {
        func->addSavedRegs(interval->rreg);
        for (auto def : interval->defs)
            def->setReg(interval->rreg);
        for (auto use : interval->uses)
            use->setReg(interval->rreg);
    }
}

void LinearScan::genSpillCode()
{
    // 遍历活跃区间列表
    for(auto &interval : intervals)
    {
        // 如果当前活跃区间不需要溢出处理，继续下一个区间
        if(!interval->spill)
            continue;

        // 为溢出的区间分配栈空间
        interval->disp = func->AllocSpace(4);

        // 1. 在每个use变量的位置之前插入ldr指令
        for (auto use : interval->uses){
            MachineBlock* block = use->getParent()->getParent();
            MachineOperand* offset = new MachineOperand(MachineOperand::IMM, -interval->disp);

            // 如果使用的是整数寄存器
            if(!use->isFloat()){
                block->insertBefore(use->getParent(), new LoadMInstruction(block, new MachineOperand(*use), new MachineOperand(MachineOperand::REG, 11), offset, LoadMInstruction::LDR));
            }
            // 如果使用的是浮点寄存器
            else{
                block->insertBefore(use->getParent(), new LoadMInstruction(block, new MachineOperand(*use), new MachineOperand(MachineOperand::REG, 11), offset, LoadMInstruction::VLDR));
            }
        }

        // 2. 在每个def变量的位置之后插入str指令
        for (auto def : interval->defs){
            MachineBlock* block = def->getParent()->getParent();
            MachineOperand* offset = new MachineOperand(MachineOperand::IMM, -interval->disp);

            // 如果定义的是整数寄存器
            if(!def->isFloat()){
                block->insertAfter(def->getParent(), new StoreMInstruction(block, new MachineOperand(*def), new MachineOperand(MachineOperand::REG, 11), offset, StoreMInstruction::STR));
            }
            // 如果定义的是浮点寄存器
            else{
                block->insertAfter(def->getParent(), new StoreMInstruction(block, new MachineOperand(*def), new MachineOperand(MachineOperand::REG, 11), offset, StoreMInstruction::VSTR));
            }
        }
    }
}

// 处理过期的活动区间
void LinearScan::expireOldIntervals(Interval *interval)
{
    // 遍历活跃区间列表
    for(std::vector<Interval*>::iterator it = active.begin(); it != active.end(); ){
        // 检查当前活跃区间是否应该过期（根据活跃区间的结束位置）
        if(!victimComp(*it, interval)){
            return;  // 如果当前活跃区间不应过期，则直接返回
        }

        // 如果活跃区间应该过期
        // rreg表示整数寄存器编号小于11，freg表示浮点寄存器编号
        if ((*it)->rreg < 11) {
            regs.push_back((*it)->rreg);  // 将整数寄存器添加到可用寄存器列表
            it = active.erase(find(active.begin(), active.end(), *it));  // 从活跃列表中移除该活跃区间
            sort(regs.begin(), regs.end());  // 对可用整数寄存器列表进行排序
        }
        else {
            fregs.push_back((*it)->rreg);  // 将浮点寄存器添加到可用寄存器列表
            it = active.erase(find(active.begin(), active.end(), *it));  // 从活跃列表中移除该活跃区间
            sort(fregs.begin(), fregs.end());  // 对可用浮点寄存器列表进行排序
        }
    }
}

// 处理溢出
void LinearScan::spillAtInterval(Interval *interval)
{
    /*
        spill ← last interval in active
        if endpoint[spill] > endpoint[i] then
            register[i] ← register[spill]
            location[spill] ← new stack location
            remove spill from active
            add i to active, sorted by increasing end point
        else
            location[i] ← new stack location

    */
   // 溢出的活跃区间结束更晚,就让他溢出
    if (active[active.size() - 1]->end <= interval->end) {
        interval->spill = true;  // 只需要将 unhandled 区间的 spill 标志置位
    }
    else {// 最后一个活跃区间结束更晚,则让最后一个活跃区间的溢出
        active[active.size() - 1]->spill = true;  // 将最后一个活跃区间的 spill 标志置位
        interval->rreg = active[active.size() - 1]->rreg;  // 将其占用的寄存器分配给 unhandled 区间

        active.push_back(interval);  // 将 unhandled 区间添加到活跃区间列表
        sort(active.begin(), active.end(), insertComp);  // 根据起始位置对活跃区间列表进行排序
    }
}


bool LinearScan::compareStart(Interval *a, Interval *b)
{
    return a->start < b->start;
}