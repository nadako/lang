package toylang.cfg;

enum SyntaxEdge {
    SEBranch(bbThen:BasicBlock, bbElse:Null<BasicBlock>, bbNext:BasicBlock);
    SELoop(head:BasicBlock, bbBody:BasicBlock, bbNext:BasicBlock);
    SENone;
}
