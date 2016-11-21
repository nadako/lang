package toylang;

import toylang.Type;

enum SyntaxEdge {
    SEBranch(bbThen:BasicBlock, bbElse:Null<BasicBlock>, bbNext:BasicBlock);
    SESwitch(cases:Array<SESwitchCase>, bbDefault:Null<BasicBlock>, bbNext:BasicBlock);
    SELoop(head:BasicBlock, bbBody:BasicBlock, bbNext:BasicBlock);
    SENone;
}

typedef SESwitchCase = {expr:TExpr, body:BasicBlock}