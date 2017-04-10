import CacheUtils::*;
import Types::*;
import CCTypes::*;
import CCSizes::*;
import LLPipe::*;
import LLCRqMshr::*;
import LLBank::*;

(* synthesize *)
module mkLastLvCRqMshr(
    LLCRqMshr#(LLChildNum, LLCRqNum, LLWay, LLTag, cRqT)
) provisos(
    Alias#(cRqT, LLRq#(LLCRqId, DmaRqId, LLChild))
);
    function Addr getAddr(cRqT r) = r.addr;
    let m <- mkLLCRqMshr(getAddr);
    return m;
endmodule

(* synthesize *)
module mkLLPipeline(
    LLPipe#(LgLLBankNum, LLChildNum, LLWayNum, LLIndex, LLTag, LLCRqMshrIdx)
);
    let m <- mkLLPipe;
    return m;
endmodule

typedef LLBank#(LgLLBankNum, LLChildNum, LLWayNum, LLIndexSz, LLTagSz, LLCRqNum, LLCRqId, DmaRqId) LLBankWrapper;

(* synthesize *)
module mkLLBankWrapper(LLBankWrapper);
    let m <- mkLLBank(mkLastLvCRqMshr, mkLLPipeline);
    return m;
endmodule
