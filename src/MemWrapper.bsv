import Types::*;
import CacheUtils::*;
import CCTypes::*;
import CCSizes::*;
import MemBank::*;

typedef MemBank#(LgMemBankNum, MemLdQSz, MemWrBSz, MemChild, MemLdRqId) MemBankWrapper;

(* synthesize *)
module mkMemBankWrapper(MemBankWrapper);
    let m <- mkMemBank;
    return m;
endmodule
