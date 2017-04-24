
// Copyright (c) 2017 Massachusetts Institute of Technology
// 
// Permission is hereby granted, free of charge, to any person
// obtaining a copy of this software and associated documentation
// files (the "Software"), to deal in the Software without
// restriction, including without limitation the rights to use, copy,
// modify, merge, publish, distribute, sublicense, and/or sell copies
// of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
// 
// The above copyright notice and this permission notice shall be
// included in all copies or substantial portions of the Software.
// 
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
// EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
// MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
// NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
// BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
// ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
// CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.

import Types::*;
import CacheUtils::*;
import MemoryTypes::*;
import CCTypes::*;
import FShow::*;
import Fifo::*;
import MsgFifo::*;

interface MemBank#(
    numeric type lgBankNum,
    numeric type ldQSz, // load Q size
    numeric type wrBSz, // write buffer size
    type childT,
    type ldRqIdT
);
    interface MemFifoServer#(ldRqIdT, childT) to_proc;
    interface RiscyAxiMaster to_mem;
endinterface

typedef struct {
    childT child;
    ldRqIdT id;
} LdMemTag#(type ldRqIdT, type childT) deriving(Bits, Eq, FShow);

module mkMemBank(
    MemBank#(lgBankNum, ldQSz, wrBSz, childT, ldRqIdT)
) provisos (
    Alias#(ldTagT, LdMemTag#(ldRqIdT, childT)),
    Alias#(toMemT, ToMemMsg#(ldRqIdT, childT)),
    Alias#(memRsT, MemRsMsg#(ldRqIdT, childT)),
    Add#(1, a__, wrBSz),
    Alias#(childT, Bit#(_childSz)),
    Alias#(ldRqIdT, Bit#(_ldRqIdT))
);

    Fifo#(2, toMemT) fromCQ <- mkCFFifo;
    Fifo#(2, memRsT) rsToCQ <- mkCFFifo;
    CacheToAxi#(wrBSz, ldQSz, ldTagT) axiMem <- mkCacheToAxi;

    rule sendToMem;
        fromCQ.deq;
        $display("%t Mem %m sendToMem: ", $time, fshow(fromCQ.first));
        case(fromCQ.first) matches
            tagged Ld .rq: begin
                axiMem.msgToAxi.enqReq(CacheReqToMem {
                    cAddr: getLineAddr(rq.addr),
                    id: LdMemTag {child: rq.child, id: rq.id}
                });
            end
            tagged Wb .rs: begin
                axiMem.msgToAxi.enqResp(CacheRespToMem {
                    cAddr: getLineAddr(rs.addr),
                    data: pack(rs.data)
                });
            end
            default: begin
                doAssert(False, "impossible toMem type");
            end
        endcase
    endrule

    rule deqWbDone;
        axiMem.wbDone.deq;
    endrule

    rule getMemResp;
        axiMem.ldResp.deq;
        MemRespToCache#(ldTagT) resp = axiMem.ldResp.first;
        memRsT mRs = MemRsMsg {
            data: unpack(resp.data),
            child: resp.id.child,
            id: resp.id.id
        };
        rsToCQ.enq(mRs);
        $display("%t Mem %m getMemResp: ", $time, fshow(mRs));
    endrule

    interface MemFifoServer to_proc;
        interface fromCPut = toFifoEnq(fromCQ);
        interface rsToCGet = toFifoDeq(rsToCQ);
    endinterface
    interface to_mem = axiMem.to_mem;
endmodule

