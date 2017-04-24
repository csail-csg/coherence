
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
import CCTypes::*;
import CCSizes::*;
import L1Wrapper::*;
import DirWrapper::*;
import CrossBar::*;
import Vector::*;
import GetPut::*;
import ClientServer::*;
import L1Bank::*;
import DirBank::*;
import Connectable::*;

export L1Dir(..);
export mkL1Dir;

// DirChildNum == L1Num
// XXX no banking: LgDirBankNum = 0, LdL1BankNum = 0

typedef TExp#(LgDirBankNum) DirNum;
typedef Bit#(LgDirBankNum) DirIdx;
typedef Bit#(TLog#(L1Num)) L1Idx;

typedef CRqMsg#(L1Way, void) CRqFromL1;
typedef CRqMsg#(DirCRqId, DirChild) CRqToDir;
typedef CRsMsg#(void) CRsFromL1;
typedef CRsMsg#(DirChild) CRsToDir;
typedef PRqRsMsg#(DirCRqId, DirChild) PRqRsFromDir;
typedef PRqRsMsg#(L1Way, void) PRqRsToL1;


// cross bar for L1 cRq to Dir
typedef CrossBar#(L1Num, CRqFromL1, DirNum, CRqToDir) L1CRqToDirXBar;

(* synthesize *)
module mkL1CRqToDirXBar(L1CRqToDirXBar);
    function XBarDstInfo#(DirIdx, CRqToDir) getL1CRqDstInfo(L1Idx whichL1, CRqFromL1 rq);
        return XBarDstInfo {
            idx: 0,
            data: CRqMsg {
                addr: rq.addr,
                fromState: rq.fromState,
                toState: rq.toState,
                id: rq.id,
                child: whichL1
            }
        };
    endfunction

    let m <- mkCrossBar(getL1CRqDstInfo);
    return m;
endmodule

// cross bar for L1 cRs to Dir
typedef CrossBar#(L1Num, CRsFromL1, DirNum, CRsToDir) L1CRsToDirXBar;

(* synthesize *)
module mkL1CRsToDirXBar(L1CRsToDirXBar);
    function XBarDstInfo#(DirIdx, CRsToDir) getL1CRsDstInfo(L1Idx whichL1, CRsFromL1 rs);
        return XBarDstInfo {
            idx: 0,
            data: CRsMsg {
                addr: rs.addr,
                toState: rs.toState,
                data: rs.data,
                child: whichL1
            }
        };
    endfunction

    let m <- mkCrossBar(getL1CRsDstInfo);
    return m;
endmodule

// cross bar for Dir pRqRs to L1
typedef CrossBar#(DirNum, PRqRsFromDir, L1Num, PRqRsToL1) DirPRqRsToL1XBar;

(* synthesize *)
module mkDirPRqRsToL1XBar(DirPRqRsToL1XBar);
    function XBarDstInfo#(L1Idx, PRqRsToL1) getDirPRqRsDstInfo(DirIdx whichDir, PRqRsFromDir msg);
        return (case(msg) matches
            tagged PRq .rq: return XBarDstInfo {
                idx: rq.child,
                data: PRq (PRqMsg {
                    addr: rq.addr,
                    toState: rq.toState,
                    child: ?
                })
            };
            tagged PRs .rs: return XBarDstInfo {
                idx: rs.child,
                data: PRs (PRsMsg {
                    addr: rs.addr,
                    toState: rs.toState,
                    child: ?,
                    data: rs.data,
                    id: rs.id
                })
            };
        endcase);
    endfunction

    let m <- mkCrossBar(getDirPRqRsDstInfo);
    return m;
endmodule

// L1 + Dir
interface L1Dir;
    interface L1Proc#(DirChildNum, ProcRqId) to_proc;
    interface MemFifoClient#(DirCRqMshrIdx, void) to_mem;
endinterface

(* synthesize *)
module mkL1Dir(L1Dir) provisos(
    Add#(0, 0, LgL1BankNum),
    Add#(0, 0, LgDirBankNum),
    Add#(1, 0, DirNum)
);
    Vector#(L1Num, L1BankWrapper) l1 <- replicateM(mkL1BankWrapper);
    Vector#(DirNum, DirBankWrapper) dir <- replicateM(mkDirBankWrapper);

    let cRqXBar <- mkL1CRqToDirXBar;
    let cRsXBar <- mkL1CRsToDirXBar;
    let pXBar <- mkDirPRqRsToL1XBar;

    for(Integer i = 0; i < valueOf(L1Num); i = i+1) begin
        mkConnection(cRqXBar.srcIfc[i], l1[i].rqToPGet);
        mkConnection(cRsXBar.srcIfc[i], l1[i].rsToPGet);
        mkConnection(pXBar.dstIfc[i], l1[i].fromPPut);
    end

    for(Integer i = 0; i < valueOf(DirNum); i = i+1) begin
        mkConnection(cRqXBar.dstIfc[i], dir[i].rqFromCPut);
        mkConnection(cRsXBar.dstIfc[i], dir[i].rsFromCPut);
        mkConnection(pXBar.srcIfc[i], dir[i].toCGet);
    end

    function Server#(ProcRq#(ProcRqId, MemInst), ProcRs#(ProcRqId)) getL1Ifc(L1BankWrapper ifc);
        return (interface Server;
            interface request = ifc.rqFromCPut;
            interface response = ifc.toCGet;
        endinterface);
    endfunction
    interface to_proc = map(getL1Ifc, l1);
    interface to_mem = dir[0].to_mem;
endmodule
