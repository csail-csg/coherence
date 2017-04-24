
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

import Vector::*;
import FShow::*;
import Types::*;
import CCTypes::*;
import CCPipe::*;
import RWBramCore::*;
import RandomReplace::*;

// type param ordering: bank < way < index < tag < cRq < pRq

// in L1 cache, only cRq can occupy cache line (pRq handled immediately)
// replacement is always done immediately (never have replacing line)
// so cache owner type is simply Maybe#(cRqIdxT)

// input types
typedef struct {
    Addr addr;
    rqIdxT mshrIdx;
} L1PipeRqIn#(type rqIdxT) deriving(Bits, Eq, FShow);

typedef struct {
    Addr addr;
    Msi toState;
    Maybe#(Line) data;
    wayT way;
} L1PipePRsIn#(type wayT) deriving(Bits, Eq, FShow);

typedef union tagged {
    L1PipeRqIn#(cRqIdxT) CRq;
    L1PipeRqIn#(pRqIdxT) PRq;
    L1PipePRsIn#(wayT) PRs;
} L1PipeIn#(
    type wayT,
    type cRqIdxT, 
    type pRqIdxT
) deriving (Bits, Eq, FShow);

// output cmd to the processing rule in L1$
typedef union tagged {
    cRqIdxT L1CRq;
    pRqIdxT L1PRq;
    void L1PRs;
} L1Cmd#(
    type cRqIdxT, 
    type pRqIdxT
) deriving (Bits, Eq, FShow);

interface L1Pipe#(
    numeric type lgBankNum,
    numeric type wayNum,
    type indexT,
    type tagT,
    type cRqIdxT, 
    type pRqIdxT
);
    method Action send(L1PipeIn#(Bit#(TLog#(wayNum)), cRqIdxT, pRqIdxT) r);
    method PipeOut#(
        Bit#(TLog#(wayNum)), 
        tagT, Msi, void, // no dir
        Maybe#(cRqIdxT),
        Line, L1Cmd#(cRqIdxT, pRqIdxT)
    ) first;
    method Action deqWrite(
        Maybe#(cRqIdxT) swapRq,
        RamData#(tagT, Msi, void, Maybe#(cRqIdxT), Line) wrRam // always write BRAM
    );
endinterface

// real cmd used in pipeline
typedef struct {
    Addr addr;
    wayT way;
} L1PipePRsCmd#(type wayT) deriving(Bits, Eq, FShow);

typedef union tagged {
    L1PipeRqIn#(cRqIdxT) CRq;
    L1PipeRqIn#(pRqIdxT) PRq;
    L1PipePRsCmd#(wayT) PRs;
} L1PipeCmd#(
    type wayT,
    type cRqIdxT, 
    type pRqIdxT
) deriving (Bits, Eq, FShow);

module mkL1Pipe(
    L1Pipe#(lgBankNum, wayNum, indexT, tagT, cRqIdxT, pRqIdxT)
) provisos(
    Alias#(wayT, Bit#(TLog#(wayNum))),
    Alias#(dirT, void), // no directory
    Alias#(ownerT, Maybe#(cRqIdxT)),
    Alias#(pipeInT, L1PipeIn#(wayT, cRqIdxT, pRqIdxT)),
    Alias#(pipeCmdT, L1PipeCmd#(wayT, cRqIdxT, pRqIdxT)),
    Alias#(l1CmdT, L1Cmd#(cRqIdxT, pRqIdxT)),
    Alias#(pipeOutT, PipeOut#(wayT, tagT, Msi, dirT, ownerT, Line, l1CmdT)), // output type
    Alias#(infoT, CacheInfo#(tagT, Msi, dirT, ownerT)),
    Alias#(ramDataT, RamData#(tagT, Msi, dirT, ownerT, Line)),
    Alias#(respStateT, RespState#(Msi)),
    Alias#(tagMatchResT, TagMatchResult#(wayT)),
    // requirement
    Alias#(indexT, Bit#(indexSz)),
    Alias#(tagT, Bit#(tagSz)),
    Alias#(cRqIdxT, Bit#(cRqIdxSz)),
    Alias#(pRqIdxT, Bit#(pRqIdxSz)),
    Add#(indexSz, a__, AddrSz),
    Add#(tagSz, b__, AddrSz)
);
    // RAMs
    Vector#(wayNum, RWBramCore#(indexT, infoT)) infoRam <- replicateM(mkRWBramCore);
    Vector#(wayNum, RWBramCore#(indexT, Line)) dataRam <- replicateM(mkRWBramCore);
    
    // initialize RAM
    Reg#(Bool) initDone <- mkReg(False);
    Reg#(indexT) initIndex <- mkReg(0);

    rule doInit(!initDone);
        for(Integer i = 0; i < valueOf(wayNum); i = i+1) begin
            infoRam[i].wrReq(initIndex, CacheInfo {
                tag: 0,
                cs: I,
                dir: ?,
                owner: Invalid
            });
        end
        initIndex <= initIndex + 1;
        if(initIndex == maxBound) begin
            initDone <= True;
        end
    endrule

    // random replacement
    RandomReplace#(wayNum) randRep <- mkRandomReplace;

    // functions
    function Addr getAddrFromCmd(pipeCmdT cmd);
        return (case(cmd) matches
            tagged CRq .r: r.addr;
            tagged PRq .r: r.addr;
            tagged PRs .r: r.addr;
            default: ?;
        endcase);
    endfunction

    function indexT getIndex(pipeCmdT cmd);
        Addr a = getAddrFromCmd(cmd);
        return truncate(a >> (valueOf(LgLineSzBytes) + valueOf(lgBankNum)));
    endfunction

    function ActionValue#(tagMatchResT) tagMatch(
        pipeCmdT cmd,
        Vector#(wayNum, tagT) tagVec, 
        Vector#(wayNum, Msi) csVec, 
        Vector#(wayNum, ownerT) ownerVec
    );
        return actionvalue
            function tagT getTag(Addr a) = truncateLSB(a);

            $display("%t L1 %m tagMatch: ", $time, 
                fshow(cmd), " ; ", 
                fshow(getTag(getAddrFromCmd(cmd))),
                fshow(tagVec), " ; ", 
                fshow(csVec), " ; ", 
                fshow(ownerVec), " ; " 
            );
            if(cmd matches tagged PRs .rs) begin
                // PRs directly read from cmd
                return TagMatchResult {
                    way: rs.way,
                    pRqMiss: False
                };
            end
            else begin
                // CRq/PRq: need tag matching
                Addr addr = getAddrFromCmd(cmd);
                tagT tag = getTag(addr);
                // find hit way (nothing is being replaced)
                function Bool isMatch(Tuple2#(Msi, tagT) csTag);
                    match {.cs, .t} = csTag;
                    return cs > I && t == tag;
                endfunction
                Maybe#(wayT) hitWay = searchIndex(isMatch, zip(csVec, tagVec));
                if(hitWay matches tagged Valid .w) begin
                    return TagMatchResult {
                        way: w,
                        pRqMiss: False
                    };
                end
                else if(cmd matches tagged PRq .rq) begin
                    // pRq miss
                    return TagMatchResult {
                        way: 0, // default to 0
                        pRqMiss: True
                    };
                end
                else begin
                    // find a unlocked way to replace for cRq
                    Vector#(wayNum, Bool) unlocked = ?;
                    Vector#(wayNum, Bool) invalid = ?;
                    for(Integer i = 0; i < valueOf(wayNum); i = i+1) begin
                        invalid[i] = csVec[i] == I;
                        unlocked[i] = !isValid(ownerVec[i]);
                    end
                    Maybe#(wayT) repWay = randRep.getReplaceWay(unlocked, invalid);
                    // sanity check: repWay must be valid
                    if(!isValid(repWay)) begin
                        $fwrite(stderr, "[L1Pipe] ERROR: ", fshow(cmd), " cannot find way to replace\n");
                        $finish;
                    end
                    return TagMatchResult {
                        way: fromMaybe(?, repWay),
                        pRqMiss: False
                    };
                end
            end
        endactionvalue;
    endfunction

    function ActionValue#(dirT) updateChildDir(pipeCmdT cmd, Msi toState, dirT oldDir);
    actionvalue
        doAssert(False, "L1 should not update dir");
        return ?;
    endactionvalue
    endfunction

    function Action checkUpPRsDataValid(Msi cs, Bool dataV);
    action
        doAssert((cs == I) == dataV, ("valid resp data for upgrade from I"));
    endaction
    endfunction

    function Action checkDownCRsDataValid(pipeCmdT cmd, dirT dir, Bool dataV);
    action
        doAssert(False, ("L1 should not have cRs"));
    endaction
    endfunction

    CCPipe#(wayNum, indexT, tagT, Msi, dirT, ownerT, Line, pipeCmdT) pipe <- mkCCPipe(
        regToReadOnly(initDone), getIndex, tagMatch, updateChildDir, 
        checkUpPRsDataValid, checkDownCRsDataValid,
        infoRam, dataRam
    );

    method Action send(pipeInT req);
        case(req) matches
            tagged CRq .rq: begin
                pipe.enq(CRq (rq), Invalid, Invalid);
            end
            tagged PRq .rq: begin
                pipe.enq(PRq (rq), Invalid, Invalid);
            end
            tagged PRs .rs: begin
                pipe.enq(PRs (L1PipePRsCmd {
                    addr: rs.addr,
                    way: rs.way
                }), rs.data, UpCs (rs.toState));
            end
        endcase
    endmethod

    // need to adapt pipeline output to real output format
    method pipeOutT first;
        let pout = pipe.first;
        return PipeOut {
            cmd: (case(pout.cmd) matches
                tagged CRq .rq: L1CRq (rq.mshrIdx);
                tagged PRq .rq: L1PRq (rq.mshrIdx);
                tagged PRs .rs: L1PRs;
                default: ?;
            endcase),
            way: pout.way,
            pRqMiss: pout.pRqMiss,
            ram: pout.ram
        };
    endmethod

    method Action deqWrite(Maybe#(cRqIdxT) swapRq, ramDataT wrRam);
        // get new cmd
        Addr addr = getAddrFromCmd(pipe.first.cmd); // inherit addr
        Maybe#(pipeCmdT) newCmd = Invalid;
        if(swapRq matches tagged Valid .idx) begin // swap in cRq
            newCmd = Valid (CRq (L1PipeRqIn {addr: addr, mshrIdx: idx}));
        end
        // call pipe
        pipe.deqWrite(newCmd, wrRam);
    endmethod
endmodule
