
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

// type param ordering: bank < child < way < index < tag < cRq

// input types
typedef struct {
    Addr addr;
    cRqIdxT mshrIdx;
} LLPipeCRqIn#(type cRqIdxT) deriving(Bits, Eq, FShow);

typedef struct {
    Addr addr;
    Msi toState; // come from req in MSHR (E or M)
    Line data; // come from memory must be valid
    wayT way; // come from MSHR
} LLPipeMRsIn#(type wayT) deriving(Bits, Eq, FShow);

typedef union tagged {
    LLPipeCRqIn#(cRqIdxT) CRq;
    CRsMsg#(childT) CRs;
    LLPipeMRsIn#(wayT) MRs;
} LLPipeIn#(
    type childT,
    type wayT,
    type cRqIdxT
) deriving (Bits, Eq, FShow);

// output cmd to the processing rule in LLC
typedef union tagged {
    cRqIdxT LLCRq; // mshr idx of the cRq
    childT LLCRs; // which child is downgrading
    void LLMRs;
} LLCmd#(type childT, type cRqIdxT) deriving (Bits, Eq, FShow);

interface LLPipe#(
    numeric type lgBankNum,
    numeric type childNum,
    numeric type wayNum,
    type indexT,
    type tagT,
    type cRqIdxT
);
    method Action send(LLPipeIn#(Bit#(TLog#(childNum)), Bit#(TLog#(wayNum)), cRqIdxT) r);
    method Bool notEmpty;
    method PipeOut#(
        Bit#(TLog#(wayNum)),
        tagT, Msi, Vector#(childNum, Msi),
        Maybe#(CRqOwner#(cRqIdxT)),
        Line, LLCmd#(Bit#(TLog#(childNum)), cRqIdxT)
    ) first;
    method PipeOut#(
        Bit#(TLog#(wayNum)),
        tagT, Msi, Vector#(childNum, Msi),
        Maybe#(CRqOwner#(cRqIdxT)),
        Line, LLCmd#(Bit#(TLog#(childNum)), cRqIdxT)
    ) unguard_first;
    method Action deqWrite(
        Maybe#(cRqIdxT) swapRq,
        RamData#(tagT, Msi, Vector#(childNum, Msi), Maybe#(CRqOwner#(cRqIdxT)), Line) wrRam // always write BRAM
    );
endinterface

// real cmd used in pipeline
typedef struct {
    Addr addr;
    childT child;
} LLPipeCRsCmd#(type childT) deriving(Bits, Eq, FShow);

typedef struct {
    Addr addr;
    wayT way;
} LLPipeMRsCmd#(type wayT) deriving(Bits, Eq, FShow);

typedef union tagged {
    LLPipeCRqIn#(cRqIdxT) CRq;
    LLPipeCRsCmd#(childT) CRs;
    LLPipeMRsCmd#(wayT) MRs;
} LLPipeCmd#(
    type childT,
    type wayT,
    type cRqIdxT
) deriving (Bits, Eq, FShow);

module mkLLPipe(
    LLPipe#(lgBankNum, childNum, wayNum, indexT, tagT, cRqIdxT)
) provisos(
    Alias#(childT, Bit#(TLog#(childNum))),
    Alias#(wayT, Bit#(TLog#(wayNum))),
    Alias#(dirT, Vector#(childNum, Msi)),
    Alias#(ownerT, Maybe#(CRqOwner#(cRqIdxT))),
    Alias#(pipeInT, LLPipeIn#(childT, wayT, cRqIdxT)),
    Alias#(pipeCmdT, LLPipeCmd#(childT, wayT, cRqIdxT)),
    Alias#(llCmdT, LLCmd#(childT, cRqIdxT)),
    Alias#(pipeOutT, PipeOut#(wayT, tagT, Msi, dirT, ownerT, Line, llCmdT)), // output type
    Alias#(infoT, CacheInfo#(tagT, Msi, dirT, ownerT)),
    Alias#(ramDataT, RamData#(tagT, Msi, dirT, ownerT, Line)),
    Alias#(respStateT, RespState#(Msi)),
    Alias#(tagMatchResT, TagMatchResult#(wayT)),
    // requirement 
    Alias#(indexT, Bit#(indexSz)),
    Alias#(tagT, Bit#(tagSz)),
    Alias#(cRqIdxT, Bit#(_cRqIdxSz)),
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
                dir: replicate(I),
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
            tagged CRs .r: r.addr;
            tagged MRs .r: r.addr;
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

            $display("%t LL %m tagMatch: ", $time, 
                fshow(cmd), " ; ", 
                fshow(getTag(getAddrFromCmd(cmd))), " ; ",
                fshow(tagVec), " ; ", 
                fshow(csVec), " ; ", 
                fshow(ownerVec)
            );
            if(cmd matches tagged MRs .rs) begin
                // MRs directly read from cmd
                return TagMatchResult {
                    way: rs.way,
                    pRqMiss: False
                };
            end
            else begin
                // CRq/CRs: need tag matching
                Addr addr = getAddrFromCmd(cmd);
                tagT tag = getTag(addr);
                // find hit way (we do not check replacing bit in LLC)
                // this makes <cRq a> blocked by other <cRq b> which is replacing addr a
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
                else begin
                    // cRs must hit, so only cRq cannot enter here
                    doAssert(cmd matches tagged CRq ._rq ? True : False,
                        "only cRq can tag match miss"
                    );
                    // find a unlocked way to replace for cRq
                    Vector#(wayNum, Bool) unlocked = ?;
                    Vector#(wayNum, Bool) invalid = ?;
                    for(Integer i = 0; i < valueOf(wayNum); i = i+1) begin
                        invalid[i] = csVec[i] == I;
                        unlocked[i] = !isValid(ownerVec[i]);
                    end
                    Maybe#(wayT) repWay = randRep.getReplaceWay(unlocked, invalid);
                    // sanity check: repWay must be valid
                    doAssert(isValid(repWay), "should always find a way to replace");
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
        if(cmd matches tagged CRs .cRs) begin
            dirT newDir = oldDir;
            newDir[cRs.child] = toState;
            return newDir;
        end
        else begin
            doAssert(False, "only cRs updates dir");
            return oldDir; // should not happen
        end
    endactionvalue
    endfunction

    function Action checkUpMRsDataValid(Msi cs, Bool dataV);
    action
        doAssert((cs == I) && dataV, ("LLC mRs always has data"));
    endaction
    endfunction

    function Action checkDownCRsDataValid(pipeCmdT cmd, dirT dir, Bool dataV);
    action
        doAssert(cmd matches tagged CRs .cRs &&& ((dir[cRs.child] == M) == dataV) ? True : False, 
            "cRs has data for downgrade from M"
        );
    endaction
    endfunction

    CCPipe#(wayNum, indexT, tagT, Msi, dirT, ownerT, Line, pipeCmdT) pipe <- mkCCPipe(
        regToReadOnly(initDone), getIndex, tagMatch, updateChildDir, 
        checkUpMRsDataValid, checkDownCRsDataValid,
        infoRam, dataRam
    );

    // get first output from CCPipe output
    function pipeOutT getFirst(PipeOut#(wayT, tagT, Msi, dirT, ownerT, Line, pipeCmdT) pout);
        return PipeOut {
            cmd: (case(pout.cmd) matches
                tagged CRq .rq: LLCRq (rq.mshrIdx);
                tagged CRs .rs: LLCRs (rs.child);
                tagged MRs .rs: LLMRs;
                default: ?;
            endcase),
            way: pout.way,
            pRqMiss: pout.pRqMiss,
            ram: pout.ram
        };
    endfunction

    method Action send(pipeInT req);
        case(req) matches
            tagged CRq .rq: begin
                pipe.enq(CRq (rq), Invalid, Invalid);
            end
            tagged CRs .rs: begin
                pipe.enq(CRs (LLPipeCRsCmd {
                    addr: rs.addr,
                    child: rs.child
                }), rs.data, DownDir (rs.toState));
            end
            tagged MRs .rs: begin
                pipe.enq(MRs (LLPipeMRsCmd {
                    addr: rs.addr,
                    way: rs.way
                }), Valid (rs.data), UpCs (rs.toState));
            end
        endcase
    endmethod

    // need to adapt pipeline output to real output format
    method pipeOutT first;
        return getFirst(pipe.first); // guarded version
    endmethod

    method pipeOutT unguard_first;
        return getFirst(pipe.unguard_first); // unguarded version
    endmethod

    method notEmpty = pipe.notEmpty;

    method Action deqWrite(Maybe#(cRqIdxT) swapRq, ramDataT wrRam);
        // get new cmd
        Addr addr = getAddrFromCmd(pipe.first.cmd); // inherit addr
        Maybe#(pipeCmdT) newCmd = Invalid;
        if(swapRq matches tagged Valid .idx) begin
            newCmd = Valid (CRq (LLPipeCRqIn {addr: addr, mshrIdx: idx}));
        end
        // call pipe
        pipe.deqWrite(newCmd, wrRam);
    endmethod
endmodule
