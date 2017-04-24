
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
    rqIdxT mshrIdx;
} IntPipeRqIn#(type rqIdxT) deriving(Bits, Eq, FShow);

typedef struct {
    Addr addr;
    Msi toState;
    Maybe#(Line) data;
    wayT way;
} IntPipePRsIn#(type wayT) deriving(Bits, Eq, FShow);

typedef union tagged {
    IntPipeRqIn#(cRqIdxT) CRq;
    IntPipeRqIn#(pRqIdxT) PRq;
    CRsMsg#(childT) CRs;
    IntPipePRsIn#(wayT) PRs;
} IntPipeIn#(
    type childT,
    type wayT,
    type cRqIdxT, 
    type pRqIdxT
) deriving (Bits, Eq, FShow);

// output cmd to the processing rule in Int$
typedef union tagged {
    cRqIdxT IntCRq;
    pRqIdxT IntPRq;
    childT IntCRs;
    void IntPRs;
} IntCmd#(
    type childT,
    type cRqIdxT, 
    type pRqIdxT
) deriving (Bits, Eq, FShow);

interface IntPipe#(
    numeric type lgBankNum,
    numeric type childNum,
    numeric type wayNum,
    type indexT,
    type tagT,
    type cRqIdxT, 
    type pRqIdxT
);
    method Action send(IntPipeIn#(Bit#(TLog#(childNum)), Bit#(TLog#(wayNum)), cRqIdxT, pRqIdxT) r);
    method PipeOut#(
        Bit#(TLog#(wayNum)), 
        tagT, Msi, Vector#(childNum, Msi),
        CacheOwner#(cRqIdxT, pRqIdxT),
        Line, IntCmd#(Bit#(TLog#(childNum)), cRqIdxT, pRqIdxT)
    ) first;
    method Action deqWrite(
        Maybe#(MshrIndex#(cRqIdxT, pRqIdxT)) swapRq,
        RamData#(tagT, Msi, Vector#(childNum, Msi), CacheOwner#(cRqIdxT, pRqIdxT), Line) wrRam // always write BRAM
    );
endinterface

// real cmd used in pipeline
typedef struct {
    Addr addr;
    wayT way;
} IntPipePRsCmd#(type wayT) deriving(Bits, Eq, FShow);

typedef struct {
    Addr addr;
    childT child;
} IntPipeCRsCmd#(type childT) deriving(Bits, Eq, FShow);

typedef union tagged {
    IntPipeRqIn#(cRqIdxT) CRq;
    IntPipeRqIn#(pRqIdxT) PRq;
    IntPipeCRsCmd#(childT) CRs;
    IntPipePRsCmd#(wayT) PRs;
} IntPipeCmd#(
    type childT,
    type wayT,
    type cRqIdxT, 
    type pRqIdxT
) deriving (Bits, Eq, FShow);

module mkIntPipe(
    IntPipe#(lgBankNum, childNum, wayNum, indexT, tagT, cRqIdxT, pRqIdxT)
) provisos(
    Alias#(childT, Bit#(TLog#(childNum))),
    Alias#(wayT, Bit#(TLog#(wayNum))),
    Alias#(dirT, Vector#(childNum, Msi)),
    Alias#(ownerT, CacheOwner#(cRqIdxT, pRqIdxT)),
    Alias#(pipeInT, IntPipeIn#(childT, wayT, cRqIdxT, pRqIdxT)),
    Alias#(pipeCmdT, IntPipeCmd#(childT, wayT, cRqIdxT, pRqIdxT)),
    Alias#(intCmdT, IntCmd#(childT, cRqIdxT, pRqIdxT)),
    Alias#(pipeOutT, PipeOut#(wayT, tagT, Msi, dirT, ownerT, Line, intCmdT)), // output type
    Alias#(infoT, CacheInfo#(tagT, Msi, dirT, ownerT)),
    Alias#(ramDataT, RamData#(tagT, Msi, dirT, ownerT, Line)),
    Alias#(respStateT, RespState#(Msi)),
    Alias#(mshrIndexT, MshrIndex#(cRqIdxT, pRqIdxT)),
    Alias#(tagMatchResT, TagMatchResult#(wayT)),
    // requirement 
    Alias#(indexT, Bit#(indexSz)),
    Alias#(tagT, Bit#(tagSz)),
    Alias#(cRqIdxT, Bit#(_cRqIdxSz)),
    Alias#(pRqIdxT, Bit#(_pRqIdxSz)),
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
            tagged PRq .r: r.addr;
            tagged CRs .r: r.addr;
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

            $display("%t Int %m tagMatch: ", $time, 
                fshow(cmd), " ; ", 
                fshow(getTag(getAddrFromCmd(cmd))), " ; ",
                fshow(tagVec), " ; ", 
                fshow(csVec), " ; ", 
                fshow(ownerVec)
            );
            if(cmd matches tagged PRs .rs) begin
                // PRs directly read from cmd
                return TagMatchResult {
                    way: rs.way,
                    pRqMiss: False
                };
            end
            else if(cmd matches tagged CRs .rs) begin
                // CRs: simple tag match (no need to check owner)
                tagT tag = getTag(rs.addr);
                // find hit way
                function Bool isMatch(Integer i);
                    return csVec[i] > I && tagVec[i] == tag;
                endfunction
                Vector#(wayNum, Integer) idxVec = genVector;
                Maybe#(wayT) hitWay = searchIndex(isMatch, idxVec);
                doAssert(isValid(hitWay), "cRs must hit");
                return TagMatchResult {
                    way: fromMaybe(?, hitWay),
                    pRqMiss: False
                };
            end
            else begin
                // CRq/PRq: need tag matching
                Addr addr = getAddrFromCmd(cmd);
                tagT tag = getTag(addr);
                // find hit way
                // XXX cannot hit on way which is being replaced by other cRq
                // because in this case cRq could find another way to replace
                // and pRq is acutally a miss (line will be replaced anyway)
                // XXX we could hit on way owned by pRq or cRq (which is not replacing)
                // this simplifies the analysis in IntBank.bsv
                function Bool isMatch(Integer i);
                    Bool tagCs = csVec[i] > I && tagVec[i] == tag;
                    Bool noRep = (case(ownerVec[i]) matches
                        tagged CRq .cOwner: return !cOwner.replacing;
                        default: return True;
                    endcase);
                    return tagCs && noRep;
                endfunction
                Vector#(wayNum, Integer) idxVec = genVector;
                Maybe#(wayT) hitWay = searchIndex(isMatch, idxVec);
                if(hitWay matches tagged Valid .w) begin
                    return TagMatchResult {
                        way: w,
                        pRqMiss: False
                    };
                end
                else begin
                    if(cmd matches tagged PRq .rq) begin
                        // pRq miss
                        return TagMatchResult {
                            way: 0, // default to 0
                            pRqMiss: True
                        };
                    end
                    else begin
                        // cRq miss: find a way to replace, the way should be:
                        // 1. not occupied by any cRq
                        // 2. not occupied by any pRq with successor
                        Vector#(wayNum, Bool) unlocked = ?;
                        Vector#(wayNum, Bool) invalid = ?;
                        for(Integer i = 0; i < valueOf(wayNum); i = i+1) begin
                            invalid[i] = csVec[i] == I;
                            unlocked[i] = (case(ownerVec[i]) matches
                                tagged Invalid: True;
                                tagged CRq .rq: False;
                                tagged PRq .rq: !rq.hasSucc;
                                default: False; // impossible case
                            endcase);
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

    function Action checkUpPRsDataValid(Msi cs, Bool dataV);
    action
        doAssert((cs == I) == dataV, ("valid resp data for upgrade from I"));
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
            tagged CRs .rs: begin
                pipe.enq(CRs (IntPipeCRsCmd {
                    addr: rs.addr,
                    child: rs.child
                }), rs.data, DownDir (rs.toState));
            end
            tagged PRs .rs: begin
                pipe.enq(PRs (IntPipePRsCmd {
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
                tagged CRq .rq: IntCRq (rq.mshrIdx);
                tagged PRq .rq: IntPRq (rq.mshrIdx);
                tagged CRs .rs: IntCRs (rs.child);
                tagged PRs .rs: IntPRs;
                default: ?;
            endcase),
            way: pout.way,
            pRqMiss: pout.pRqMiss,
            ram: pout.ram
        };
    endmethod

    method Action deqWrite(Maybe#(mshrIndexT) swapRq, ramDataT wrRam);
        // get new cmd
        Addr addr = getAddrFromCmd(pipe.first.cmd); // inherit addr
        Maybe#(pipeCmdT) newCmd = Invalid;
        if(swapRq matches tagged Valid .mshrIdx) begin
            newCmd = Valid (case(mshrIdx) matches
                tagged CRq .idx: CRq (IntPipeRqIn {addr: addr, mshrIdx: idx});
                tagged PRq .idx: PRq (IntPipeRqIn {addr: addr, mshrIdx: idx});
                default: ?; // impossible
            endcase);
        end
        // call pipe
        pipe.deqWrite(newCmd, wrRam);
    endmethod
endmodule
