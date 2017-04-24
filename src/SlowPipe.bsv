
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

import Ehr::*;
import Fifo::*;
import Vector::*;
import RWBramCore::*;
import FShow::*;
import CCTypes::*;

// general type param ordering: way < index < tag < msi < dir < owner < line < pipeCmd
typedef union tagged {
    void Invalid;
    msiT DownDir; // cRs downgraded toState
    msiT UpCs; // pRs upgraded toState
} RespState#(type msiT) deriving(Bits, Eq, FShow);

typedef struct {
    pipeCmdT cmd;
    // tag match & ram output
    wayT way;
    Bool pRqMiss; // pRq miss, valid only if go through tag match
    RamData#(tagT, msiT, dirT, ownerT, lineT) ram;
} PipeOut#(type wayT, type tagT, type msiT, type dirT, type ownerT, type lineT, type pipeCmdT) deriving(Bits, Eq, FShow);

interface CCPipe#(
    numeric type wayNum,
    type indexT,
    type tagT,
    type msiT,
    type dirT,
    type ownerT,
    type lineT,
    type pipeCmdT
);
    method Action enq(pipeCmdT cmd, Maybe#(lineT) respLine, RespState#(msiT) toState);
    method Bool notFull;
    method PipeOut#(Bit#(TLog#(wayNum)), tagT, msiT, dirT, ownerT, lineT, pipeCmdT) first;
    method PipeOut#(Bit#(TLog#(wayNum)), tagT, msiT, dirT, ownerT, lineT, pipeCmdT) unguard_first;
    method Bool notEmpty;
    method Action deqWrite(Maybe#(pipeCmdT) newCmd, RamData#(tagT, msiT, dirT, ownerT, lineT) wrRam);
endinterface

// input data type (temporarily buffered in bypass FIFO)
typedef struct {
    pipeCmdT cmd;
    Maybe#(lineT) respLine;
    RespState#(msiT) toState;
} InputData#(type msiT, type lineT, type pipeCmdT) deriving(Bits, Eq);

// internal pipeline reg types
// three stages
// 1: enq
// 2: tag match, read data and dir
// 3: output

typedef struct {
    pipeCmdT cmd;
    // bypasses
    Vector#(wayNum, Maybe#(CacheInfo#(tagT, msiT, dirT, ownerT))) infoVec;
    // CRs/PRs info
    Maybe#(lineT) respLine;
    RespState#(msiT) toState;
} Enq2Match#(numeric type wayNum, type tagT, type msiT, type dirT, type ownerT, type lineT, type pipeCmdT) deriving(Bits, Eq);

typedef struct {
    pipeCmdT cmd;
    // tag match results
    wayT way;
    Bool pRqMiss;
    // RAM outputs
    // cs is merged with PRs toState
    // dir is merged with CRs toState
    CacheInfo#(tagT, msiT, dirT, ownerT) info;
    // bypassed or resp line
    Maybe#(lineT) line;
} Match2Out#(type wayT, type tagT, type msiT, type dirT, type ownerT, type lineT, type pipeCmdT) deriving(Bits, Eq);

typedef struct {
    indexT index;
    wayT way;
    RamData#(tagT, msiT, dirT, ownerT, lineT) ram; // data to write into RAM
} BypassInfo#(type wayT, type indexT, type tagT, type msiT, type dirT, type lineT, type ownerT) deriving(Bits, Eq);

typedef struct {
    wayT way;
    Bool pRqMiss;
} TagMatchResult#(type wayT) deriving(Bits, Eq);

module mkCCPipe#(
    Bool doBypass, // whether we do bypass or not
    ReadOnly#(Bool) initDone,
    function indexT getIndex(pipeCmdT cmd),
    function ActionValue#(TagMatchResult#(wayT)) tagMatch(
        // actionvalue enable us to do checking inside the function
        pipeCmdT cmd, 
        // below are current RAM outputs, is merged with ram write from final stage
        // but is NOT merged with state changes carried in PRs/CRs 
        Vector#(wayNum, tagT) tagVec, 
        Vector#(wayNum, msiT) csVec, 
        Vector#(wayNum, ownerT) ownerVec
    ),
    function ActionValue#(dirT) updateChildDir(pipeCmdT cmd, msiT toState, dirT oldDir), // get new dir for CRs
    function Action checkUpPRsDataValid(msiT cs, Bool dataValid),
    function Action checkDownCRsDataValid(pipeCmdT cmd, dirT dir, Bool dataValid)
)(
    Vector#(wayNum, RWBramCore#(indexT, infoT)) infoRam,
    Vector#(wayNum, RWBramCore#(indexT, lineT)) dataRam,
    CCPipe#(wayNum, indexT, tagT, msiT, dirT, ownerT, lineT, pipeCmdT) ifc
) provisos (
    Alias#(wayT, Bit#(TLog#(wayNum))),
    Alias#(infoT, CacheInfo#(tagT, msiT, dirT, ownerT)),
    Alias#(ramDataT, RamData#(tagT, msiT, dirT, ownerT, lineT)),
    Alias#(respStateT, RespState#(msiT)),
    Alias#(pipeOutT, PipeOut#(wayT, tagT, msiT, dirT, ownerT, lineT, pipeCmdT)),
    Alias#(inputDataT, InputData#(msiT, lineT, pipeCmdT)),
    Alias#(enq2MatchT, Enq2Match#(wayNum, tagT, msiT, dirT, ownerT, lineT, pipeCmdT)),
    Alias#(match2OutT, Match2Out#(wayT, tagT, msiT, dirT, ownerT, lineT, pipeCmdT)),
    Alias#(bypassInfoT, BypassInfo#(wayT, indexT, tagT, msiT, dirT, lineT, ownerT)),
    Bits#(indexT, _indexSz),
    Bits#(tagT, _tagSz),
    Bits#(msiT, _msiSz),
    Bits#(dirT, _dirSz),
    Bits#(ownerT, _ownerSz),
    Bits#(lineT, _lineSz),
    Bits#(pipeCmdT, _pipeCmdSz),
    Eq#(indexT)
);

    // input bypass fifo: make enq & deq conflict free
    Fifo#(1, inputDataT) inputQ <- mkBypassFifo;

    // pipeline regs

    Ehr#(3, Maybe#(enq2MatchT)) enq2Mat <- mkEhr(Invalid);
    // port 0: bypass
    Reg#(Maybe#(enq2MatchT)) enq2Mat_bypass = enq2Mat[0];
    // port 1: tag match
    Reg#(Maybe#(enq2MatchT)) enq2Mat_match = enq2Mat[1];
    // port 2: enq
    Reg#(Maybe#(enq2MatchT)) enq2Mat_enq = enq2Mat[2];

    Ehr#(2, Maybe#(match2OutT)) mat2Out <- mkEhr(Invalid);
    // port 0: out
    Reg#(Maybe#(match2OutT)) mat2Out_out = mat2Out[0];
    // port 1: tag match
    Reg#(Maybe#(match2OutT)) mat2Out_match = mat2Out[1];

    // bypass write to ram
    RWire#(bypassInfoT) bypass <- mkRWire;

    // in case no bypass, we check whether we stall for same index 
    Bool stallSameIndex = False;
    if(!doBypass) begin
        // we check EHR port 0 of pipeline EHRs
        // because effect of write ram may not be visible to read ram in same cycle
        // In case of swap in the mat2out stage, the swapped in req are for the same index
        Wire#(Maybe#(indexT)) enq2MatIndex <- mkBypassWire;
        Wire#(Maybe#(indexT)) mat2OutIndex <- mkBypassWire;

        (* fire_when_enabled, no_implicit_conditions *)
        rule setIndexWires;
            if(enq2Mat[0] matches tagged Valid .e2m) begin
                enq2MatIndex <= Valid (getIndex(e2m.cmd));
            end
            else begin
                enq2MatIndex <= Invalid;
            end
            if(mat2Out[0] matches tagged Valid .m2o) begin
                mat2OutIndex <= Valid (getIndex(m2o.cmd));
            end
            else begin
                mat2OutIndex <= Invalid;
            end
        endrule

        indexT newIndex = getIndex(inputQ.first.cmd);
        if(enq2MatIndex matches tagged Valid .idx &&& idx == newIndex) begin
            stallSameIndex = True;
        end
        if(mat2OutIndex matches tagged Valid .idx &&& idx == newIndex) begin
            stallSameIndex = True;
        end
    end

    // stage 1: enq req to pipeline: access info RAM & bypass
    rule doEnq(!isValid(enq2Mat_enq) && initDone && !stallSameIndex);
        inputQ.deq;
        inputDataT in = inputQ.first;
        // read ram
        indexT index = getIndex(in.cmd);
        for(Integer i = 0; i < valueOf(wayNum); i = i+1) begin
            infoRam[i].rdReq(index);
        end
        // write reg & get bypass
        enq2MatchT e2m = Enq2Match {
            cmd: in.cmd,
            infoVec: replicate(Invalid),
            respLine: in.respLine,
            toState: in.toState
        };
        if(doBypass) begin
            if(bypass.wget matches tagged Valid .b &&& b.index == index) begin
                e2m.infoVec[b.way] = Valid (b.ram.info);
            end
        end
        enq2Mat_enq <= Valid (e2m);
    endrule

    // stage 2
    // first get bypass
    if(doBypass) begin
        (* fire_when_enabled, no_implicit_conditions *)
        rule doMatch_bypass(isValid(bypass.wget) && isValid(enq2Mat_bypass) && initDone);
            bypassInfoT b = fromMaybe(?, bypass.wget);
            enq2MatchT e2m = fromMaybe(?, enq2Mat_bypass);
            if(b.index == getIndex(e2m.cmd)) begin
                e2m.infoVec[b.way] = Valid (b.ram.info);
            end
            enq2Mat_bypass <= Valid (e2m);
        endrule
    end

    rule doTagMatch(isValid(enq2Mat_match) && !isValid(mat2Out_match) && initDone);
        enq2MatchT e2m = fromMaybe(?, enq2Mat_match);
        // get cache output & merge with bypass
        Vector#(wayNum, infoT) infoVec = ?;
        for(Integer i = 0; i < valueOf(wayNum); i = i+1) begin
            infoRam[i].deqRdResp;
            infoVec[i] = fromMaybe(infoRam[i].rdResp, e2m.infoVec[i]);
        end
        // do tag match to get way to occupy
        Vector#(wayNum, tagT) tagVec = ?;
        Vector#(wayNum, msiT) csVec = ?;
        Vector#(wayNum, ownerT) ownerVec = ?;
        for(Integer i = 0; i < valueOf(wayNum); i = i+1) begin
            tagVec[i] = infoVec[i].tag;
            csVec[i] = infoVec[i].cs;
            ownerVec[i] = infoVec[i].owner;
        end
        let tmRes <- tagMatch(e2m.cmd, tagVec, csVec, ownerVec);
        wayT way = tmRes.way;
        Bool pRqMiss = tmRes.pRqMiss;
        // read data
        indexT index = getIndex(e2m.cmd);
        dataRam[way].rdReq(index);
        // set mat2out & merge with CRs/PRs & merge with bypass
        // resp info has higher priority than bypass
        match2OutT m2o = Match2Out {
            cmd: e2m.cmd,
            way: way,
            pRqMiss: pRqMiss,
            info: infoVec[way],
            line: e2m.respLine
        };
        if(e2m.toState matches tagged UpCs .s) begin
            checkUpPRsDataValid(m2o.info.cs, isValid(e2m.respLine)); // sanity check on data
            m2o.info.cs = s;
        end
        if(e2m.toState matches tagged DownDir .s) begin
            checkDownCRsDataValid(m2o.cmd, m2o.info.dir, isValid(e2m.respLine)); // sanity check on data
            m2o.info.dir <- updateChildDir(m2o.cmd, s, m2o.info.dir);
        end
        if(doBypass) begin
            if(bypass.wget matches tagged Valid .b &&& b.index == index &&& b.way == way &&& !isValid(m2o.line)) begin
                // bypass has lower priority than resp data
                m2o.line = Valid (b.ram.line);
            end
        end
        mat2Out_match <= Valid (m2o);
        // reset enq2mat
        enq2Mat_match <= Invalid;
    endrule

    // construct output with bypass/resp data
    function pipeOutT firstOut;
        match2OutT m2o = fromMaybe(?, mat2Out_out);
        return PipeOut {
            cmd: m2o.cmd,
            way: m2o.way,
            pRqMiss: m2o.pRqMiss,
            ram: RamData {
                info: m2o.info,
                line: fromMaybe(dataRam[m2o.way].rdResp, m2o.line)
            }
        };
    endfunction

    method Action enq(pipeCmdT cmd, Maybe#(lineT) respLine, respStateT toState);
        inputQ.enq(InputData {
            cmd: cmd,
            respLine: respLine,
            toState: toState
        });
    endmethod

    method Bool notFull = inputQ.notFull;

    method pipeOutT first if(isValid(mat2Out_out) && initDone);
        return firstOut;
    endmethod

    method pipeOutT unguard_first;
        return firstOut;
    endmethod

    method Bool notEmpty = isValid(mat2Out_out) && initDone;

    method Action deqWrite(Maybe#(pipeCmdT) newCmd, ramDataT wrRam) if(isValid(mat2Out_out) && initDone);
        match2OutT m2o = fromMaybe(?, mat2Out_out);
        wayT way = m2o.way;
        indexT index = getIndex(m2o.cmd);
        // write ram
        infoRam[way].wrReq(index, wrRam.info);
        dataRam[way].wrReq(index, wrRam.line);
        // set bypass to Enq and Match stages
        if(doBypass) begin
            bypass.wset(BypassInfo {
                index: index,
                way: way,
                ram: wrRam
            });
        end
        // change pipeline reg
        if(newCmd matches tagged Valid .cmd) begin
            // update pipeline reg
            m2o.pRqMiss = False;
            mat2Out_out <= Valid (Match2Out {
                cmd: cmd, // swapped in new cmd
                way: m2o.way, // keep way same
                pRqMiss: False, // reset (not valid for swapped in pRq)
                info: wrRam.info, // get bypass
                line: Valid (wrRam.line) // get bypass
            });
        end
        else begin
            // XXX deq ram resp, I think this should not block
            dataRam[way].deqRdResp;
            // reset pipeline reg
            mat2Out_out <= Invalid;
        end
    endmethod
endmodule
