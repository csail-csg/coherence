
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
import Connectable::*;
import CCTypes::*;
import Types::*;

// some cross bar utils

// for children to parent cross bar (parent to child is just reversing src/dst):
// if lgCBank > lgPBank then
//    srcNum = 2^(lgCBank - lgPBank) * cNum
//    dstNum = 1
//    xbarNum = 2^lgPBank
// if lgCBank < lgPBank then
//    srcNum = cNum
//    dstNum = 2^(lgPBank - lgCBank)
//    xbarNum = 2^lgCBank

// number of child endpoint in each xbar
typedef TMul#(childNum, TDiv#(TExp#(lgChildBank), TExp#(lgParentBank)))
    GetChildEPNum#(
        numeric type childNum, 
        numeric type lgChildBank,
        numeric type lgParentBank
    );
// number of child endpoint in each xbar
typedef TDiv#(TExp#(lgParentBank), TExp#(lgChildBank))
    GetParentEPNum#(
        numeric type lgChildBank,
        numeric type lgParentBank
    );
// number of xbars
typedef TExp#(TMin#(lgChildBank, lgParentBank))
    GetXBarNum#(
        numeric type lgChildBank,
        numeric type lgParentBank
    );

// connect XBar with child and parent ports
// id of xbar for child/parent bank = truncate(cBankId or pBankId) = c/pBankId % number of xbar
// port for a child in one xbar = floor(cBankId / 2^lgPBank) + childId * ceil(2^(lgCBank - lgPBank))
// port for a parent in one xbar = floor(pBankId / 2^lgCBank)
module mkConnXBar#(
    Vector#(childNum, Vector#(childBankNum, c2xIfcT)) child,
    Vector#(parentBankNum, p2xIfcT) parent,
    Vector#(xbarNum, Vector#(childEPNum, x2cIfcT)) x2c, // cross bar to child
    Vector#(xbarNum, Vector#(parentEPNum, x2pIfcT)) x2p // cross bar to parent
)(Empty) provisos (
    NumAlias#(childBankNum, TExp#(lgChildBank)),
    NumAlias#(parentBankNum, TExp#(lgParentBank)),
    NumAlias#(childEPNum, GetChildEPNum#(childNum, lgChildBank, lgParentBank)),
    NumAlias#(parentEPNum, GetParentEPNum#(lgChildBank, lgParentBank)),
    NumAlias#(xbarNum, GetXBarNum#(lgChildBank, lgParentBank)),
    NumAlias#(perChildEPNum, TDiv#(childEPNum, childNum)),
    Connectable#(c2xIfcT, x2cIfcT),
    Connectable#(p2xIfcT, x2pIfcT)
);
    // connect child with xbar
    for(Integer i = 0; i < valueOf(childNum); i = i+1) begin
        for(Integer j = 0; j < valueOf(childBankNum); j = j+1) begin
            Integer whichXbar = j % valueOf(xbarNum);
            Integer whichEP = (j >> valueOf(lgParentBank)) + i * valueOf(perChildEPNum);
            mkConnection(child[i][j], x2c[whichXBar][whichEP]);
        end
    end

    // connect parent with xbar
    for(Integer i = 0; i < valueOf(parentNum); i = i+1) begin
        Integer whichXBar = i % valueOf(xbarNum);
        Integer whichEP = i >> valueOf(lgChildBank);
        mkConnection(parent[i], x2p[whichXBar][whichEP]);
    end
endmodule

// XXX under the assumption of using the above mkConnXBar
// child to parent router that gives dst parent using child msg addr
// dst port = getPBankId(addr) >> lgCBank
interface C2PRouter#(
    numeric type childNum,
    numeric type lgChildBank,
    numeric type lgParentBank
);
    method Bit#(TLog#(GetParentEPNum#(lgChildBank, lgParentBank))) getDst(Addr a);
    method Bit#(TLog#(childNum)) getChild(Bit#(TLog#(GetChildEPNum#(childNum, lgChildBank, lgParentBank))) childEP);
endinterface

module mkC2PRouter(C2PRouter#(lgChildBank, lgParentBank)) provisos(
    NumAlias#(lgParentEP, TLog#(GetParentEPNum#(lgChildBank, lgParentBank))),
    NumAlias#(lgChildEP, TLog#(GetChildEPNum#(childNum, lgChildBank, lgParentBank))),
    Add#(lgParentBank, a__, AddrSz),
    Add#(lgParentEP, b__, lgParentBank),
    Add#(TLog#(childNum), c__, lgChildEP)
);
    method Bit#(lgParentEP) getDst(Addr addr);
        Bit#(lgParentBank) pBankId = truncate(addr >> valueOf(LgLineSzBytes));
        return truncate(pBankId >> valueOf(lgChildBank));
    endmethod

    method Bit#(TLog#(childNum)) getChild(Bit#(lgChildEP) cEP);
        return truncateLSB(cEP);
    endmethod
endmodule

// XXX under the assumption of using the above mkConnXBar
// parent to child router that gives dst child using parent msg addr
// dst port = (getCBankId(addr) >> min(lgParentBank, lgChildBank)) + childId * ceil(2^(lgChildBank - lgParentBank))
interface P2CRouter#(
    numeric type childNum,
    numeric type lgChildBank,
    numeric type lgParentBank
);
    method Bit#(TLog#(GetChildEPNum#(childNum, lgChildBank, lgParentBank))) getDst(Addr a, Bit#(TLog#(childNum)) c);
endinterface

module mkP2CRouter(C2PRouter#(childNum, lgChildBank, lgParentBank)) provisos(
    NumAlias#(lgChildEP, TLog#(GetChildEPNum#(childNum, lgChildBank, lgParentBank))),
    // max(0, lgChildBank - lgParentBank) = lg(ceil(2^lgChildBank - lgParentBank))
    NumAlias#(lgPerChildEP, TLog#(TDiv#(TExp#(lgChildBank), TExp#(lgParentBank)))), 
    Alias#(childT, Bit#(TLog#(childNum))),
    Add#(lgChildBank, a__, AddrSz),
    Add#(lgPerChildEP, b__, lgChildEP)
);
    method Bit#(lgChildEP) getDst(Addr a, childT c);
        Bit#(lgChildBank) cBankId = truncate(a >> valueOf(LdLineSzBytes));
        Bit#(lgPerChildEP) offset = truncateLSB(cBankId);
        return zeroExtend(offset) + (zeroExtend(c) << valueOf(lgPerChildEPNum));
    endmethod
endmodule
