
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

// L1 -> L2 -> LL

// cache size = bank num * way num * set num * 64B

// 16KB L1
typedef 16 L1Num;

typedef 4 L1WayNum;
typedef 0 LgL1BankNum;
typedef 6 LgL1SetNum;

typedef Bit#(LgL1BankNum) L1BankId;
typedef LgL1SetNum L1IndexSz;
typedef Bit#(L1IndexSz) L1Index;
typedef GetTagSz#(LgL1BankNum, LgL1SetNum) L1TagSz;
typedef Bit#(L1TagSz) L1Tag;
typedef Bit#(TLog#(L1WayNum)) L1Way;

typedef 4 L1CRqNum;
typedef 2 L1PRqNum;
typedef Bit#(TLog#(L1CRqNum)) L1CRqMshrIdx;
typedef Bit#(TLog#(L1PRqNum)) L1PRqMshrIdx;

typedef Bit#(32) ProcRqId;

// 128KB L2
typedef 4 L2Num;

typedef 8 L2WayNum;
typedef 0 LgL2BankNum;
typedef 8 LgL2SetNum;

typedef Bit#(LgL2BankNum) L2BankId;
typedef LgL2SetNum L2IndexSz;
typedef Bit#(LgL2SetNum) L2Index;
typedef GetTagSz#(LgL2BankNum, LgL2SetNum) L2TagSz;
typedef Bit#(L2TagSz) L2Tag;
typedef Bit#(TLog#(L2WayNum)) L2Way;

typedef 8 L2CRqNum;
typedef 4 L2PRqNum;
typedef Bit#(TLog#(L2CRqNum)) L2CRqMshrIdx;
typedef Bit#(TLog#(L2PRqNum)) L2PRqMshrIdx;

typedef TDiv#(L1Num, L2Num) L2ChildNum;
typedef Bit#(TLog#(L2ChildNum)) L2Child;
typedef L1Way L2CRqId;

// Last-Level: 1MB per bank
typedef 16 LLWayNum;
typedef 0 LgLLBankNum;
typedef 10 LgLLSetNum;

typedef Bit#(LgLLBankNum) LLBankId;
typedef LgLLSetNum LLIndexSz;
typedef Bit#(LLIndexSz) LLIndex;
typedef GetTagSz#(LgLLBankNum, LgLLSetNum) LLTagSz;
typedef Bit#(LLTagSz) LLTag;
typedef Bit#(TLog#(LLWayNum)) LLWay;

typedef 16 LLCRqNum;
typedef Bit#(TLog#(LLCRqNum)) LLCRqMshrIdx;

typedef L2Num LLChildNum;
typedef Bit#(TLog#(LLChildNum)) LLChild;
typedef L2Way LLCRqId;

typedef Bit#(0) DmaRqId; // FIXME: dma id is Bit#(0)

