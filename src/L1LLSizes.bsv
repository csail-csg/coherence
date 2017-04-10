import Vector::*;

// cache size = bank num * way num * set num * 64B

// L1 connect to LL

// 32KB L1
typedef 8 L1DNum;
typedef 0 L1INum;
typedef TAdd#(L1DNum, L1INum) L1Num;

typedef 4 L1WayNum;
typedef Bit#(TLog#(L1WayNum)) L1Way;

typedef 1 LgL1BankNum;
typedef 6 LgL1SetNum;
typedef TExp#(LgL1BankNum) L1BankNum;
typedef Bit#(LgL1BankNum) L1BankId;
typedef LgL1SetNum L1IndexSz;
typedef Bit#(L1IndexSz) L1Index;
typedef GetTagSz#(LgL1BankNum, LgL1SetNum) L1TagSz;
typedef Bit#(L1TagSz) L1Tag;

typedef 0 LgIBankNum;
typedef 7 LgISetNum;
typedef Bit#(LgIBankNum) IBankId;
typedef LgISetNum IIndexSz;
typedef Bit#(IIndexSz) IIndex;
typedef GetTagSz#(LgIBankNum, LgISetNum) ITagSz;
typedef Bit#(ITagSz) ITag;

typedef 4 L1CRqNum;
typedef 2 L1PRqNum;
typedef Bit#(TLog#(L1CRqNum)) L1CRqMshrIdx;
typedef Bit#(TLog#(L1PRqNum)) L1PRqMshrIdx;

typedef Bit#(32) ProcRqId;

typedef 4 L1ISupSz;
typedef Vector#(L1ISupSz, Maybe#(Instruction)) L1InstResult;

// Last-Level: 512KB per bank
typedef 16 LLWayNum;
typedef 0 LgLLBankNum;
typedef 9 LgLLSetNum;

typedef Bit#(LgLLBankNum) LLBankId;
typedef LgLLSetNum LLIndexSz;
typedef Bit#(LLIndexSz) LLIndex;
typedef GetTagSz#(LgLLBankNum, LgLLSetNum) LLTagSz;
typedef Bit#(LLTagSz) LLTag;
typedef Bit#(TLog#(LLWayNum)) LLWay;

typedef 16 LLCRqNum;
typedef Bit#(TLog#(LLCRqNum)) LLCRqMshrIdx;

typedef L1Num LLChildNum;
typedef Bit#(TLog#(LLChildNum)) LLChild;
typedef L1Way LLCRqId;

typedef Bit#(32) DmaRqId;

