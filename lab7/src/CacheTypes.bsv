import Types::*;
import Vector::*;

/* =============== MODIFY HERE FOR SIMULATION =============== */
// When you are using direct mapped cache, LinesPerSet must be 1.
typedef 1 WordsPerBlock;  // You can change this to 1, 2, 4, 8.
typedef 1 LinesPerSet;   // You can change this to 1, 2, 4, 8, 16, 32.
/* ========================================================== */


/* DO NOT MODIFY BELOW HERE! */
typedef 256 TotalCacheSize;  // unit: Word
typedef Vector#(WordsPerBlock, Data) Line;
typedef Line MemResp;

typedef Bit#(TLog#(WordsPerBlock)) BlockOffset;
typedef Bit#(TLog#(LinesPerSet)) SetOffset;

typedef TDiv#(TDiv#(TotalCacheSize, LinesPerSet), WordsPerBlock) NumOfSets;
typedef Bit#(TLog#(NumOfSets)) CacheIndex;
typedef Bit#(TSub#(TSub#(TSub#(AddrSz, TLog#(NumOfSets)),SizeOf#(BlockOffset)),3)) CacheTag;
