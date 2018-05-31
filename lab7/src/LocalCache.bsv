import Types::*;
import MemTypes::*;
import CacheTypes::*;
import Fifo::*;
import RegFile::*;
import Vector::*;

interface Cache;
	method Action req(MemReq r);
	method ActionValue#(Data) resp;

 	method ActionValue#(CacheMemReq) memReq;
 	method Action memResp(Line r);

	method Data getMissCnt;
	method Data getTotalReq;
endinterface

typedef enum {Ready, StartMiss, SendFillReq, WaitFillResp} CacheStatus deriving (Bits, Eq);

module mkCacheDirectMap(Cache);
	RegFile#(CacheIndex, Line) 				  dataArray	<- mkRegFileFull;
	RegFile#(CacheIndex, Maybe#(CacheTag))	   tagArray	<- mkRegFileFull;
	RegFile#(CacheIndex, Bool) 				 dirtyArray <- mkRegFileFull;

	Reg#(Bit#(TAdd#(SizeOf#(CacheIndex), 1))) 	init   <- mkReg(0);
	Reg#(CacheStatus) 							status <- mkReg(Ready);

	Fifo#(1, Data) 		hitQ <- mkBypassFifo;
	Reg#(MemReq)	 missReq <- mkRegU;

	Fifo#(2, CacheMemReq) memReqQ <- mkCFFifo;
	Fifo#(2, Line) 		 memRespQ <- mkCFFifo;

	Reg#(Data) missCnt <- mkReg(0);
	Reg#(Data)  reqCnt <- mkReg(0);

	// Three bits from LSB is always 3'b0 ; because PC has 8-alligned address. So ignore the three bits.
	function CacheIndex getIdx(Addr addr) = truncate(addr >> (3 + fromInteger(valueOf(SizeOf#(BlockOffset)))));
	function CacheTag getTag(Addr addr) = truncateLSB(addr);
	function BlockOffset getOffset(Addr addr) = truncate(addr >> 3);

	function Addr getBlockAddr(CacheTag tag, CacheIndex idx);
		BlockOffset def_offset = 0;
		Addr addr = {tag, idx, def_offset, 3'b0};
		return addr;
	endfunction

	let inited = truncateLSB(init) == 1'b1;

	rule initialize(!inited);
		init <= init + 1;
		tagArray.upd(truncate(init), Invalid);
		dirtyArray.upd(truncate(init), False);
	endrule

	/* TODO: Implement rules below to implement direct-mapped cache. */

	rule startMiss(status == StartMiss);
		/* TODO: Implement here */
	endrule

	rule sendFillReq(status == SendFillReq);
		/* TODO: Implement here */
	endrule

	rule waitFillResp(status == WaitFillResp);
		/* TODO: Implement here */
	endrule

	method Action req(MemReq r) if (status == Ready && inited);
		/* TODO: Implement here */

		let hit = ?;

		/* DO NOT MODIFY BELOW HERE! */
		if(!hit)
		begin
			missCnt <= missCnt + 1;
		end
		reqCnt <= reqCnt + 1;
	endmethod

	method ActionValue#(Data) resp;
		hitQ.deq;
		return hitQ.first;
	endmethod

	method ActionValue#(CacheMemReq) memReq;
		memReqQ.deq;
		return memReqQ.first;
	endmethod

	method Action memResp(Line r);
		memRespQ.enq(r);
	endmethod

	method Data getMissCnt;
		return missCnt;
	endmethod

	method Data getTotalReq;
		return reqCnt;
	endmethod
endmodule


module mkCacheSetAssociative (Cache);

	Vector#(LinesPerSet, RegFile#(CacheIndex, Line))				  dataArray <- replicateM(mkRegFileFull);
	Vector#(LinesPerSet, RegFile#(CacheIndex, Maybe#(CacheTag)))	   tagArray <- replicateM(mkRegFileFull);
	Vector#(LinesPerSet, RegFile#(CacheIndex, Bool))				 dirtyArray <- replicateM(mkRegFileFull);
	Vector#(LinesPerSet, RegFile#(CacheIndex, SetOffset)) lruArray <- replicateM(mkRegFileFull);

	Reg#(Bit#(TAdd#(SizeOf#(CacheIndex), 1))) init <- mkReg(0);
	Reg#(CacheStatus)					    status <- mkReg(Ready);
	Reg#(Maybe#(SetOffset)) 		targetLine <- mkReg(Invalid);

	Fifo#(1, Data)  hitQ <- mkBypassFifo;
	Reg#(MemReq) missReq <- mkRegU;

	Fifo#(2, CacheMemReq) memReqQ <- mkCFFifo;
	Fifo#(2, Line) 		 memRespQ <- mkCFFifo;

	Reg#(Data) missCnt <- mkReg(0);
	Reg#(Data)  reqCnt <- mkReg(0);

	function CacheIndex getIdx(Addr addr) = truncate(addr >> (3 + fromInteger(valueOf(SizeOf#(BlockOffset)))));
	function CacheTag getTag(Addr addr) = truncateLSB(addr);
	function BlockOffset getOffset(Addr addr) = truncate(addr >> 3);

	function Addr getBlockAddr(CacheTag tag, CacheIndex idx);
		BlockOffset def_offset = 0;
		Addr addr = {tag, idx, def_offset, 3'b0};
		return addr;
	endfunction

	function Maybe#(SetOffset) checkHit(CacheTag tag, CacheIndex idx);
		// Returns the SetOffset when cache hit occurs at given idx with the given tag.
		// It happens by checking the validity and tag value.
		Maybe#(SetOffset) ret = Invalid;

		for(Integer i = 0; i< valueOf(LinesPerSet); i = i + 1)
		begin
			let tagArrayVal = tagArray[i].sub(idx);

			if(isValid(tagArrayVal) && (fromMaybe(?, tagArrayVal) == tag) )
			begin
				ret = tagged Valid fromInteger(i);
			end
		end

		return ret;
	endfunction

	function Maybe#(SetOffset) findInvalid(CacheIndex idx);
		// Returns the SetOffset of a invalid cache slot at given idx.
		// If no one exists, returns Invalid.
		Maybe#(SetOffset) ret = Invalid;

		for(Integer i = 0; i < valueOf(LinesPerSet); i = i+1)
		begin
			if(!isValid(tagArray[i].sub(idx)))
			begin
				ret = tagged Valid fromInteger(i);
			end
		end

		return ret;
	endfunction

	function SetOffset findLRU(CacheIndex idx);
		// Returns the exact LRU.
		return lruArray[valueOf(LinesPerSet) - 1].sub(idx);
	endfunction

	function Action updateLRUArray(CacheIndex idx, SetOffset lineNum);
		// update lruArray to help finding LRU.
	    return action
			// find the index of lineNum element in the LRUArray.
	       	Integer idxInLRUArray = 0;
        	for (Integer i = 1; i < valueOf(LinesPerSet); i = i+1)
        	begin
            	if (lineNum == lruArray[i].sub(idx)) begin
            		idxInLRUArray = i;
          		end
			end

			// right shift elements before lineNum.
          	for (Integer i = 1;  i<= idxInLRUArray; i = i+1)
          	begin
            	lruArray[i].upd(idx, lruArray[i-1].sub(idx));
          	end

          	// put lineNum at the front.
          	lruArray[0].upd(idx, lineNum);
	    endaction;
    endfunction

 	let inited = truncateLSB(init) == 1'b1;

	rule initialize(!inited);
		init <= init + 1;

		for(Integer i = 0; i< valueOf(LinesPerSet);i = i+1)
		begin
			tagArray[i].upd(truncate(init), Invalid);
			dirtyArray[i].upd(truncate(init), False);
			lruArray[i].upd(truncate(init), fromInteger(i));
		end
	endrule

	/* TODO: Implement rules below to implement set-associative cache. */

	rule startMiss(status == StartMiss);
		/* TODO: Implement here */
	endrule

	rule sendFillReq(status == SendFillReq);
		/* TODO: Implement here */
	endrule

	rule waitFillResp(status == WaitFillResp);
		/* TODO: Implement here */
	endrule

	method Action req(MemReq r) if (status == Ready && inited);
		/* TODO: Implement here */

		let hit = ?;

		/* DO NOT MODIFY BELOW HERE! */
		if(!isValid(hit))
		begin
			missCnt <= missCnt + 1;
		end
		reqCnt <= reqCnt + 1;
	endmethod

	method ActionValue#(Data) resp;
		hitQ.deq;
		return hitQ.first;
	endmethod

	method ActionValue#(CacheMemReq) memReq;
		memReqQ.deq;
		return memReqQ.first;
	endmethod

	method Action memResp(Line r);
		memRespQ.enq(r);
	endmethod

	method Data getMissCnt;
		return missCnt;
	endmethod

	method Data getTotalReq;
		return reqCnt;
	endmethod
endmodule

module mkCache (Cache);
	/* TODO: Switch the comments for testing different types of Cache. */

	Cache cacheDirectMap <- mkCacheDirectMap;
	return cacheDirectMap;

	/*
	Cache cacheSetAssociative <- mkCacheSetAssociative;
	return cacheSetAssociative;
	*/

endmodule
