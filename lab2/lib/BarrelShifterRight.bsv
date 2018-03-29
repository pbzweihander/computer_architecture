import Multiplexer::*;

interface BarrelShifterRight;
	method ActionValue#(Bit#(64)) rightShift(Bit#(64) val, Bit#(6) shiftAmt, Bit#(1) shiftValue);
endinterface

module mkBarrelShifterRight(BarrelShifterRight);
	method ActionValue#(Bit#(64)) rightShift(Bit#(64) val, Bit#(6) shiftAmt, Bit#(1) shiftValue);
		return (val >> shiftAmt);
	endmethod
endmodule

interface BarrelShifterRightLogical;
	method ActionValue#(Bit#(64)) rightShift(Bit#(64) val, Bit#(6) shiftAmt);
endinterface

module mkBarrelShifterRightLogical(BarrelShifterRightLogical);
//	let bsr <- mkBarrelShifterRight;
	method ActionValue#(Bit#(64)) rightShift(Bit#(64) val, Bit#(6) shiftAmt);
		return (val >> shiftAmt);
	endmethod
endmodule

typedef BarrelShifterRightLogical BarrelShifterRightArithmetic;

module mkBarrelShifterRightArithmetic(BarrelShifterRightArithmetic);
//	let bsr <- mkBarrelShifterRight;
	method ActionValue#(Bit#(64)) rightShift(Bit#(64) val, Bit#(6) shiftAmt);
		Int#(64) newVal = unpack(val);
		return pack(newVal >> shiftAmt);
	endmethod
endmodule
