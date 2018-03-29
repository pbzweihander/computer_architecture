import BarrelShifterRight::*;

interface BarrelShifterLeft;
    method ActionValue#(Bit#(64)) leftShift(Bit#(64) val, Bit#(6) shiftAmt);
endinterface

module mkBarrelShifterLeft(BarrelShifterLeft);
    let bsr <- mkBarrelShifterRightLogical;
    method ActionValue#(Bit#(64)) leftShift(Bit#(64) val, Bit#(6) shiftAmt);
        let result <- bsr.rightShift(reverseBits(val), shiftAmt);
        return reverseBits(result);
    endmethod
endmodule
