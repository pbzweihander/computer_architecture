import Multiplexer::*;

interface BarrelShifterRight;
  method ActionValue#(Bit#(64)) rightShift(Bit#(64) val, Bit#(6) shiftAmt, Bit#(1) shiftValue);
endinterface

module mkBarrelShifterRight(BarrelShifterRight);
  method ActionValue#(Bit#(64)) rightShift(Bit#(64) val, Bit#(6) shiftAmt, Bit#(1) shiftValue);
    Bit#(64) result = val;
    Integer c = 0;
    for (Integer i = 1; c < 6; i = i + i) begin
      Bit#(64) shifted;
      for (Integer j = 0; j < 64 - i; j = j + 1)
        shifted[j] = result[j + i];
      for (Integer j = 64 - i; j < 64; j = j + 1)
        shifted[j] = shiftValue;
      result = multiplexer64(shiftAmt[c], result, shifted);
      c = c + 1;
    end
    return result;
  endmethod
endmodule

interface BarrelShifterRightLogical;
  method ActionValue#(Bit#(64)) rightShift(Bit#(64) val, Bit#(6) shiftAmt);
endinterface

module mkBarrelShifterRightLogical(BarrelShifterRightLogical);
  let bsr <- mkBarrelShifterRight;
  method ActionValue#(Bit#(64)) rightShift(Bit#(64) val, Bit#(6) shiftAmt);
    let result <- bsr.rightShift(val, shiftAmt, 0);
    return result;
  endmethod
endmodule

typedef BarrelShifterRightLogical BarrelShifterRightArithmetic;

module mkBarrelShifterRightArithmetic(BarrelShifterRightArithmetic);
  let bsr <- mkBarrelShifterRight;
  method ActionValue#(Bit#(64)) rightShift(Bit#(64) val, Bit#(6) shiftAmt);
    let result <- bsr.rightShift(val, shiftAmt, val[63]);
    return result;
  endmethod
endmodule
