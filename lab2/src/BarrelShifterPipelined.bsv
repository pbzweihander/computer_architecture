import Multiplexer::*;
import FIFO::*;
import FIFOF::*;
import Vector::*;
import SpecialFIFOs::*;

/* Interface of the basic right shifter module */
interface BarrelShifterRightPipelined;
    method Action shift_request(Bit#(64) operand, Bit#(6) shamt, Bit#(1) val);
    method ActionValue#(Bit#(64)) shift_response();
endinterface

module mkBarrelShifterRightPipelined(BarrelShifterRightPipelined);
    FIFOF#(Tuple3#(Bit#(64), Bit#(6), Bit#(1))) inFifo <- mkFIFOF;
    let outFifo <- mkFIFOF;
    Vector#(5, Reg#(Maybe#(Tuple3#(Bit#(64), Bit#(6), Bit#(1))))) regs <- replicateM(mkReg(tagged Invalid));

    function Bit#(64) shift_once(Bit#(64) operand, Bit#(1) shamt, Bit#(1) val, Integer i);
        Bit#(64) shifted;
        for (Integer j = 0; j < 64 - i; j = j + 1)
            shifted[j] = operand[j + i];
        for (Integer j = 64 - i; j < 64; j = j + 1)
            shifted[j] = val;
        return multiplexer64(shamt, operand, shifted);
    endfunction

    rule shift;
        if (inFifo.notEmpty()) begin
            inFifo.deq();
            let first = inFifo.first();
            let operand = tpl_1(first);
            let shamt = tpl_2(first);
            let val = tpl_3(first);
            regs[0] <= tagged Valid tuple3(shift_once(operand, shamt[0], val, 1), shamt, val);
        end else
            regs[0] <= tagged Invalid;
        for (Integer i = 1, Integer j = 2; i <= 4; i = i + 1, j = j + j) begin
            if (regs[i - 1] matches tagged Valid .r) begin
                let operand = tpl_1(r);
                let shamt = tpl_2(r);
                let val = tpl_3(r);
                regs[i] <= tagged Valid tuple3(shift_once(operand, shamt[i], val, j), shamt, val);
            end else
                regs[i] <= tagged Invalid;
        end
        if (regs[4] matches tagged Valid .r) begin
            let operand = tpl_1(r);
            let shamt = tpl_2(r);
            let val = tpl_3(r);
            outFifo.enq(shift_once(operand, shamt[5], val, 32));
        end
    endrule

    method Action shift_request(Bit#(64) operand, Bit#(6) shamt, Bit#(1) val);
        inFifo.enq(tuple3(operand, shamt, val));
    endmethod

    method ActionValue#(Bit#(64)) shift_response();
        outFifo.deq;
        return outFifo.first;
    endmethod
endmodule

interface BarrelShifterRightLogicalPipelined;
    method Action shift_request(Bit#(64) operand, Bit#(6) shamt);
    method ActionValue#(Bit#(64)) shift_response();
endinterface

typedef BarrelShifterRightLogicalPipelined BarrelShifterRightArithmeticPipelined;
typedef BarrelShifterRightLogicalPipelined BarrelShifterLeftPipelined;

module mkBarrelShifterLeftPipelined(BarrelShifterLeftPipelined);
    let bsrp <- mkBarrelShifterRightPipelined;

    method Action shift_request(Bit#(64) operand, Bit#(6) shamt);
        bsrp.shift_request(reverseBits(operand), shamt, 0);
    endmethod

    method ActionValue#(Bit#(64)) shift_response();
        let result <- bsrp.shift_response();
        return reverseBits(result);
    endmethod
endmodule

module mkBarrelShifterRightLogicalPipelined(BarrelShifterRightLogicalPipelined);
    let bsrp <- mkBarrelShifterRightPipelined;

    method Action shift_request(Bit#(64) operand, Bit#(6) shamt);
        bsrp.shift_request(operand, shamt, 0);
    endmethod

    method ActionValue#(Bit#(64)) shift_response();
        let result <- bsrp.shift_response();
        return result;
    endmethod
endmodule

module mkBarrelShifterRightArithmeticPipelined(BarrelShifterRightArithmeticPipelined);
    let bsrp <- mkBarrelShifterRightPipelined;

    method Action shift_request(Bit#(64) operand, Bit#(6) shamt);
        bsrp.shift_request(operand, shamt, operand[63]);
    endmethod

    method ActionValue#(Bit#(64)) shift_response();
        let result <- bsrp.shift_response();
        return result;
    endmethod
endmodule
