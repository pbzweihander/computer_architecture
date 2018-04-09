import Vector::*;

import FftCommon::*;
import Fifo::*;

interface Fft;
    method Action enq(Vector#(FftPoints, ComplexData) in);
    method ActionValue#(Vector#(FftPoints, ComplexData)) deq;
endinterface

(* synthesize *)
module mkFftCombinational(Fft);
    Fifo#(2, Vector#(FftPoints, ComplexData)) inFifo <- mkCFFifo;
    Fifo#(2, Vector#(FftPoints, ComplexData)) outFifo <- mkCFFifo;
    Vector#(NumStages, Vector#(BflysPerStage, Bfly4)) bfly <- replicateM(replicateM(mkBfly4));

    function Vector#(FftPoints, ComplexData) stage_f(StageIdx stage, Vector#(FftPoints, ComplexData) stage_in);
        Vector#(FftPoints, ComplexData) stage_temp, stage_out;
        for (FftIdx i = 0; i < fromInteger(valueOf(BflysPerStage)); i = i + 1) begin
            FftIdx idx = i * 4;
            Vector#(4, ComplexData) x;
            Vector#(4, ComplexData) twid;
            for (FftIdx j = 0; j < 4; j = j + 1 ) begin
                x[j] = stage_in[idx+j];
                twid[j] = getTwiddle(stage, idx+j);
            end
            let y = bfly[stage][i].bfly4(twid, x);

            for(FftIdx j = 0; j < 4; j = j + 1 )
                stage_temp[idx+j] = y[j];
        end

        stage_out = permute(stage_temp);

        return stage_out;
    endfunction

    rule doFft;
        inFifo.deq;
        Vector#(4, Vector#(FftPoints, ComplexData)) stage_data;
        stage_data[0] = inFifo.first;

        for (StageIdx stage = 0; stage < 3; stage = stage + 1)
            stage_data[stage+1] = stage_f(stage, stage_data[stage]);
        outFifo.enq(stage_data[3]);
    endrule

    method Action enq(Vector#(FftPoints, ComplexData) in);
        inFifo.enq(in);
    endmethod

    method ActionValue#(Vector#(FftPoints, ComplexData)) deq;
        outFifo.deq;
        return outFifo.first;
    endmethod
endmodule

(* synthesize *)
module mkFftFolded(Fft);
    Fifo#(2, Vector#(FftPoints, ComplexData)) inFifo <- mkCFFifo;
    Fifo#(2, Vector#(FftPoints, ComplexData)) outFifo <- mkCFFifo;
    Vector#(BflysPerStage, Bfly4) bfly <- replicateM(mkBfly4);
    Reg#(Maybe#(Vector#(FftPoints, ComplexData))) stage_reg <- mkReg(tagged Invalid);
    Reg#(Bit#(3)) stage_count <- mkReg(0);

    function Vector#(FftPoints, ComplexData) stage_f(StageIdx stage, Vector#(FftPoints, ComplexData) stage_in);
        Vector#(FftPoints, ComplexData) stage_temp, stage_out;
        for (FftIdx i = 0; i < fromInteger(valueOf(BflysPerStage)); i = i + 1) begin
            FftIdx idx = i * 4;
            Vector#(4, ComplexData) x;
            Vector#(4, ComplexData) twid;
            for (FftIdx j = 0; j < 4; j = j + 1 ) begin
                x[j] = stage_in[idx+j];
                twid[j] = getTwiddle(stage, idx+j);
            end
            let y = bfly[i].bfly4(twid, x);

            for(FftIdx j = 0; j < 4; j = j + 1 )
                stage_temp[idx+j] = y[j];
        end

        stage_out = permute(stage_temp);

        return stage_out;
    endfunction

    rule startFft(stage_reg matches tagged Invalid);
        inFifo.deq;
        stage_reg <= tagged Valid inFifo.first;
        stage_count <= 0;
    endrule

    rule doFft(stage_count < 3 && isValid(stage_reg));
        stage_reg <= tagged Valid stage_f(stage_count, validValue(stage_reg));
        stage_count <= stage_count + 1;
    endrule

    rule endFft(stage_count == 3 && isValid(stage_reg));
        outFifo.enq(validValue(stage_reg));
        stage_reg <= tagged Invalid;
    endrule

    method Action enq(Vector#(FftPoints, ComplexData) in);
        inFifo.enq(in);
    endmethod

    method ActionValue#(Vector#(FftPoints, ComplexData)) deq;
        outFifo.deq;
        return outFifo.first;
    endmethod
endmodule

(* synthesize *)
module mkFftPipelined(Fft);
    Fifo#(2, Vector#(FftPoints, ComplexData)) inFifo <- mkCFFifo;
    Fifo#(2, Vector#(FftPoints, ComplexData)) outFifo <- mkCFFifo;
    Vector#(NumStages, Vector#(BflysPerStage, Bfly4)) bfly <- replicateM(replicateM(mkBfly4));
    Vector#(2, Reg#(Maybe#(Vector#(FftPoints, ComplexData)))) stage_regs <- replicateM(mkReg(tagged Invalid));

    function Vector#(FftPoints, ComplexData) stage_f(StageIdx stage, Vector#(FftPoints, ComplexData) stage_in);
        Vector#(FftPoints, ComplexData) stage_temp, stage_out;
        for (FftIdx i = 0; i < fromInteger(valueOf(BflysPerStage)); i = i + 1) begin
            FftIdx idx = i * 4;
            Vector#(4, ComplexData) x;
            Vector#(4, ComplexData) twid;
            for (FftIdx j = 0; j < 4; j = j + 1 ) begin
                x[j] = stage_in[idx+j];
                twid[j] = getTwiddle(stage, idx+j);
            end
            let y = bfly[stage][i].bfly4(twid, x);

            for(FftIdx j = 0; j < 4; j = j + 1 )
                stage_temp[idx+j] = y[j];
        end

        stage_out = permute(stage_temp);

        return stage_out;
    endfunction

    rule doFft;
        if (inFifo.notEmpty) begin
            inFifo.deq;
            stage_regs[0] <= tagged Valid stage_f(0, inFifo.first);
        end else
            stage_regs[0] <= tagged Invalid;
        if (stage_regs[0] matches tagged Valid .r)
            stage_regs[1] <= tagged Valid stage_f(1, r);
        else
            stage_regs[1] <= tagged Invalid;
        if (stage_regs[1] matches tagged Valid .r)
            outFifo.enq(stage_f(2, r));
    endrule

    method Action enq(Vector#(FftPoints, ComplexData) in);
        inFifo.enq(in);
    endmethod

    method ActionValue#(Vector#(FftPoints, ComplexData)) deq;
        outFifo.deq;
        return outFifo.first;
    endmethod
endmodule

interface SuperFoldedFft#(numeric type radix);
    method ActionValue#(Vector#(FftPoints, ComplexData)) deq;
    method Action enq(Vector#(FftPoints, ComplexData) in);
endinterface

module mkFftSuperFolded(SuperFoldedFft#(radix)) provisos(Div#(TDiv#(FftPoints, 4), radix, times), Mul#(radix, times, TDiv#(FftPoints, 4)));
    Fifo#(2, Vector#(FftPoints, ComplexData)) inFifo <- mkCFFifo;
    Fifo#(2, Vector#(FftPoints, ComplexData)) outFifo <- mkCFFifo;
    Vector#(radix, Bfly4) bfly <- replicateM(mkBfly4);
    Reg#(Maybe#(FftPoints, ComplexData)) stage_reg = mkReg(tagged Invalid);

    rule doFft;
        //TODO: Remove below two lines Implement the rest of this module
        outFifo.enq(inFifo.first);
        inFifo.deq;
    endrule

    method Action enq(Vector#(FftPoints, ComplexData) in);
        inFifo.enq(in);
    endmethod

    method ActionValue#(Vector#(FftPoints, ComplexData)) deq;
        outFifo.deq;
        return outFifo.first;
    endmethod
endmodule

function Fft getFft(SuperFoldedFft#(radix) f);
    return (interface Fft;
                method enq = f.enq;
                method deq = f.deq;
            endinterface);
endfunction
