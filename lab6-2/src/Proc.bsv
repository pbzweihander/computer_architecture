import Types::*;
import ProcTypes::*;
import MemTypes::*;
import BypassRFile::*;
import Scoreboard::*;
import IMemory::*;
import DMemory::*;
import Decode::*;
import Exec::*;
import Cop::*;
import Fifo::*;

typedef struct {
    Inst inst;
    Addr pc;
    Addr ppc;
    Bool epoch;
} Fetch2Decode deriving(Bits, Eq);

typedef struct {
    DecodedInst dInst;
    Addr ppc;
    Bool epoch;
} Decode2Exec deriving(Bits, Eq);

typedef struct {
    ExecInst eInst;
    Bool drop;
} Exec2Memory deriving(Bits, Eq);

typedef struct {
    ExecInst eInst;
    Bool drop;
} Memory2WriteBack deriving(Bits, Eq);

typedef struct {
    Maybe#(FullIndx) dstE;
    Maybe#(FullIndx) dstM;
    Maybe#(Data) valE;
    Maybe#(Data) valM;
} Forward deriving(Bits, Eq);

(*synthesize*)
module mkProc(Proc);
    Reg#(Addr) pc <- mkRegU;
    RFile rf <- mkBypassRFile;
    IMemory iMem <- mkIMemory;
    DMemory dMem <- mkDMemory;
    Cop cop <- mkCop;

    Reg#(CondFlag) condFlag <- mkRegU;
    Reg#(ProcStatus) stat <- mkRegU;

    Fifo#(1, Addr) execRedirect <- mkBypassFifo;
    Fifo#(1, Addr) memRedirect <- mkBypassFifo;
    Fifo#(1, ProcStatus) statRedirect <- mkBypassFifo;

    Fifo#(2, Fetch2Decode) f2d <- mkPipelineFifo;
    Fifo#(2, Decode2Exec) d2e <- mkPipelineFifo;
    Fifo#(2, Exec2Memory) e2m <- mkPipelineFifo;
    Fifo#(2, Memory2WriteBack) m2w <- mkPipelineFifo;

    Fifo#(1, Forward) e2d <- mkBypassFifo;
    Fifo#(1, Forward) m2d <- mkBypassFifo;

    Reg#(Bool) fEpoch <- mkRegU;
    Reg#(Bool) eEpoch <- mkRegU;

    rule doFetch(cop.started && stat == AOK);
        /* Fetch */
        let ipc = pc;
        let iEpoch = fEpoch;

        if (execRedirect.notEmpty) begin
            execRedirect.deq;
            ipc = execRedirect.first;
            iEpoch = !iEpoch;
        end
        if (memRedirect.notEmpty) begin
            memRedirect.deq;
            ipc = memRedirect.first;
            iEpoch = !iEpoch;
        end

        let inst = iMem.req(ipc);
        let ppc = nextAddr(ipc, getICode(inst));

        $display("Fetch : from Pc %d , expanded inst : %x, \n", ipc, inst, showInst(inst));

        pc <= ppc;
        fEpoch <= iEpoch;

        f2d.enq(Fetch2Decode{
                inst: inst,
                pc: ipc,
                ppc: ppc,
                epoch: iEpoch
            });
    endrule

    rule doDecode(cop.started && stat == AOK);
        let inst = f2d.first.inst;
        let ipc = f2d.first.pc;
        let ppc = f2d.first.ppc;
        let iEpoch = f2d.first.epoch;

        Forward f[2];
        let stall = False;
        let forwardA = False;
        let forwardB = False;

        /* Decode */
        let dInst = decode(inst, ipc);

        if (e2d.notEmpty) begin
            e2d.deq;
            f[1] = e2d.first;
        end else
            f[1] = Forward {
                    dstE: Invalid,
                    dstM: Invalid,
                    valE: Invalid,
                    valM: Invalid
                };

        if (m2d.notEmpty) begin
            m2d.deq;
            f[0] = m2d.first;
        end else
            f[0] = Forward {
                    dstE: Invalid,
                    dstM: Invalid,
                    valE: Invalid,
                    valM: Invalid
                };

        if (dInst.regA matches tagged Valid.regA) begin
            if (f[0].dstE == tagged Valid regA) begin
                dInst.valA = f[0].valE;
                forwardA = True;
            end else if (f[0].dstM == tagged Valid regA) begin
                if (f[0].valM matches tagged Invalid)
                    stall = True;
                else begin
                    dInst.valA = f[0].valM;
                    forwardA = True;
                end
            end

            if (f[1].dstE == tagged Valid regA) begin
                dInst.valA = f[1].valE;
                forwardA = True;
            end else if (f[1].dstM == tagged Valid regA) begin
                if (f[1].valM matches Invalid)
                    stall = True;
                else begin
                    dInst.valA = f[1].valM;
                    forwardA = True;
                end
            end

            if (!forwardA) begin
                dInst.valA = tagged Valid rf.rdA(validRegValue(dInst.regA));
            end
        end else
            dInst.valA = Invalid;

        if (dInst.regB matches tagged Valid.regB) begin
            if (f[0].dstE == tagged Valid regB) begin
                dInst.valB = f[0].valE;
                forwardB = True;
            end else if (f[0].dstM == tagged Valid regB) begin
                if (f[0].valM matches tagged Invalid)
                    stall = True;
                else begin
                    dInst.valB = f[0].valM;
                    forwardB = True;
                end
            end

            if (f[1].dstE == tagged Valid regB) begin
                dInst.valB = f[1].valE;
                forwardB = True;
            end else if (f[1].dstM == tagged Valid regB) begin
                if (f[1].valM matches Invalid)
                    stall = True;
                else begin
                    dInst.valB = f[1].valM;
                    forwardB = True;
                end
            end

            if (!forwardB) begin
                dInst.valB = tagged Valid rf.rdB(validRegValue(dInst.regB));
            end
        end else
            dInst.valB = Invalid;

        if (dInst.regA matches tagged Valid.regA)
            dInst.copVal = tagged Valid cop.rd(validRegValue(dInst.regA));
        else
            dInst.copVal = Invalid;

        if (!stall) begin
            f2d.deq;

            $display("Decode : from Pc %d , expanded inst : %x, \n", ipc, inst, showInst(inst));

            d2e.enq(Decode2Exec {
                    dInst: dInst,
                    ppc: ppc,
                    epoch: iEpoch
                });
        end else
            $display("stall");
    endrule

    rule doExecute(cop.started && stat == AOK);
        let dInst = d2e.first.dInst;
        let ppc = d2e.first.ppc;
        let iEpoch = d2e.first.epoch;
        d2e.deq;

        /* Execute */
        let eInst = exec(dInst, condFlag, ppc);
        if (iEpoch == eEpoch) begin
            condFlag <= eInst.condFlag;

            e2m.enq(Exec2Memory {
                    eInst: eInst,
                    drop: False
                });
            e2d.enq(Forward {
                    dstE: eInst.dstE,
                    dstM: eInst.dstM,
                    valE: eInst.valE,
                    valM:eInst.valM
                });

            if (eInst.mispredict) begin
                eEpoch <= !eEpoch;
                if (eInst.iType != Ret) begin
                    let redirPc = validValue(eInst.nextPc);
                    $display("execute: mispredicted, redirect %d ", redirPc);
                    execRedirect.enq(redirPc);
                end
            end
        end else
            e2m.enq(Exec2Memory {
                    eInst: eInst,
                    drop: True
                });
    endrule

    rule doMemory(cop.started && stat == AOK);
        let eInst = e2m.first.eInst;
        let drop = e2m.first.drop;
        e2m.deq;

        /* Memory */
        if (!drop) begin
            let iType = eInst.iType;
            case (iType)
                MRmov, Pop, Ret: begin
                    let ldData <- (dMem.req(MemReq {
                            op: Ld,
                            addr: eInst.memAddr,
                            data:?
                        }));
                    eInst.valM = Valid(little2BigEndian(ldData));
                    $display("Loaded %d from %d", little2BigEndian(ldData), eInst.memAddr);
                    if (iType == Ret) begin
                        eInst.nextPc = eInst.valM;
                        let redirPc = validValue(eInst.nextPc);
                        $display("memory: mispredicted, redirect %d ", redirPc);
                        memRedirect.enq(redirPc);
                    end
                end

                RMmov, Call, Push: begin
                    let stData = (iType == Call)
                        ? eInst.valP
                        : validValue(eInst.valA);
                    let dummy <- dMem.req(MemReq {
                            op: St,
                            addr: eInst.memAddr,
                            data: big2LittleEndian(stData)
                        });
                    $display("Stored %d into %d", stData, eInst.memAddr);
                end
            endcase

            m2w.enq(Memory2WriteBack {
                    eInst: eInst,
                    drop: False
                });
            m2d.enq(Forward {
                    dstE: eInst.dstE,
                    dstM: eInst.dstM,
                    valE: eInst.valE,
                    valM:eInst.valM
                });
        end else
            m2w.enq(Memory2WriteBack {
                    eInst: eInst,
                    drop: True
                });
    endrule

    rule doWriteBack(cop.started && stat == AOK);
        let eInst = m2w.first.eInst;
        let drop = m2w.first.drop;
        m2w.deq;

        if (!drop) begin
            /* Update Status */
            let newStatus =
                case (eInst.iType)
                    Unsupported: INS;
                    Hlt: HLT;
                    default: AOK;
                endcase;
            statRedirect.enq(newStatus);

            /* WriteBack */
            if (isValid(eInst.dstE)) begin
                $display("On %d, writes %d   (wrE)", validRegValue(eInst.dstE), validValue(eInst.valE));
                rf.wrE(validRegValue(eInst.dstE), validValue(eInst.valE));
            end
            if(isValid(eInst.dstM)) begin
                $display("On %d, writes %d   (wrM)", validRegValue(eInst.dstM), validValue(eInst.valM));
                rf.wrM(validRegValue(eInst.dstM), validValue(eInst.valM));
            end

            cop.wr(eInst.dstE, validValue(eInst.valE));
        end
    endrule

    rule upd_Stat(cop.started);
        $display("Stat update");
        statRedirect.deq;
        stat <= statRedirect.first;
    endrule

    rule statHLT(cop.started && stat == HLT);
        $fwrite(stderr, "Program Finished by halt\n");
        $finish;
    endrule

    rule statINS(cop.started && stat == INS);
        $fwrite(stderr, "Executed unsupported instruction. Exiting\n");
        $finish;
    endrule

    method ActionValue#(Tuple3#(RIndx, Data, Data)) cpuToHost;
        let retV <- cop.cpuToHost;
        return retV;
    endmethod

    method Action hostToCpu(Addr startpc) if (!cop.started);
        cop.start;
        eEpoch <= False;
        fEpoch <= False;
        pc <= startpc;
        stat <= AOK;
    endmethod
endmodule
