import Types::*;
import ProcTypes::*;
import Vector::*;


function DecodedInst decode(Inst inst,Addr pc);
	DecodedInst dInst = ?;
	let iCode = inst[79:76];
	let ifun  = inst[75:72];
	let rA    = inst[71:68];
	let rB    = inst[67:64];
	let imm   = little2BigEndian(inst[63:0]);
	let dest  = little2BigEndian(inst[71:8]);

	case (iCode)

	/* TODO: Finish implementing decode.bsv */
	/* HINT: rrmovq is included in cmov */
	?:
	begin
		dInst.iType = Unsupported;
		dInst.opqFunc = FNop;
		dInst.condUsed = Al;
		dInst.valP = pc + 1;
		dInst.dstE = Invalid;
		dInst.dstM = Invalid;
		dInst.regA = Invalid;
		dInst.regB = Invalid;
		dInst.valC = Invalid;
	end


	/* DO NOT MODIFY BELOW HERE! */
	copinst:
	begin
		dInst.iType = case(ifun)
					  	mtc0 : Mtc0; //Mtc0
					  	mfc0 : Mfc0; //Mfc0
				  	  endcase;
		dInst.opqFunc = FNop;
		dInst.condUsed = Al;
		dInst.valP = pc + 2;
		dInst.dstE = case(ifun)
						mtc0 : validCop(rB);
						mfc0 : validReg(rB);
					 endcase;
		dInst.dstM = Invalid;
		dInst.regA = case(ifun)
						mtc0 : validReg(rA);
						mfc0 : validCop(rA);
					 endcase;
		dInst.regB = Invalid;
		dInst.valC = Invalid;
	end

	default:
	begin
		dInst.iType = Unsupported;
		dInst.opqFunc = FNop;
		dInst.condUsed = Al;
		dInst.valP = pc + 1;
		dInst.dstE = Invalid;
		dInst.dstM = Invalid;
		dInst.regA = Invalid;
		dInst.regB = Invalid;
		dInst.valC = Invalid;
	end

	endcase
	return dInst;
endfunction
