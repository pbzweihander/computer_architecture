import Types::*;
import FShow::*;
import MemTypes::*;

typedef Bit#(4) RIndx;
typedef enum {AOK, ADR, INS, HLT} ProcStatus deriving(Bits,Eq);

typedef Bit#(8) OpCode;
typedef Bit#(4) ICode;

typedef enum {Normal, CopReg} RegType deriving (Bits, Eq);
typedef enum {Unsupported, Rmov, Opq, RMmov, MRmov, Cmov, Jmp, Push, Pop, Call, Ret, Hlt, Nop, Mtc0, Mfc0 } IType deriving(Bits,Eq);
typedef enum {Al, Eq, Neq, Lt, Le, Gt, Ge} CondUsed deriving(Bits,Eq);
typedef enum {FNop, FAdd, FSub, FAnd ,FXor} OpqFunc deriving(Bits,Eq);

typedef Tuple2#(CondFlag, Data) OpqRes;

typedef Tuple4#(CondFlag, Bool, Bool, Data) ExecRes;


  typedef struct {
    RegType regType;
    RIndx idx;
  } FullIndx deriving (Bits, Eq);

  function Maybe#(FullIndx) validReg(RIndx idx) = Valid(FullIndx{regType: Normal, idx: idx});
  function Maybe#(FullIndx) validCop(RIndx idx) = Valid(FullIndx{regType: CopReg, idx: idx});
  function RIndx validRegValue(Maybe#(FullIndx) idx) = validValue(idx).idx;

////////////////////////////////

interface Proc;
	method ActionValue#(Tuple3#(RIndx, Data, Data)) cpuToHost;
	method Action hostToCpu(Addr startpc);
	method ActionValue#(Tuple2#(Data,Data)) getCounts;
endinterface

typedef struct {Bool zf; Bool sf; Bool of;} CondFlag deriving(Bits, Eq);  // Condition Codes; {ZF, SF, OF}

typedef struct{
  IType   iType;
  OpqFunc opqFunc;
  CondUsed  condUsed;
  Addr valP;
  Maybe#(FullIndx) dstE;
  Maybe#(FullIndx) dstM;
  Maybe#(FullIndx) regA;
  Maybe#(FullIndx) regB;
  Maybe#(Data) valA;
  Maybe#(Data) valB;
  Maybe#(Data) valC;
  Maybe#(Data) copVal;
} DecodedInst deriving(Bits,Eq);

typedef struct{
  IType iType;
  Maybe#(FullIndx) dstE;
  Maybe#(FullIndx) dstM;
  Maybe#(Data) valE;
  Maybe#(Data) valA;
  Maybe#(Data) valC;
  Maybe#(Data) valM;
  Addr valP;
  CondFlag condFlag;
  Bool condSatisfied;
  Bool mispredict;
  Addr memAddr;
  Maybe#(Addr) addr;
  Maybe#(Data) copVal;
} ExecInst deriving(Bits,Eq);

typedef struct{
	Addr pc;
	Addr nextPc;
	Bool taken;
	Bool mispredict;
} Redirect deriving(Bits,Eq);

function ICode getICode(Inst inst);
	return inst[79:76];
endfunction	

function OpCode getOpCode(Inst inst);
	return inst[79:72];
endfunction

function Addr nextAddr(Addr pc, Bit#(4) iCode);	
	let offset = case(iCode) 
					halt, nop, ret : 1;
					cmov, opq, push, pop, copinst : 2;
					jmp,call : 9;
					irmovq, rmmovq, mrmovq : 10;
					default : 1;
				  endcase;
	return pc + offset;
endfunction

function Bool isValidInst(OpCode opCode);
  let iCode = opCode[7:4];
  let fCode = opCode[3:0];

  let res = case(iCode)
  			halt, nop, irmovq, rmmovq, mrmovq, call, ret, push, pop : (fCode == 0);
				cmov, jmp : ((fCode >= 0) && (fCode < 7));
				opq : ((fCode >= 0) && (fCode < 4));
				copinst : ((fCode == 0) || (fCode == 1));
				default : False;
			endcase;

  return res;
endfunction

//Registers
Bit#(4) rax = 4'b0000;  //0 
Bit#(4) rcx = 4'b0001;  //1
Bit#(4) rdx = 4'b0010;  //2
Bit#(4) rbx = 4'b0011;  //3
Bit#(4) rsp = 4'b0100;  //4
Bit#(4) rbp = 4'b0101;  //5
Bit#(4) rsi = 4'b0110;  //6
Bit#(4) rdi = 4'b0111;  //7
Bit#(4) r8  = 4'b1000;  //8
Bit#(4) r9  = 4'b1001;  //9
Bit#(4) r10 = 4'b1010;  //A
Bit#(4) r11 = 4'b1011;  //B
Bit#(4) r12 = 4'b1100;  //C
Bit#(4) r13 = 4'b1101;  //D
Bit#(4) r14 = 4'b1110;  //E


//Instiructions

 //Miscellaneous Instructions
  Bit#(4) halt     = 4'b0000; 	//0
  Bit#(4) nop      = 4'b0001; 	//1

 //Move Operations
  Bit#(4) irmovq   = 4'b0011; 	//3
  Bit#(4) rmmovq   = 4'b0100; 	//4
  Bit#(4) mrmovq   = 4'b0101; 	//5

 // Conditional Moves
  Bit#(4) cmov     = 4'b0010; 	//2

 //Arithmetic and Logical Operations
  Bit#(4) opq      = 4'b0110; 	//6
   //Sub-Func (Opqs)
    Bit#(4) addc     = 4'b0000;
    Bit#(4) subc     = 4'b0001;
    Bit#(4) andc     = 4'b0010;
    Bit#(4) xorc     = 4'b0011;


 //Jumps 
  Bit#(4) jmp      = 4'b0111;	//7

 //Function Call & Return Instructions
  Bit#(4) call     = 4'b1000; //8
  Bit#(4) ret      = 4'b1001;	//9

 //Stack Operations
  Bit#(4) push     = 4'b1010;	//10
  Bit#(4) pop      = 4'b1011;	//11

  //Compare Functions
    Bit#(4) fNcnd    = 4'b0000;
    Bit#(4) fLe      = 4'b0001;
    Bit#(4) fLt      = 4'b0010;
    Bit#(4) fEq      = 4'b0011;
    Bit#(4) fNeq     = 4'b0100;
    Bit#(4) fGe      = 4'b0101;
    Bit#(4) fGt      = 4'b0110;
  //Coprocesor Instruction
  Bit#(4) copinst = 4'b1100;	//12

  Bit#(4) mtc0 = 4'b0000;
	Bit#(4) mfc0 = 4'b0001;

function Data reverseEndian(Data target);
  return {target[7:0],target[15:8],target[23:16],target[31:24],target[39:32],target[47:40],target[55:48],target[63:56]};
endfunction

function Data big2LittleEndian(Data target) = reverseEndian(target);
function Data little2BigEndian(Data target) = reverseEndian(target);


function Fmt showInst(Inst inst);
  Fmt retv = fshow("");
  
  let iCode = inst[79:76];
  let fCode = inst[75:72];
  let regA  = inst[71:68];
  let regB  = inst[67:64];
  let vals  = reverseEndian(inst[63:0]);
  let dest  = reverseEndian(inst[71:8]);

  case(iCode)

	halt : 
  	retv = fshow ("halt");

	nop :
		retv = fshow ("nop");
		
	irmovq :
		retv = fshow("irmovq $") + fshow(vals) + fshow(",  %") + fshow(regB);

	rmmovq :
		retv = fshow("rmmovq %") + fshow(regA) + fshow (", ") + fshow(vals)  + fshow("(%") + fshow(regB) + fshow(")");

	mrmovq :
		retv = fshow("mrmovq ") + fshow(vals)  + fshow("(%") + fshow(regB) + fshow(")") + fshow(", %") + fshow(regA);
	
	cmov : 
	begin
		case(fCode)
			fNcnd :
				retv = fshow("rrmovq %") + fshow(regA) + fshow(", %") + fshow(regB);
			default :
			begin
				retv = fshow("cmov");
				retv = retv + (case(fCode)
			    				fLe  : fshow("le %");
								fLt  : fshow("l %");
								fEq  : fshow("e %");
								fNeq : fshow("ne %");		
								fGe  : fshow("ge %");
								fGt  : fshow("g %");
							 endcase);
				retv = retv + fshow(regA) + fshow(", %") + fshow(regB);
			end
		endcase
	end
				
	opq :
	begin
		retv = case(fCode)
				addc : fshow("addq %");
				subc : fshow("subq %");
				andc : fshow("andq %");
				xorc : fshow("xorq %");
			  endcase;

		retv = retv + fshow(regA) + fshow(", %") + fshow(regB);
	end
	
	jmp :
	begin
		retv = fshow("j");
		retv = retv + (case(fCode)
						fNcnd : fshow("mp $");
						fLe   : fshow("le $");
						fLt   : fshow("l $");
						fEq   : fshow("e $");
						fNeq  : fshow("ne $");
						fGe   : fshow("ge $");
						fGt   : fshow("g $");
			 		 endcase);
		retv = retv + fshow(dest);
	end

	push :
		retv = fshow("push %") + fshow(regA);

	pop :
		retv = fshow("pop %") + fshow(regA);

	ret :
		retv = fshow("ret");

	call : 
		retv = fshow("call ") + fshow(dest);

	copinst : 
	begin
		retv = case(fCode)
				mtc0 : fshow("mtc0 %");
				mfc0 : fshow("mfc0 %");
			   endcase;
		retv = retv + fshow(regA) + fshow(", %") + fshow(regB);
	end

	default :
		retv = fshow("Unsupported Instruction");	
  endcase

  return retv;
endfunction

