import Types::*;
import MemTypes::*;
import RegFile::*;
import Vector::*;

interface IMemory;
	method Inst req(Addr a);
endinterface

(*synthesize*)

module mkIMemory(IMemory);
	RegFile#(MemIndx, Data) iMem <- mkRegFileFullLoad("memory.vmh");
	
	method req(Addr a);
		let idx = a >> 3; // 3 bits are for the offset
		let offset = a[2:0];

		Vector#(3, Data) tempRegVec = newVector;

		for(Integer i = 0 ; i< 3 ; i = i +1)
			tempRegVec[i] = iMem.sub(truncate(idx + fromInteger(i)));

		let line = {tempRegVec[0], tempRegVec[1], tempRegVec[2]};
	

		case(offset) matches
			0:
				return line[191:112];
			1: 
				return line[183:104];
			2:
				return line[175:96];
			3:
				return line[167:88];
			4:
				return line[159:80];
			5: 
				return line[151:72];
			6:
				return line[143:64];
			7:
				return line[135:56];
		endcase
	endmethod

endmodule

