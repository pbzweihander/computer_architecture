import Vector::*;

import "BDPI"
function Action setSeed(Bit#(64) seed);
import "BDPI"
function ActionValue#(Bit#(64)) getRandom();

interface Rand#(numeric type n);
  method ActionValue#(Bit#(n)) get;
endinterface

module mkRand#(Bit#(64) seed)(Rand#(n)) provisos(Div#(n, 64, num), Add#(a, n, TMul#(num, 64)));
  Reg#(Bool) init <- mkReg(False);

  rule initialize(!init);
    setSeed(seed);
    init <= True;
  endrule

  method ActionValue#(Bit#(n)) get if(init);
    Vector#(num, Bit#(64)) rands = newVector;
    for(Integer i = 0; i < valueOf(num); i = i + 1)
      rands[i] <- getRandom;
    return truncate(pack(rands));
  endmethod
endmodule
