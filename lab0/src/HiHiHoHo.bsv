package HiHiHoHo;
    String hi = "Hi";
    String ho = "Ho";
    String bye = "Bye!";

    (* synthesize *)
    module mkHiHiHoHo(Empty);
        Reg#(Bit#(3)) counter <- mkReg(0);
        rule inc_counter;
            counter <= counter + 1;
        endrule

        rule say_hi(counter == 0 || counter == 1);
            $display(hi);
        endrule

        rule say_ho(counter == 2 || counter == 3);
            $display(ho);
        endrule

        rule finish(counter == 4);
            $display(bye);
            $finish;
        endrule
    endmodule
endpackage
