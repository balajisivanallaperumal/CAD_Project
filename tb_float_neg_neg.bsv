package tb_float;

import PDFPM::*;
import ClientServer::*;
import GetPut::*;

(*synthesize*)
module mktb_float(Empty);
 
    Server#(Tuple3#(Double, Double, RoundMode), Tuple2#(Double, Exception)) multiply <- mkFloatingPointMultiplier();
        
     Reg#(Bit#(2)) rg_state <- mkReg(0);
     Reg#(Bit#(8)) counter <- mkReg(0);

    rule inc_counter;
        counter <= counter + 1;
    endrule

    rule state1(rg_state ==0);
    
	Double v1;
	v1.sign = False;
	v1.exp = 'b10000000101;
	v1.sfd = 'b1001000000000000000000000000000000000000000000000000;
	
	Double v2;
	v2.sign = False;
	v2.exp = 'b10000000111;
	v2.sfd = 'b0010110000000000000000000000000000000000000000000000;
	
	multiply.request.put(tuple3(v1,v2,Rnd_Nearest_Even));
	
	rg_state <= 1; 
    endrule
    
    rule state2(rg_state ==1);
        
        let {v,w} <- multiply.response.get();
        $display(" Counter:%d",counter);
        $display(" Mul response:",fshow(v));
        $display(" Mul response binary: %b",v);
        rg_state <= 2;
        
    endrule
    
    rule state3(rg_state == 2);
        $finish();
    endrule

endmodule

endpackage:tb_float
