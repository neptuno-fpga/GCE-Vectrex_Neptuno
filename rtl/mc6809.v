/*
  
   Multicore 2 / Multicore 2+
  
   Copyright (c) 2017-2020 - Victor Trucco

  
   All rights reserved
  
   Redistribution and use in source and synthezised forms, with or without
   modification, are permitted provided that the following conditions are met:
  
   Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.
  
   Redistributions in synthesized form must reproduce the above copyright
   notice, this list of conditions and the following disclaimer in the
   documentation and/or other materials provided with the distribution.
  
   Neither the name of the author nor the names of other contributors may
   be used to endorse or promote products derived from this software without
   specific prior written permission.
  
   THIS CODE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
   AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
   THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
   PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE
   LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
   CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
   SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
   INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
   CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
   ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
   POSSIBILITY OF SUCH DAMAGE.
  
   You are responsible for any legal issues arising from your use of this code.
  
*/`timescale 1ns / 1ps
module mc6809
(
	input         CLK,
	input         CLKEN,
	input         nRESET,

	input         CPU,

	output reg    E,
	output reg    riseE,
	output reg    fallE, // everything except interrupts/dma registered/latched here

	output reg    Q,
	output reg    riseQ,
	output reg    fallQ, // NMI,IRQ,FIRQ,DMA,HALT registered here

	input   [7:0] Din,
	output  [7:0] Dout,
	output [15:0] ADDR,
	output        RnW,

	input         nIRQ,
	input         nFIRQ,
	input         nNMI,
	input         nHALT
);

cpu09 cpu1
(
	.clk(CLK),
	.ce(fallE),
	.rst(~nRESET | CPU),
	.addr(ADDR1),
	.rw(RnW1),
	.data_out(Dout1),
	.data_in(Din),
	.irq(~nIRQ),
	.firq(~nFIRQ),
	.nmi(~nNMI),
	.halt(~nHALT)
);

mc6809is cpu2
(
	.CLK(CLK),
	.D(Din),
	.DOut(Dout2),
	.ADDR(ADDR2),
	.RnW(RnW2),
	.fallE_en(fallE),
	.fallQ_en(fallQ),
	.nIRQ(nIRQ),
	.nFIRQ(nFIRQ),
	.nNMI(nNMI),
	.nHALT(nHALT),
	.nRESET(nRESET & CPU),
	.nDMABREQ(1)
);

wire  [7:0] Dout1,Dout2;
wire [15:0] ADDR1,ADDR2;
wire        RnW1,RnW2;

assign Dout = CPU ? Dout2 : Dout1;
assign ADDR = CPU ? ADDR2 : ADDR1;
assign RnW  = CPU ? RnW2  : RnW1;

always @(posedge CLK)
begin
	reg [1:0] clk_phase =0;

	fallE <= 0;
	fallQ <= 0;
	riseE <= 0;
	riseQ <= 0;

	if (CLKEN) begin
		clk_phase <= clk_phase + 1'd1;
		case (clk_phase)
			2'b00: begin E <= 0; fallE <= 1; end
			2'b01: begin Q <= 1; riseQ <= 1; end
			2'b10: begin E <= 1; riseE <= 1; end
			2'b11: begin Q <= 0; fallQ <= 1; end
		endcase
	end
end

endmodule
