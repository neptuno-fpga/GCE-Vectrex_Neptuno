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
  
*///============================================================================
//
//  Multicore 2+ Top by Victor Trucco
//
//============================================================================
// 2020 12 16 - Fix by Oduvaldo: Pot Y for joy 1 and 2 were swapped, fixed it

`default_nettype none

module vectrex_np1
(
     // Clocks
    input wire  clock_50_i,

    // Buttons
    //input wire [4:1]    btn_n_i,

    // SRAM (IS61WV102416BLL-10TLI)
    output wire [19:0]sram_addr_o  = 20'b00000000000000000000,
    inout wire  [15:0]sram_data_io   = 8'bzzzzzzzzbzzzzzzzz,
    output wire sram_we_n_o     = 1'b1,
    output wire sram_oe_n_o     = 1'b1,
    output wire sram_ub_n_o     = 1'b1,
    output wire sram_lb_n_o     = 1'b1,
        
    // SDRAM (W9825G6KH-6)
    output [12:0] SDRAM_A,
    output  [1:0] SDRAM_BA,
    inout  [15:0] SDRAM_DQ,
    output        SDRAM_DQMH,
    output        SDRAM_DQML,
    output        SDRAM_CKE,
    output        SDRAM_nCS,
    output        SDRAM_nWE,
    output        SDRAM_nRAS,
    output        SDRAM_nCAS,
    output        SDRAM_CLK,

    // PS2
    inout wire  ps2_clk_io        = 1'bz,
    inout wire  ps2_data_io       = 1'bz,
    inout wire  ps2_mouse_clk_io  = 1'bz,
    inout wire  ps2_mouse_data_io = 1'bz,

    // SD Card
    output wire sd_cs_n_o         = 1'bZ,
    output wire sd_sclk_o         = 1'bZ,
    output wire sd_mosi_o         = 1'bZ,
    input wire  sd_miso_i,

    // Joysticks
    output wire joy_clock_o       = 1'b1,
    output wire joy_load_o        = 1'b1,
    input  wire joy_data_i,
    output wire joy_p7_o          = 1'b1,

    // Audio
    output      AUDIO_L,
    output      AUDIO_R,
    input wire  ear_i,
    //output wire mic_o             = 1'b0,
    //  I2S
    output		SCLK,
    output		LRCLK,
    output		SDIN,		

    // VGA
    output  [5:0] VGA_R,
    output  [5:0] VGA_G,
    output  [5:0] VGA_B,
    output        VGA_HS,
    output        VGA_VS,

    //STM32
    input wire  stm_tx_i,
    output wire stm_rx_o,
    output wire stm_rst_o           = 1'bz, // '0' to hold the microcontroller reset line, to free the SD card
   
    input         SPI_SCK,
    output        SPI_DO,
    input         SPI_DI,
    input         SPI_SS2,
    //output wire   SPI_nWAIT        = 1'b1, // '0' to hold the microcontroller data streaming

    //inout [31:0] GPIO,

    output LED                    = 1'b1 // '0' is LED on
);


//---------------------------------------------------------
//-- MC2+ defaults
//---------------------------------------------------------

//assign GPIO = 32'bzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz;
assign stm_rst_o    = 1'bZ;
assign stm_rx_o = 1'bZ;

//no SRAM for this core
assign sram_we_n_o  = 1'b1;
assign sram_oe_n_o  = 1'b1;

//all the SD reading goes thru the microcontroller for this core
assign sd_cs_n_o = 1'bZ;
assign sd_sclk_o = 1'bZ;
assign sd_mosi_o = 1'bZ;

wire joy1_up_i, joy1_down_i, joy1_left_i, joy1_right_i, joy1_p6_i, joy1_p9_i;
wire joy2_up_i, joy2_down_i, joy2_left_i, joy2_right_i, joy2_p6_i, joy2_p9_i;

// joystick_serial  joystick_serial 
// (
    // .clk_i           ( clk_12 ),
    // .joy_data_i      ( joy_data_i ),
    // .joy_clk_o       ( joy_clock_o ),
    // .joy_load_o      ( joy_load_o ),

    // .joy1_up_o       ( joy1_up_i ),
    // .joy1_down_o     ( joy1_down_i ),
    // .joy1_left_o     ( joy1_left_i ),
    // .joy1_right_o    ( joy1_right_i ),
    // .joy1_fire1_o    ( joy1_p6_i ),
    // .joy1_fire2_o    ( joy1_p9_i ),

    // .joy2_up_o       ( joy2_up_i ),
    // .joy2_down_o     ( joy2_down_i ),
    // .joy2_left_o     ( joy2_left_i ),
    // .joy2_right_o    ( joy2_right_i ),
    // .joy2_fire1_o    ( joy2_p6_i ),
    // .joy2_fire2_o    ( joy2_p9_i )
// );

joydecoder joystick_serial  (
    .clk          ( clk_12 ), 
    .joy_data     ( joy_data_i ),
    .joy_clk      ( joy_clock_o ),
    .joy_load     ( joy_load_o ),
	 .clock_locked ( pll_locked ),

    .joy1up       ( joy1_up_i ),
    .joy1down     ( joy1_down_i ),
    .joy1left     ( joy1_left_i ),
    .joy1right    ( joy1_right_i ),
    .joy1fire1    ( joy1_p6_i ),
    .joy1fire2    ( joy1_p9_i ),

    .joy2up       ( joy2_up_i ),
    .joy2down     ( joy2_down_i ),
    .joy2left     ( joy2_left_i ),
    .joy2right    ( joy2_right_i ),
    .joy2fire1    ( joy2_p6_i ),
    .joy2fire2    ( joy2_p9_i )
); 



//-- END defaults -------------------------------------------------------


localparam CONF_STR = {
    "S,BIN/VEC/ROM,Load Game...;",
    "O1,CPU,MC6809,CPU09;",
    "O2,Show Frame,Yes,No;",
    "O3,Skip Logo,Yes,No;",
    "O4,Joystick swap,Off,On;",
    "O5,Second port,Joystick,Speech;",
//  "O23,Phosphor persistance,1,2,3,4;",
//  "O8,Overburn,No,Yes;",  
    "T6,Reset;",
    "V,v1.50."
};

wire [31:0] status;
wire  [1:0] buttons;
wire  [1:0] switches;
wire  [15:0] kbjoy;
wire  [7:0] joystick_0;
wire  [7:0] joystick_1;
wire [15:0] joy_ana_0;
wire [15:0] joy_ana_1;
wire        ypbpr;
wire        ps2_kbd_clk, ps2_kbd_data;
wire  [7:0] pot_x_1, pot_x_2;
wire  [7:0] pot_y_1, pot_y_2;
wire  [9:0] audio;
wire            hs, vs, cs;
wire  [3:0] r, g, b;
wire            hb, vb;
wire        blankn = ~(hb | vb);
wire            cart_rd;
wire [14:0] cart_addr;
wire  [7:0] cart_do;
wire        ioctl_downl;
wire  [7:0] ioctl_index;
wire        ioctl_wr;
wire [24:0] ioctl_addr;
wire  [7:0] ioctl_dout;


assign LED = !ioctl_downl;

wire            clk_24, clk_12;
wire            pll_locked;

pll pll (
    .inclk0         ( clock_50_i    ),
    .areset         ( 0             ),
    .c0             ( clk_24        ),
    .c1             ( clk_12        ),
    .c2             ( SDRAM_CLK     ),
    .locked         ( pll_locked    )
    );

//assign SDRAM_CLK = clk_24;
wire [15:0] sdram_do;
assign cart_do = sdram_do[7:0];

sdram cart
(
    .*,
    .init(~pll_locked),
    .clk(clk_24),
    .wtbt(2'b00),
    .dout(sdram_do),
    .din ({ioctl_dout, ioctl_dout}),
    .addr(ioctl_downl ? ioctl_addr : cart_addr),
    .we(ioctl_downl & ioctl_wr),
    .rd(!ioctl_downl & cart_rd),
    .ready()
);

reg reset = 0;
reg second_reset = 0;

always @(posedge clk_24) begin
    integer timeout = 0;
    reg [15:0] reset_counter = 0;
    reg reset_start;

    reset <= 0;
    //reset_start <= status[6] | ~btn_n_i[4] | ioctl_downl | second_reset;
	reset_start <= status[6] | ioctl_downl | second_reset;
    if (reset_counter) begin
        reset <= 1'b1;
        reset_counter <= reset_counter - 1'd1;
    end
    if (reset_start) reset_counter <= 16'd1000;

    second_reset <= 0;
    if (timeout) begin
        timeout <= timeout - 1;
        if(timeout == 1) second_reset <= 1'b1;
    end
    if(ioctl_downl && !status[3]) timeout <= 5000000;
end

reg signed [7:0] pot_x; 
reg signed [7:0] pot_y; 
reg signed [7:0] pot2_x;
reg signed [7:0] pot2_y;

assign pot_x_1 = status[4] ? pot2_x : pot_x;
assign pot_x_2 = status[4] ? pot_x  : pot2_x;
assign pot_y_1 = status[4] ? pot2_y : pot_y;
assign pot_y_2 = status[4] ? pot_y  : pot2_y;


always @(posedge clk_12)
begin
    
        pot_x <= 8'h00;
        pot_y <= 8'h00;
            
        if (m_up)    pot_y <= 8'h7F; 
        if (m_down)  pot_y <= 8'h80; 
        if (m_left)  pot_x <= 8'h80; 
        if (m_right) pot_x <= 8'h7F; 

        if (m_up2)    pot2_y <= 8'h7F; 
        if (m_down2)  pot2_y <= 8'h80; 
        if (m_left2)  pot2_x <= 8'h80; 
        if (m_right2) pot2_x <= 8'h7F; 

end 

vectrex vectrex (
    .clock_24       ( clk_24            ),  
    .clock_12       ( clk_12        ),
    .reset          ( reset             ),
    .cpu            ( status[1]         ),
    .video_r            ( rr                ),
    .video_g            ( gg                ),
    .video_b            ( bb                ),
    .video_csync    ( cs                ),
    .video_hblank   ( hb                ),
    .video_vblank   ( vb                ),
    .speech_mode    ( status[5]     ),
    .video_hs       ( hs                ),
    .video_vs       ( vs                ),
    .frame          ( frame_line    ),
    .audio_out      ( audio         ),
    .cart_addr      ( cart_addr     ),
    .cart_do            ( cart_do       ),
    .cart_rd            ( cart_rd       ),
                                                         //MXYZSACB
                                                         // gfedcba
    .btn11          ( status[4] ? m_fire2G : m_fireG),
    .btn12          ( status[4] ? m_fire2C : m_fireC),
    .btn13          ( status[4] ? m_fire2A : m_fireA),
    .btn14          ( status[4] ? m_fire2B : m_fireB),
    .pot_x_1        ( pot_x_1           ),
    .pot_y_1        ( pot_y_1           ),
    .btn21          ( status[4] ? m_fireG : m_fire2G),
    .btn22          ( status[4] ? m_fireC : m_fire2C),
    .btn23          ( status[4] ? m_fireA : m_fire2A),
    .btn24          ( status[4] ? m_fireB : m_fire2B),
    .pot_x_2        ( pot_x_2           ),
    .pot_y_2        ( pot_y_2           ),
    .leds               (                   ),
    .dbg_cpu_addr   (                   )
    );

dac #(10) dac (
    .clk_i          ( clk_24            ),
    .res_n_i        ( 1             ),
    .dac_i          ( audio         ),
    .dac_o          ( AUDIO_L       )
    );
assign AUDIO_R = AUDIO_L;

//i2s audio
wire MCLK;

audio_top i2s
(
	.clk_50MHz(clock_50_i),
	.dac_MCLK (MCLK),
	.dac_LRCK (LRCLK),
	.dac_SCLK (SCLK),
	.dac_SDIN (SDIN),
	.L_data   ({audio, 6'b000000}),
	.R_data   ({audio, 6'b000000})
);

//////////////////   VIDEO   //////////////////

wire frame_line;
wire [3:0] rr,gg,bb;

assign r = status[2] & frame_line ? 4'h4 : blankn ? rr : 4'd0;
assign g = status[2] & frame_line ? 4'h0 : blankn ? gg : 4'd0;
assign b = status[2] & frame_line ? 4'h0 : blankn ? bb : 4'd0;

//mist_video #(.COLOR_DEPTH(4)) mist_video
//(
//    .clk_sys(clk_24),
//    .SPI_DI(SPI_DI),
//    .SPI_SCK(SPI_SCK),
//    .SPI_SS3(SPI_SS2),
//    .scandoubler_disable(1),
//    .rotate(2'b00),
//    .HSync(hs),
//    .VSync(vs),
//    .R(r),
//    .G(g),
//    .B(b),
//    .VGA_HS(VGA_HS),
//    .VGA_VS(VGA_VS),
//    .VGA_R(VGA_R),
//    .VGA_G(VGA_G),
//    .VGA_B(VGA_B),
//    .osd_enable ( osd_enable )
//);

wire [5:0]vga_r_s;
wire [5:0]vga_g_s;
wire [5:0]vga_b_s;

osd osd (
    // OSDs pixel clock, should be synchronous to cores pixel clock to
    // avoid jitter.
    .clk_sys(clk_24),
    .ce(1),

    // SPI interface
    .SPI_DI(SPI_DI),
    .SPI_SCK(SPI_SCK),
    .SPI_SS3(SPI_SS2),

    .rotate (2'b00), //[0] - rotate [1] - left or right

    // VGA signals coming from core
    .HSync(hs),
    .VSync(vs),
    .R_in({r, r[3:2]}),
    .G_in({g, g[3:2]}),
    .B_in({b, b[3:2]}),

    // VGA signals going to video connector

    .R_out(vga_r_s),
    .G_out(vga_g_s),
    .B_out(vga_b_s),
    .osd_enable ( osd_enable )
    

);
// assign VGA_R  = vga_r_s[5:1];
// assign VGA_G  = vga_g_s[5:1];
// assign VGA_B  = vga_b_s[5:1];
assign VGA_R  = vga_r_s;
assign VGA_G  = vga_g_s;
assign VGA_B  = vga_b_s;
assign VGA_HS = hs;
assign VGA_VS = vs;

    /*
////////////////////////////////////////////
user_io #(.STRLEN(($size(CONF_STR)>>3))) user_io (
    .clk_sys       ( clk_24       ),
    .conf_str      ( CONF_STR     ),
    .SPI_CLK       ( SPI_SCK      ),
    .SPI_SS_IO     ( CONF_DATA0   ),
    .SPI_MISO      ( SPI_DO       ),
    .SPI_MOSI      ( SPI_DI       ),
    .buttons       ( buttons      ),
    .switches      ( switches     ),
    .ypbpr         ( ypbpr        ),
    .ps2_kbd_clk   ( ps2_kbd_clk  ),
    .ps2_kbd_data  ( ps2_kbd_data ),
    .joystick_0    ( joystick_0   ),
    .joystick_1    ( joystick_1   ),
    .joystick_analog_0( joy_ana_0 ),
    .joystick_analog_1( joy_ana_1 ),
    .status        ( status       )
    );
*/

data_io #(
    .STRLEN(($size(CONF_STR)>>3)))
data_io(
    .clk_sys       ( clk_24       ),
    .SPI_SCK       ( SPI_SCK      ),
    .SPI_SS2       ( SPI_SS2      ),
    .SPI_DI        ( SPI_DI       ),
    .SPI_DO        ( SPI_DO       ),
    
    .data_in       ( keys_s ),
    .conf_str      ( CONF_STR     ),
    .status        ( status       ),

    .ioctl_download( ioctl_downl  ),
    .ioctl_index   ( ioctl_index  ),
    .ioctl_wr      ( ioctl_wr     ),
    .ioctl_addr    ( ioctl_addr   ),
    .ioctl_dout    ( ioctl_dout   )
    );


//-----------------------

wire m_up, m_down, m_left, m_right, m_fireA, m_fireB, m_fireC, m_fireD, m_fireE, m_fireF, m_fireG;
wire m_up2, m_down2, m_left2, m_right2, m_fire2A, m_fire2B, m_fire2C, m_fire2D, m_fire2E, m_fire2F, m_fire2G;
wire m_tilt, m_coin1, m_coin2, m_coin3, m_coin4, m_one_player, m_two_players, m_three_players, m_four_players;

wire m_right4, m_left4, m_down4, m_up4, m_right3, m_left3, m_down3, m_up3;

// wire btn_one_player =   ~btn_n_i[1] | m_one_player;
// wire btn_two_players =  ~btn_n_i[2] | m_two_players;
// wire btn_coin  =        ~btn_n_i[3] | m_coin1;
wire btn_one_player =  m_one_player;
wire btn_two_players = m_two_players;
wire btn_coin  =       m_coin1;


wire kbd_intr;
wire [7:0] kbd_scancode;
wire [7:0] keys_s;

//get scancode from keyboard
io_ps2_keyboard keyboard 
 (
  .clk       ( clk_12 ),
  .kbd_clk   ( ps2_clk_io ),
  .kbd_dat   ( ps2_data_io ),
  .interrupt ( kbd_intr ),
  .scancode  ( kbd_scancode )
);

wire [15:0]joy1_s;
wire [15:0]joy2_s;
wire [8:0]controls_s;
wire osd_enable, direct_video;

//translate scancode to joystick
//kbd_joystick #( .OSD_CMD    ( 3'b011 ), .CLK_SPEED (12000)) k_joystick
//(
//    .clk          ( clk_12 ),
//    .kbdint       ( kbd_intr ),
//    .kbdscancode  ( kbd_scancode ), 
//    .direct_video ( direct_video ),
//
//    .joystick_0     ({ joy1_p9_i, joy1_p6_i, joy1_up_i, joy1_down_i, joy1_left_i, joy1_right_i }),
//    .joystick_1     ({ joy2_p9_i, joy2_p6_i, joy2_up_i, joy2_down_i, joy2_left_i, joy2_right_i }),
//      
//    //-- joystick_0 and joystick_1 should be swapped
//    .joyswap        ( 0 ),
//
//    //-- player1 and player2 should get both joystick_0 and joystick_1
//    .oneplayer      ( 0 ),
//
//    //-- tilt, coin4-1, start4-1
//    .controls    ( {m_tilt, m_coin4, m_coin3, m_coin2, m_coin1, m_four_players, m_three_players, m_two_players, m_one_player} ),
//
//    //-- fire12-1, up, down, left, right
//
//    .player1     ( {m_fireG,  m_fireF, m_fireE, m_fireD, m_fireC, m_fireB, m_fireA, m_up, m_down, m_left, m_right} ),
//    .player2     ( {m_fire2G, m_fire2F, m_fire2E, m_fire2D, m_fire2C, m_fire2B, m_fire2A, m_up2, m_down2, m_left2, m_right2} ),
//
//    //-- keys to the OSD
//    .osd_o         ( keys_s ),
//    .osd_enable     ( osd_enable ),
//
//    //-- sega joystick
//    .sega_strobe    ( joy_p7_o )
//        
//        
//); 

kbd_joystick_ua #( .OSD_CMD    ( 3'b011 ), .USE_VKP (1'b0)) k_joystick
(
    .clk          ( clk_12 ),
    .kbdint       ( kbd_intr ),
    .kbdscancode  ( kbd_scancode ), 
    .direct_video ( direct_video ),

    .joystick_0     ({ joy1_p9_i, joy1_p6_i, joy1_up_i, joy1_down_i, joy1_left_i, joy1_right_i }),
    .joystick_1     ({ joy2_p9_i, joy2_p6_i, joy2_up_i, joy2_down_i, joy2_left_i, joy2_right_i }),
      
    //-- joystick_0 and joystick_1 should be swapped
    .joyswap        ( 0 ),

    //-- player1 and player2 should get both joystick_0 and joystick_1
    .oneplayer      ( 0 ),

    //-- tilt, coin4-1, start4-1
    .controls    ( {m_tilt, m_coin4, m_coin3, m_coin2, m_coin1, m_four_players, m_three_players, m_two_players, m_one_player} ),

    //-- fire12-1, up, down, left, right

    .player1     ( {m_fireG,  m_fireF, m_fireE, m_fireD, m_fireC, m_fireB, m_fireA, m_up, m_down, m_left, m_right} ),
    .player2     ( {m_fire2G, m_fire2F, m_fire2E, m_fire2D, m_fire2C, m_fire2B, m_fire2A, m_up2, m_down2, m_left2, m_right2} ),

    //-- keys to the OSD
    .osd_o         ( keys_s ),
    .osd_enable     ( osd_enable ),

    //-- sega joystick
    .sega_clk     ( hs ),		 
    .sega_strobe    ( joy_p7_o )
);	 
	 
endmodule 