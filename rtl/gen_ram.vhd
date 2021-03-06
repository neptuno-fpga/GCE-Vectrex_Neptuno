--
-- Multicore 2 / Multicore 2+
--
-- Copyright (c) 2017-2020 - Victor Trucco
--
-- All rights reserved
--
-- Redistribution and use in source and synthezised forms, with or without
-- modification, are permitted provided that the following conditions are met:
--
-- Redistributions of source code must retain the above copyright notice,
-- this list of conditions and the following disclaimer.
--
-- Redistributions in synthesized form must reproduce the above copyright
-- notice, this list of conditions and the following disclaimer in the
-- documentation and/or other materials provided with the distribution.
--
-- Neither the name of the author nor the names of other contributors may
-- be used to endorse or promote products derived from this software without
-- specific prior written permission.
--
-- THIS CODE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
-- AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
-- THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
-- PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE
-- LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
-- CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
-- SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
-- INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
-- CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
-- ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
-- POSSIBILITY OF SUCH DAMAGE.
--
-- You are responsible for any legal issues arising from your use of this code.
--
		
-- -----------------------------------------------------------------------
--
-- Syntiac's generic VHDL support files.
--
-- -----------------------------------------------------------------------
-- Copyright 2005-2008 by Peter Wendrich (pwsoft@syntiac.com)
-- http://www.syntiac.com/fpga64.html
--
-- Modified April 2016 by Dar (darfpga@aol.fr) 
-- http://darfpga.blogspot.fr
--   Remove address register when writing
--
-- -----------------------------------------------------------------------
--
-- gen_rwram.vhd
--
-- -----------------------------------------------------------------------
--
-- generic ram.
--
-- -----------------------------------------------------------------------

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.numeric_std.ALL;

-- -----------------------------------------------------------------------

entity gen_ram is
	generic (
		dWidth : integer := 8;
		aWidth : integer := 10
	);
	port (
		clk : in std_logic;
		we : in std_logic;
		addr : in std_logic_vector((aWidth-1) downto 0);
		d : in std_logic_vector((dWidth-1) downto 0);
		q : out std_logic_vector((dWidth-1) downto 0)
	);
end entity;

-- -----------------------------------------------------------------------

architecture rtl of gen_ram is
	subtype addressRange is integer range 0 to ((2**aWidth)-1);
	type ramDef is array(addressRange) of std_logic_vector((dWidth-1) downto 0);
	signal ram: ramDef;

	signal rAddrReg : std_logic_vector((aWidth-1) downto 0);
	signal qReg : std_logic_vector((dWidth-1) downto 0);
begin
-- -----------------------------------------------------------------------
-- Signals to entity interface
-- -----------------------------------------------------------------------
--	q <= qReg;

-- -----------------------------------------------------------------------
-- Memory write
-- -----------------------------------------------------------------------
	process(clk)
	begin
		if rising_edge(clk) then
			if we = '1' then
				ram(to_integer(unsigned(addr))) <= d;
			end if;
		end if;
	end process;
	
-- -----------------------------------------------------------------------
-- Memory read
-- -----------------------------------------------------------------------
process(clk)
	begin
		if rising_edge(clk) then
--			qReg <= ram(to_integer(unsigned(rAddrReg)));
--			rAddrReg <= addr;
----			qReg <= ram(to_integer(unsigned(addr)));
      q <= ram(to_integer(unsigned(addr)));
		end if;
	end process;
--q <= ram(to_integer(unsigned(addr)));
end architecture;

