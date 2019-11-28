--------------------------------------------------------------------------------
--
--   FileName:         ram.vhd
--   Dependencies:     none
--   Design Software:  Quartus Prime Version 17.0.0 Build 595 SJ Lite Edition
--
--   HDL CODE IS PROVIDED "AS IS."  DIGI-KEY EXPRESSLY DISCLAIMS ANY
--   WARRANTY OF ANY KIND, WHETHER EXPRESS OR IMPLIED, INCLUDING BUT NOT
--   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
--   PARTICULAR PURPOSE, OR NON-INFRINGEMENT. IN NO EVENT SHALL DIGI-KEY
--   BE LIABLE FOR ANY INCIDENTAL, SPECIAL, INDIRECT OR CONSEQUENTIAL
--   DAMAGES, LOST PROFITS OR LOST DATA, HARM TO YOUR EQUIPMENT, COST OF
--   PROCUREMENT OF SUBSTITUTE GOODS, TECHNOLOGY OR SERVICES, ANY CLAIMS
--   BY THIRD PARTIES (INCLUDING BUT NOT LIMITED TO ANY DEFENSE THEREOF),
--   ANY CLAIMS FOR INDEMNITY OR CONTRIBUTION, OR OTHER SIMILAR COSTS.
--
--   Version History
--   Version 1.0 9/14/2018 Scott Larson
--     Initial Public Release
--    
--------------------------------------------------------------------------------

LIBRARY ieee;
USE ieee.std_logic_1164.all;

ENTITY ram IS
	GENERIC(
		d_width	:	INTEGER := 16;    --width of each data word
		size		:	INTEGER := 64);  --number of data words the memory can store
	PORT(
		clk		:	IN		STD_LOGIC;                             --system clock
		wr_ena	:	IN		STD_LOGIC;                             --write enable
		addr		:	IN		INTEGER RANGE 0 TO size-1;             --address to write/read
		data_in	:	IN		STD_LOGIC_VECTOR(d_width-1 DOWNTO 0);  --input data to write
		data_out	:	OUT	STD_LOGIC_VECTOR(d_width-1 DOWNTO 0)); --output data read
END ram;

ARCHITECTURE logic OF ram IS
   constant opcode_add : std_logic_vector(3 downto 0) := "0000";
	constant opcode_subtract : std_logic_vector(3 downto 0) := "0001";
	constant opcode_read : std_logic_vector(3 downto 0) := "0010";
	constant opcode_write : std_logic_vector(3 downto 0) := "0011";
	constant opcode_inverse : std_logic_vector(3 downto 0) := "0100";
	constant opcode_and : std_logic_vector(3 downto 0) := "0101";
	constant opcode_or : std_logic_vector(3 downto 0) := "0110";
	constant opcode_xor : std_logic_vector(3 downto 0) := "0111";
	constant opcode_display : std_logic_vector(3 downto 0) := "1000";
	constant opcode_load : std_logic_vector(3 downto 0) := "1001";
	constant opcode_cmp : std_logic_vector(3 downto 0) := "1010";
	constant opcode_shiftLeft : std_logic_vector(3 downto 0) := "1011";
	constant opcode_shiftRight : std_logic_vector(3 downto 0) := "1100";
	constant opcode_jump : std_logic_vector(3 downto 0) := "1101";
	constant opcode_condJump : std_logic_vector(3 downto 0) := "1110";
	constant opcode_increment : std_logic_vector(3 downto 0) := "1111";
	
	
	TYPE memory IS ARRAY(0 to size-1) OF STD_LOGIC_VECTOR(d_width-1 DOWNTO 0);  --data type for memory
	SIGNAL ram			:	memory := (
	opcode_jump & "000" & "0" & X"01",
	opcode_load & "001" & '1' & X"0A",
	opcode_load & "010" & '1' & X"0C",
	opcode_load & "011" & '1' & "00000000",
	opcode_load & "100" & '1' & "00000000",
	opcode_load & "110" & '1' & "00000101",
	opcode_add & "100" & '0' & "001" & "100" & "00",
	opcode_increment & "011" & '1' & "011" & "00000",
	opcode_cmp & "101" & '0' &  "011" & "010" & "00",
	opcode_condJump & "000" & '1' & "101" & "110" & "10",
	opcode_display & "000" & '0' & "100" & "00000",
	--opcode_jump & "000" & "0" & X"00",
	opcode_jump & "000" & "0" & X"0B",
	others => X"0000");
	SIGNAL addr_int	:	INTEGER RANGE 0 TO size-1;                                 --internal address register
BEGIN

	PROCESS(clk)
	BEGIN
		IF(clk'EVENT AND clk = '1') THEN

			IF(wr_ena = '1') THEN     --write enable is asserted
				ram(addr) <= data_in;  --write input data into memory
			END IF;
			
			addr_int <= addr;         --store the address in the internal address register

		END IF;	
	END PROCESS;
	
	data_out <= ram(addr_int);      --output data at the stored address
	
END logic;