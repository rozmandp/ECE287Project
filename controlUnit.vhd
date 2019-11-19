library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use ieee.numeric_std.all;

ENTITY controlUnit IS 
           PORT( 
			  clk : in std_logic;
			  reset : in std_logic;
			  inst : in std_logic;
			  decodeEn : out std_logic;
			  regWEn: out std_logic;
			  aluEn : out std_logic;
			  end controlUnit;

architecture behavior of controlUnit is
           TYPE state IS (Fetch, Decode, regRead, ALU, memory, regWrite)
           signal currState : state := Decode;
			  signal nextState : state;
			  begin
			  if reset = '1' then
        currState <= Decode;
			  elsif rising_edge(clk) then
            currState <= nextState;
        case currState is
          when Decode =>
			   regWrite = 0;
			   decodeEn = 1;
            nextState <= regRead;
          when regRead =>
			   decodeEn = 0;
            nextState <= ALU;
          when ALU =>
			   aluEn = 1;
            nextState <= regWrite;
          when regWrite =>
			   aluEn = 0;
				regWrite = 1;
            nextState <= Decode;
          when others =>
            nextState <= Decode;
        end case;
      end if;
    end if;
  end process;
end Behavioral; 