library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use ieee.numeric_std.all;

ENTITY controlUnit IS 
           PORT( 
			  clk : in std_logic;
			  reset : in std_logic;
			  inst : in std_logic_vector(4 downto 0);
			  decodeEn : out std_logic;
			  regREn: out std_logic;
			  regWEn: out std_logic;
			  aluEn : out std_logic;
			  memoryEn : out std_logic;
			  fetchEn : out std_logic);
			  end controlUnit;

architecture behavior of controlUnit is
           TYPE state IS (Fetch, Decode, regRead, ALU, memory, regWrite);
           signal currState : state := Fetch;
			  signal nextState : state;
			  begin
			  process (clk, reset, currState)
			  begin
			  if reset = '1' then
        currState <= Fetch;
			  else
            currState <= nextState;
				end if;
				if  rising_edge(clk) then
        case currState is
		    when Fetch =>
				nextState <= Decode;
          when Decode =>
            nextState <= regRead;
          when regRead =>	   
            nextState <= ALU;
          when ALU =>
				if( inst(4 downto 1)  = "0010" or inst(4 downto 1) = "0011") then
            nextState <= memory;
				else
				nextState <= regWrite;
				end if;
		    when memory =>
			 nextState <= regWrite;
          when regWrite =>
            nextState <= Fetch;
          when others =>
            nextState <= Fetch;
        end case;
      end if;
		--if rising_edge(clk) then
		case currState is
		when Fetch =>
		regWEn <= '0';
		fetchEn <= '1';
		decodeEn <= '0';
		regREn <= '0';
		aluEn <= '0';
		memoryEn <= '0';
		when Decode =>
		regWEn <= '0';
		fetchEn <= '0';
		decodeEn <= '1';
		regREn <= '0';
		aluEn <= '0';
		memoryEn <= '0';
		when regRead =>
		regWEn <= '0';
		fetchEn <= '0';
		decodeEn <= '0';
		regREn <= '1';
		aluEn <= '0';
		memoryEn <= '0';
		when ALU =>
		regWEn <= '0';
		fetchEn <= '0';
		decodeEn <= '0';
		regREn <= '0';
		aluEn <= '1';
		memoryEn <= '0';
		when memory =>
		regWEn <= '0';
		fetchEn <= '0';
		decodeEn <= '0';
		regREn <= '0';
		aluEn <= '0';
		memoryEn <= '1';
		when regWrite =>
		regWEn <= '1';
		fetchEn <= '0';
		decodeEn <= '0';
		regREn <= '0';
		aluEn <= '0';
		memoryEn <= '0';
		end case;
		--end if;
  end process;
end behavior; 