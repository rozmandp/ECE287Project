library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;
 

entity alu is
port (  -- the alu connections to external circuitry:
  A  : in  std_logic_vector(7 downto 0);   -- operand A
  B  : in  std_logic_vector(7 downto 0);   -- operand B
  OP : in  std_logic_vector(2 downto 0); -- opcode
  Y  : out std_logic_vector(7 downto 0));  -- operation result
end alu;

architecture behavioral of alu is
signal output : std_logic_vector(7 downto 0);
begin
	process(A,B,OP,output)
	begin
  case (OP) is  -- decode the opcode and perform the operation:
    when "000" =>  output <= A + B;   -- add
    when "001" =>  output <= A - B;   -- subtract
    when "010" =>  output <= A - 1;   -- decrement
    when "011" =>  output <= A + 1;   -- increment
    when "100" =>  output <= not A;   -- 1's complement
    when "101" =>  output <= A and B; -- bitwise AND
    when "110" =>  output <= A or B;  -- bitwise OR
    when "111" =>  output <= A xor B; -- bitwise XOR
    when others => output <= (others => 'X');
  end case; 
  end process;
  Y <= output;
end behavioral;

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;

entity ECE287project is
		port( in1 : in  std_logic_vector(7 downto 0);
				in2 : in  std_logic_vector(7 downto 0);
		      inst : in std_logic_vector(2 downto 0);
				output : out std_logic_vector(7 downto 0));
		end ECE287project;
		
architecture behavior of ECE287project is
     COMPONENT alu
	          PORT(A,B : in std_logic_vector(7 downto 0);
				      OP : in std_logic_vector(2 downto 0);
						Y  : out std_logic_vector(7 downto 0));
	  end COMPONENT;
	  
	  begin 
	       ALU1: alu PORT MAP(in1,in2,inst,output);
	  end behavior;