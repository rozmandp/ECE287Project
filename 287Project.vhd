library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;
 

entity alu is
port (  -- the alu connections to external circuitry:
  A  : in  std_logic_vector(7 downto 0);   -- operand A
  B  : in  std_logic_vector(7 downto 0);   -- operand B
  OP : in  std_logic_vector(3 downto 0); -- opcode
  Y  : out std_logic_vector(15 downto 0);  -- operation result
  clk : IN STD_LOGIC);
end alu;

architecture behavioral of alu is
signal output : std_logic_vector(15 downto 0);
begin
	process(A,B,OP,output)
	begin
	if rising_edge(clk) then
    case (OP) is  -- decode the opcode and perform the operation:
    when "0000" =>  output <= ("00000000" & A) + B;   -- add
    when "0001" =>  output <= ("00000000" & A) - B;   -- subtract
    when "0010" =>  output <= ("00000000" & A) - 1;   -- decrement
    when "0011" =>  output <= ("00000000" & A) + 1;   -- increment
    when "0100" =>  output <= not ("00000000" & A);   -- 1's complement
    when "0101" =>  output <= ("00000000" & A) and B; -- bitwise AND
    when "0110" =>  output <= ("00000000" & A) or B;  -- bitwise OR
    when "0111" =>  output <= ("00000000" & A) xor B; -- bitwise XOR
	 when "1000" =>  output <= A * B;                  -- multiplication
    when others => output <= (others => 'X');
  end case; 
  end if;
  end process;
  Y <= output;
end behavioral;

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;

ENTITY register8 IS PORT(
    d   : IN STD_LOGIC_VECTOR(7 DOWNTO 0);
    ld  : IN STD_LOGIC; -- load/enable.
    clr : IN STD_LOGIC; -- async. clear.
    clk : IN STD_LOGIC; -- clock.
    q   : OUT STD_LOGIC_VECTOR(7 DOWNTO 0) -- output
);
END register8;

ARCHITECTURE description OF register8 IS

BEGIN
    process(clk, clr)
    begin
        if clr = '1' then
            q <= "00000000";
        elsif rising_edge(clk) then
            if ld = '1' then
                q <= d;
            end if;
        end if;
    end process;
END description;

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;

ENTITY register3 IS PORT(
    d   : IN STD_LOGIC_VECTOR(3 DOWNTO 0);
    ld  : IN STD_LOGIC; -- load/enable.
    clr : IN STD_LOGIC; -- async. clear.
    clk : IN STD_LOGIC; -- clock.
    q   : OUT STD_LOGIC_VECTOR(3 DOWNTO 0) -- output
);
END register3;

ARCHITECTURE description OF register3 IS

BEGIN
    process(clk, clr)
    begin
        if clr = '1' then
            q <= "0000";
        elsif rising_edge(clk) then
            if ld = '1' then
                q <= d;
            end if;
        end if;
    end process;
END description;

library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.STD_LOGIC_ARITH.all;
use IEEE.STD_LOGIC_UNSIGNED.all;

entity HZ1_Clk is
   port (
      CLK_50MHz : in  std_logic;          -- clock signal
		CLK_1HZ : out std_logic);
end HZ1_Clk;

architecture Behavior of HZ1_Clk is
   signal counter : std_logic_vector(24 downto 0);  -- signal that does the
   signal clk : std_logic;                                                -- counting

begin 

   Prescaler: process (CLK_50MHz)
   begin  -- process Prescaler

      if CLK_50MHz'event and CLK_50MHz = '1' then  -- rising clock edge
         if counter < "1011111010111100001000000" then   -- Binary value is
                                                         -- 25e6
            counter <= counter + 1;
         else
            clk <= not clk;
            counter <= (others => '0');
         end if;
      end if;
   end process Prescaler;
	
   CLK_1HZ <= clk;
end Behavior;



library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;

entity ECE287project is
		port( input : in  std_logic_vector(7 downto 0);
				inst : in std_logic_vector(3 downto 0);
				le1 : in STD_LOGIC;
				le2 : in STD_LOGIC;
				ie : in STD_LOGIC;
				clr : in STD_LOGIC;
				clk_50MHz : IN STD_LOGIC;
				output : out std_logic_vector(15 downto 0));
		end ECE287project;
		
architecture behavior of ECE287project is
     COMPONENT alu
	          PORT(A,B : in std_logic_vector(7 downto 0);
				      OP : in std_logic_vector(3 downto 0);
						Y  : out std_logic_vector(15 downto 0);
						clk : IN STD_LOGIC);
	  end COMPONENT;
	  
	  COMPONENT register8
	          PORT( d   : IN STD_LOGIC_VECTOR(7 DOWNTO 0);
                   ld  : IN STD_LOGIC; -- load/enable.
                   clr : IN STD_LOGIC; -- async. clear.
                   clk : IN STD_LOGIC; -- clock.
                   q   : OUT STD_LOGIC_VECTOR(7 DOWNTO 0)); -- output
		end COMPONENT;
		
		COMPONENT register3
	          PORT( d   : IN STD_LOGIC_VECTOR(3 DOWNTO 0);
                   ld  : IN STD_LOGIC; -- load/enable.
                   clr : IN STD_LOGIC; -- async. clear.
                   clk : IN STD_LOGIC; -- clock.
                   q   : OUT STD_LOGIC_VECTOR(3 DOWNTO 0)); -- output
		end COMPONENT;
		
		COMPONENT HZ1_CLK 
		         port (  CLK_50MHz : in  std_logic;          -- clock signal
		                 CLK_1HZ : out std_logic);
		end COMPONENT;
		
		
		signal out1 : std_logic_vector(7 DOWNTO 0);
		signal out2 : std_logic_vector(7 DOWNTO 0);
		signal instRegOut : std_logic_vector(3 Downto 0);
		signal clk : std_logic;
	  
	  begin 
	       CLk1 : HZ1_Clk PORT MAP (clk_50MHz, clk);
	       RegisterIn1 : register8 PORT MAP(input, le1, clr, clk_50MHz, out1);
			 RegisterIn2 : register8 PORT MAP(input, le2, clr, clk_50MHz, out2);
			 RegisterInst : register3 PORT MAP(inst, ie, clr, clk_50MHz, instRegOut);
	       ALU1: alu PORT MAP(out1,out2,instRegOut,output, clk_50MHz);
	  end behavior;