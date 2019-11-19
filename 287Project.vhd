library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use ieee.numeric_std.all;

 

entity alu is
port (  -- the alu connections to external circuitry:
  A  : in  std_logic_vector(7 downto 0);   -- operand A
  B  : in  std_logic_vector(7 downto 0);   -- operand B
  en : in std_logic; -- enable
  in_we : in  std_logic;  -- Input Write Enable
  OP : in  std_logic_vector(4 downto 0); -- opcode
  Y  : out std_logic_vector(7 downto 0);  -- operation result
  imm : in std_logic_vector(7 downto 0); -- immediate data
  branch : out std_logic; -- Branch flag
  out_we : out std_logic; -- out write enable
  clk : IN STD_LOGIC);
end alu;

architecture behavioral of alu is
signal output : std_logic_vector(8 downto 0);
signal shouldBranch : std_logic;
begin
	process(A,B,OP,output)
	begin
	if rising_edge(clk) and en = '1' then
	 out_we <= in_we;
    case (OP(4 downto 1)) is  -- decode the opcode and perform the operation:
    when "0000" =>
	if op(0) = '0' then
	output <= std_logic_vector(unsigned('0' & A) + unsigned(B));   -- unsigned add
	else
	output <= std_logic_vector(signed(A(7) & A) + signed( B(7) & B));
   end if;
   shouldBranch <= '0';
 when "0001" =>
	if op(0) = '0' then
	output <= std_logic_vector(unsigned('0' & A) - unsigned(B));   -- unsigned add
	else
	output <= std_logic_vector(signed(A(7) & A) - signed( B(7) & B));
   end if;
	shouldBranch <= '0';
    when "0010" =>  output(7 downto 0) <= A;
    --when "0011" =>  -- TO DO: Write
    when "0100" =>  output(7 downto 0) <= not A;   -- 1's complement
    when "0101" =>  output(7 downto 0) <=  A and B; -- bitwise AND
    when "0110" =>  output(7 downto 0) <= A or B;  -- bitwise OR
    when "0111" =>  output(7 downto 0) <= A xor B; -- bitwise XOR
	 when "1001" => output(7 downto 0) <= imm;
	 -- when "1010" => output(7 downto 0) <= A = B;
	 when "1011" =>  output(7 downto 0) <= std_logic_vector(shift_left(signed(A), 1));
	 when "1100" =>  output(7 downto 0) <= std_logic_vector(shift_right(signed(A), 1));
	 --when "1101" =>  -- TO DO: Jump
	 --when "1110" =>  -- TO Do: Jump Conditionally
    when others => output <= (others => 'X');
  end case; 
  end if;
  end process;
  Y <= output(7 downto 0);
  branch <= shouldBranch;
end behavioral;



library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;


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
		GENERIC(
						d_width	:	INTEGER := 8;    --width of each data word
						size		:	INTEGER := 64);  --number of data words the memory can store
		port( input : in  std_logic_vector(15 downto 0);
				ps2_clk    : IN  STD_LOGIC;                     --clock signal from PS2 keyboard
				ps2_data   : IN  STD_LOGIC;                     --data signal from PS2 keyboard
				ie : in STD_LOGIC;
				clr : in STD_LOGIC;
				clk_50MHz : IN STD_LOGIC;
				output : out std_logic_vector(7 downto 0));
		end ECE287project;
		
architecture behavior of ECE287project is
     COMPONENT alu
	          PORT(A,B : in std_logic_vector(7 downto 0);
				      OP : in std_logic_vector(3 downto 0);
						Y  : out std_logic_vector(7 downto 0);
						clk : IN STD_LOGIC);
	  end COMPONENT;
	  
	  COMPONENT register8
	          PORT( d   : IN STD_LOGIC_VECTOR(7 DOWNTO 0);
                   ld  : IN STD_LOGIC; -- load/enable.
                   clr : IN STD_LOGIC; -- async. clear.
                   clk : IN STD_LOGIC; -- clock.
                   q   : OUT STD_LOGIC_VECTOR(7 DOWNTO 0)); -- output
		end COMPONENT;
		
		
		COMPONENT HZ1_CLK 
		         port (  CLK_50MHz : in  std_logic;          -- clock signal
		                 CLK_1HZ : out std_logic);
		end COMPONENT;
		
		COMPONENT ps2_keyboard_to_ascii IS
				GENERIC(
						clk_freq                  : INTEGER := 50_000_000; --system clock frequency in Hz
						ps2_debounce_counter_size : INTEGER := 8);         --set such that 2^size/clk_freq = 5us (size = 8 for 50MHz)
				PORT(
						clk        : IN  STD_LOGIC;                     --system clock input
						ps2_clk    : IN  STD_LOGIC;                     --clock signal from PS2 keyboard
						ps2_data   : IN  STD_LOGIC;                     --data signal from PS2 keyboard
						ascii_new  : OUT STD_LOGIC;                     --output flag indicating new ASCII value
						ascii_code : OUT STD_LOGIC_VECTOR(6 DOWNTO 0)); --ASCII value
		END COMPONENT ps2_keyboard_to_ascii;
		
		COMPONENT ram IS
				GENERIC(
						d_width	:	INTEGER := 8;    --width of each data word
						size		:	INTEGER := 64);  --number of data words the memory can store
				PORT(
						clk		:	IN		STD_LOGIC;                             --system clock
						wr_ena	:	IN		STD_LOGIC;                             --write enable
						addr		:	IN		INTEGER RANGE 0 TO size-1;             --address to write/read
						data_in	:	IN		STD_LOGIC_VECTOR(d_width-1 DOWNTO 0);  --input data to write
						data_out	:	OUT	STD_LOGIC_VECTOR(d_width-1 DOWNTO 0)); --output data read
				END COMPONENT ram;
				
				COMPONENT decode is
    Port ( clk : in STD_LOGIC;
           I_dataInst : in  STD_LOGIC_VECTOR (15 downto 0);
           I_en : in  STD_LOGIC;
           O_selA : out  STD_LOGIC_VECTOR (2 downto 0);
           O_selB : out  STD_LOGIC_VECTOR (2 downto 0);
           O_selD : out  STD_LOGIC_VECTOR (2 downto 0);
           O_dataIMM : out  STD_LOGIC_VECTOR (7 downto 0);
           O_regDwe : out  STD_LOGIC;
           O_aluop : out  STD_LOGIC_VECTOR (4 downto 0));
end COMPONENT decode;

COMPONENT controlUnit IS 
           PORT( 
			  clk : in std_logic;
			  reset : in std_logic;
			  inst : in std_logic;
			  decodeEn : out std_logic;
			  regWEn: out std_logic;
			  aluEn : out std_logic);
			  end COMPONENT controlUnit;
		
		signal ascii_new  :  STD_LOGIC;                     --output flag indicating new ASCII value
		signal ascii_code :  STD_LOGIC_VECTOR(6 DOWNTO 0); --ASCII value
		signal out1 : std_logic_vector(7 DOWNTO 0);
		signal out2 : std_logic_vector(7 DOWNTO 0);
		signal instRegOut : std_logic_vector(3 Downto 0);
		signal decodeEn : std_logic;
	   signal regWEn: std_logic;
		signal aluEn : std_logic;                         --write enable
		signal addr :	INTEGER RANGE 0 TO size-1;             --address to write/read
		signal data_in	:	STD_LOGIC_VECTOR(d_width-1 DOWNTO 0);  --input data to write
		signal data_out	:	STD_LOGIC_VECTOR(d_width-1 DOWNTO 0); --output data read
		signal clk : std_logic;
	  
	  begin
			 -- Keyboard : ps2_keyboard_to_ascii PORT MAP(clk_50MHz, ps2_clk, ps2_data, ascii_new, ascii_code);
			 --RAMKey : ram PORT MAP (clk_50MHz, ascii_new,addr, '0' & ascii_code, output(7 downto 0));
	       -- CLk1 : HZ1_Clk PORT MAP (clk_50MHz, clk);
	       --RegisterA : register8 PORT MAP(input, le1, clr, clk_50MHz, out1);
			 --RegisterB : register8 PORT MAP(input, le2, clr, clk_50MHz, out2);
			 --RegisterInst : register3 PORT MAP(inst, ie, clr, clk_50MHz, instRegOut);
	       --ALU1: alu PORT MAP(out1,out2,instRegOut,output, clk_50MHz);
	  end behavior;