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
  clk : IN STD_LOGIC;
  display : out std_logic); -- display flag);
end alu;

architecture behavioral of alu is
constant CMP_BIT_EQ:  integer := 7;
constant CMP_BIT_AGB: integer := 6;
constant CMP_BIT_ALB: integer := 5;
constant CMP_BIT_AZ:  integer := 4;
constant CMP_BIT_BZ:  integer := 3;
constant CJF_EQ: std_logic_vector(2 downto 0):= "000";
constant CJF_AZ: std_logic_vector(2 downto 0):= "001";
constant CJF_BZ: std_logic_vector(2 downto 0):= "010";
constant CJF_ANZ: std_logic_vector(2 downto 0):= "011";
constant CJF_BNZ: std_logic_vector(2 downto 0):= "100";
constant CJF_AGB: std_logic_vector(2 downto 0):= "101";
constant CJF_ALB: std_logic_vector(2 downto 0):= "110";
signal output : std_logic_vector(8 downto 0);
signal shouldBranch : std_logic;
begin
	process(A,B,OP,output, clk, en)
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
	display <= '0';
 when "0001" =>
	if op(0) = '0' then
	output <= std_logic_vector(unsigned('0' & A) - unsigned(B));   -- unsigned subtract
	else
	output <= std_logic_vector(signed(A(7) & A) - signed( B(7) & B));
   end if;
	shouldBranch <= '0';
	display <= '0';
    when "0010" =>  
	 output(7 downto 0) <= A;
	 shouldBranch <= '0';
    when "0011" => 
	 output(7 downto 0) <= A; -- TO DO: Write
	 shouldBranch <= '0';
    when "0100" => 
	 output(7 downto 0) <= not A;   -- 1's complement
	 shouldBranch <= '0';
    when "0101" => 
	 output(7 downto 0) <=  A and B; -- bitwise AND
	 shouldBranch <= '0';
    when "0110" => 
	 output(7 downto 0) <= A or B;  -- bitwise OR
	 shouldBranch <= '0';
    when "0111" =>  
	 output(7 downto 0) <= A xor B; -- bitwise XOR
	 shouldBranch <= '0';
	 when "1000" =>  
	 output(7 downto 0) <= A;  
	 display <= '1';
	 shouldBranch <= '0';
	 when "1001" => 
	 output(7 downto 0) <= imm;
	 shouldBranch <= '0';
	 when "1010" => 
	 if A = B then
    output(CMP_BIT_EQ) <= '1';
  else
    output(CMP_BIT_EQ) <= '0';
  end if;
 
  if A = "00000000" then
    output(CMP_BIT_AZ) <= '1';
  else
    output(CMP_BIT_AZ) <= '0';
  end if;
 
  if B = "00000000" then
    output(CMP_BIT_BZ) <= '1';
  else
    output(CMP_BIT_BZ) <= '0';
	 
  end if;
  if OP(0) = '0' then
  if unsigned(A) > unsigned(B) then
    output(CMP_BIT_AGB) <= '1';
  else
    output(CMP_BIT_AGB) <= '0';
  end if;
  if unsigned(A) < unsigned(B) then
    output(CMP_BIT_ALB) <= '1';
  else
    output(CMP_BIT_ALB) <= '0';
  end if;
else
  if signed(A) > signed(B) then
    output(CMP_BIT_AGB) <= '1';
  else
    output(CMP_BIT_AGB) <= '0';
  end if;
  if signed(A) < signed(B) then
    output(CMP_BIT_ALB) <= '1';
  else
    output(CMP_BIT_ALB) <= '0';
  end if;
end if;
shouldBranch <= '0';
	 when "1011" =>  
	 output(7 downto 0) <= std_logic_vector(shift_left(signed(A), 1));
	 shouldBranch <= '0';
	 when "1100" =>  
	 output(7 downto 0) <= std_logic_vector(shift_right(signed(A), 1));
	 shouldBranch <= '0';
	 when "1101" =>  
	 output(7 downto 0) <= imm;
	 shouldBranch <= '1';
	 when "1110" =>   -- set branch target regardless
  output(7 downto 0) <= B;
   -- the condition to jump is based on aluop(0) and dataimm(1 downto 0);
  case (OP(0) & imm(1 downto 0)) is
    when CJF_EQ =>
      shouldBranch <= A(CMP_BIT_EQ);
    when CJF_AZ =>
      shouldBranch <= A(CMP_BIT_Az);
    when CJF_BZ =>
      shouldBranch <= A(CMP_BIT_Bz);
    when CJF_ANZ =>
      shouldBranch <= not A(CMP_BIT_AZ);
    when CJF_BNZ =>
      shouldBranch <= not A(CMP_BIT_Bz);
    when CJF_AGB =>
      shouldBranch <= A(CMP_BIT_AGB);
    when CJF_ALB =>
      shouldBranch <= A(CMP_BIT_ALB);
    when others =>
        shouldBranch <= '0';
  end case;
    when "1111" => 
	 output(7 downto 0) <= std_logic_vector(unsigned(A) + 1);
	 shouldBranch <= '0';
    when others => output <= (others => 'X');
  end case; 
  end if;
  end process;
  Y <= output(7 downto 0);
  branch <= shouldBranch;
end behavioral;