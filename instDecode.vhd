library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use ieee.numeric_std.all;

entity decode is
    Port ( clk : in STD_LOGIC;
           I_dataInst : in  STD_LOGIC_VECTOR (15 downto 0);
           I_en : in  STD_LOGIC;
           O_selA : out  STD_LOGIC_VECTOR (2 downto 0);
           O_selB : out  STD_LOGIC_VECTOR (2 downto 0);
           O_selD : out  STD_LOGIC_VECTOR (2 downto 0);
           O_dataIMM : out  STD_LOGIC_VECTOR (7 downto 0);
           O_regDwe : out  STD_LOGIC;
           O_aluop : out  STD_LOGIC_VECTOR (4 downto 0));
end decode;
 
architecture Behavioral of decode is
 
begin
 
  process (clk, I_en)
  begin
    if rising_edge(clk) and I_en = '1' then
 
      O_selA <= I_dataInst(7 downto 5);
      O_selB <= I_dataInst(4 downto 2);
      O_selD <= I_dataInst(11 downto 9);
      O_dataIMM <= I_dataInst(7 downto 0);
      O_aluop <= I_dataInst(15 downto 12) & I_dataInst(8);
 
      case I_dataInst(15 downto 12) is
        when "0011" =>   -- WRITE
          O_regDwe <= '0';
        when "1101" =>   -- JUMP
          O_regDwe <= '0';
        when "1110" =>   -- JUMPEQ
          O_regDwe <= '0';
        when others =>
          O_regDwe <= '1';
      end case;
    end if;
  end process;
 
end Behavioral;