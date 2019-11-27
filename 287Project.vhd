library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use ieee.numeric_std.all;


entity ECE287project is
		GENERIC(
						d_width	:	INTEGER := 16;    --width of each data word
						size		:	INTEGER := 64);  --number of data words the memory can store
		port( reset : in STD_LOGIC;
				clk : IN STD_LOGIC;
				sevenSeg1 : out std_logic_vector(6 downto 0);
				sevenSeg2 : out  std_logic_vector(6 downto 0);
				programStage : out std_logic_vector(5 downto 0);
				dataOut : out std_logic_vector(7 downto 0);
				sevenSegAddr1 : out std_logic_vector(6 downto 0);
				sevenSegAddr2 : out std_logic_vector(6 downto 0);
				sevenSegAddr3 : out std_logic_vector(6 downto 0);
				sevenSegAddr4 : out std_logic_vector(6 downto 0);
				opcodeOut : out std_logic_vector(4 downto 0));
		end ECE287project;
		
architecture behavior of ECE287project is
		
		COMPONENT ram IS
				GENERIC(
						d_width	:	INTEGER := 16;    --width of each data word
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

			  
			  COMPONENT pc_unit is
   Port ( I_clk : in  STD_LOGIC;
           I_nPC : in  STD_LOGIC_VECTOR (15 downto 0);
           reset : in std_LOGIC;
			  shouldBranch : in std_LOGIC;
			  regWEn : in std_LOGIC;
           O_PC : out  STD_LOGIC_VECTOR (15 downto 0)
           );
end COMPONENT pc_unit;

COMPONENT alu is
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
  display : out std_logic);
  end COMPONENT alu;
  
  COMPONENT reg8by8 is
        Port( clk : in std_logic;
		        en : in std_logic;
				  dataD : in std_logic_vector(7 downto 0);
				  selA: in STD_LOGIC_VECTOR (2 downto 0);
				  selB : in STD_LOGIC_VECTOR (2 downto 0);
				  selD : in STD_LOGIC_VECTOR (2 downto 0);
				  we : in std_logic;
				  dataA : out std_logic_vector(7 downto 0);
				  dataB : out std_logic_vector(7 downto 0));
				  end COMPONENT reg8by8;
				  
   COMPONENT controlUnit IS 
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
			  end COMPONENT controlUnit;
			  

COMPONENT to_7seg is
    Port ( A : in  STD_LOGIC_VECTOR (3 downto 0);
	        en : in std_logic;
			  clk : in std_logic;
          seg7 : out  STD_LOGIC_VECTOR (6 downto 0)
             );
end COMPONENT to_7seg;

COMPONENT clockDivider is
   port (
      CLK_50MHz : in  std_logic;          -- clock signal
      clk      : out std_logic);        
end COMPONENT clockDivider;

		constant PCU_OP_NOP: std_logic_vector(1 downto 0):= "00";
      constant PCU_OP_INC: std_logic_vector(1 downto 0):= "01";
      constant PCU_OP_ASSIGN: std_logic_vector(1 downto 0):= "10";
      constant PCU_OP_RESET: std_logic_vector(1 downto 0):= "11";
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
		signal decodeEn : std_logic;
	   signal regWEn: std_logic;
		signal regREn: std_logic;
		signal aluEn : std_logic;
		signal memoryEn : std_logic;
		signal fetchEn : std_logic;
		signal ramAddr :	INTEGER RANGE 0 TO size-1;             --address to write/read
		signal ramWData	:	STD_LOGIC_VECTOR(d_width-1 DOWNTO 0);  --input data to write
		signal ramRData	:	STD_LOGIC_VECTOR(d_width-1 DOWNTO 0); --output data read
		signal ramWE : std_logic := '0';
		signal pcop : std_logic_vector(1 downto 0);
	   signal aluop : std_logic_vector(4 downto 0);
		signal instruction : std_logic_vector(15 downto 0);
		signal selA : std_logic_vector(2 downto 0);
		signal selB : std_logic_vector(2 downto 0);
		signal selD : std_logic_vector(2 downto 0);
		signal regWE : std_logic;
		signal dataImm : std_logic_vector(7 downto 0);
		signal dataALUWE : std_logic;
		signal dataWriteReg : std_logic;
		signal dataResult : std_logic_vector(7 downto 0);
		signal shouldBranch : std_logic;
		signal dataA : std_logic_vector(7 downto 0);
		signal dataB : std_logic_vector(7 downto 0);
		signal PC: std_logic_vector(15 downto 0);
      signal in_pc: std_logic_vector(15 downto 0);
		signal registerWriteData : std_logic_vector(7 downto 0);
		signal display : std_logic := '0';
		--signal clk : std_logic;
		signal displayData : std_LOGIC_VECTOR(7 downto 0);
		
		
	  begin
	     --clockDivider1 : clockDivider PORT MAP (clk_50MHz, clk);
		  ramAddr <= (to_integer(unsigned(dataResult))) when memoryEn = '1' else (to_integer(unsigned(PC)));
        ramWData <= X"00" & dataResult;
		  in_pc <= X"00" & dataResult;
        ramWE <= '1' when memoryEn = '1' and aluop(4 downto 1) = "0011" else '0';
        registerWriteData <= ramRData(7 downto 0) when regWEn = '1' and aluop(4 downto 1) = "0010" else dataResult;
        instruction <= ramRData;
		  decoder : decode PORT MAP (clk, instruction, decodeEn, selA, selB, selD, dataIMM, dataALUWE, aluop);
		  cpu_alu : alu PORT MAP (dataA, dataB, aluEn, dataALUWE, aluop, dataResult, dataIMM, shouldBranch, dataWriteReg, clk, display);
		  cpu_controlUnit : controlUnit PORT MAP( clk, reset, aluop, decodeEn, regREn, regWEn, aluEn, memoryEn, fetchEn);
		  regFile : reg8by8 PORT MAP(clk, regREn or regWEn, registerWriteData, selA, selB, selD, dataWriteReg and regWEn, dataA, dataB);
		  cpu_PC : pc_Unit PORT MAP(clk, in_PC,reset,shouldBranch,regWEn, PC);
		  cpu_ram : ram PORT MAP(clk, ramWE, ramAddr, ramWData, ramRData);
		  sevenSegCon1 : to_7seg PORT MAP (displayData(7 downto 4), display, clk, sevenSeg1);
		  sevenSegCon2 : to_7seg PORT MAP (displayData(3 downto 0), display, clk, sevenSeg2);
		  sevenSegAdr1 : to_7seg PORT MAP (PC(15 downto 12), '1', clk, sevenSegAddr1);
		  sevenSegAdr2 : to_7seg PORT MAP (PC(11 downto 8), '1', clk, sevenSegAddr2);
		  sevenSegAdr3 : to_7seg PORT MAP (PC(7 downto 4), '1', clk, sevenSegAddr3);
		  sevenSegAdr4 : to_7seg PORT MAP (PC(3 downto 0), '1', clk, sevenSegAddr4);
		  programStage(5) <= fetchEn;
		  programStage(4) <= decodeEn;
		  programStage(3) <= regREn;
		  programStage(2) <= aluEn;
		  programStage(1) <= memoryEn;
		  programStage(0) <= regWEn;
		  dataOut <= dataResult;
		  opcodeOut <= aluop;
		  displayData <= dataResult when display = '1' and aluop(4 downto 1) = opcode_Display;
	  end behavior;