---------------------------------------------------------------------------
-- instruction_memory.vhd - Implementation of A Single-Port, 16 x 16-bit
--                          Instruction Memory.
-- 
-- Notes: refer to headers in single_cycle_core.vhd for the supported ISA.
--
-- Copyright (C) 2006 by Lih Wen Koh (lwkoh@cse.unsw.edu.au)
-- All Rights Reserved. 
--
-- The single-cycle processor core is provided AS IS, with no warranty of 
-- any kind, express or implied. The user of the program accepts full 
-- responsibility for the application of the program and the use of any 
-- results. This work may be downloaded, compiled, executed, copied, and 
-- modified solely for nonprofit, educational, noncommercial research, and 
-- noncommercial scholarship purposes provided that this notice in its 
-- entirety accompanies all copies. Copies of the modified software can be 
-- delivered to persons who use it solely for nonprofit, educational, 
-- noncommercial research, and noncommercial scholarship purposes provided 
-- that this notice in its entirety accompanies all copies.
--
---------------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;

entity instruction_memory is
    port ( reset    : in  std_logic;
           clk      : in  std_logic;
           addr_in  : in  std_logic_vector(7 downto 0);
           insn_out : out std_logic_vector(31 downto 0) );
end instruction_memory;

architecture behavioral of instruction_memory is

type mem_array is array(0 to 255) of std_logic_vector(31 downto 0);
signal sig_insn_mem : mem_array;

begin
    mem_process: process (reset, addr_in ) is
  
    variable var_insn_mem : mem_array;
    variable var_addr     : integer;
  
  
  
  ------------------ instructions -------------------
--	BNE   R1 R2 imm 	compare reg R1, R2 and branch by PC + 1 + imm(signed)
--	ADD   R1 R2 R3  	add content in R1 and R2 and write to R3
--	SLRi  R1 R2 imm 	left shift content in reg R1 by imm and write to R2
--	SOW   R1 R2 imm	    POA[R1+imm] <- R2
--	STB   R1 X  X	    copy pattern number in R1 into pattern number register (so that R2 in LPA instruction is relative to offset) 
--	LPA   R1 R2 R3	    load pattern character (if R2 < R1, R3 <- PA[R2] and R2++, else R3 <- -1), R1 should be $5
--	LLA   R1 R2 R3	    load pattern length array, same as above, R1 should be $9
--	LOA   R1 R2 R3	    load pattern occurence array, same as above, R1 should be $9
--	MVI	  R1 X  R2	    R2 <- reg_file[R1]
--	END   X  X  X	    signal end of function
--	JP    X  X  imm     jump to PC + 1 + offset

--constant OP_SOW     : std_logic_vector(7 downto 0) := X"02"; --6
--constant OP_STB     : std_logic_vector(7 downto 0) := X"04"; --a
--constant OP_LPA     : std_logic_vector(7 downto 0) := X"05"; --c
--constant OP_LLA     : std_logic_vector(7 downto 0) := X"07"; --6
--constant OP_LOA     : std_logic_vector(7 downto 0) := X"09"; --a
--constant OP_MVI     : std_logic_vector(7 downto 0) := X"0b"; --c
---constant OP_END     : std_logic_vector(7 downto 0) := X"0d"; --6
--constant OP_JP      : std_logic_vector(7 downto 0) := X"0e"; --a
------------------------------------------------------
    begin
        if (reset = '1') then
            var_insn_mem := (others => X"00000000");
            var_insn_mem(0):= X"090a0b0c";
            var_insn_mem(1):= X"0d000000";
            -- TODO ADD INSTRUCTIONS
        
        else
            var_addr := conv_integer(addr_in);
            insn_out <= var_insn_mem(var_addr);
        end if;

        -- the following are probe signals (for simulation purpose)
        sig_insn_mem <= var_insn_mem;
    end process;
  
end behavioral;
