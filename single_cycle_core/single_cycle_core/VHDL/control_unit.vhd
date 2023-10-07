---------------------------------------------------------------------------
-- control_unit.vhd - Control Unit Implementation
--
-- Notes: refer to headers in single_cycle_core.vhd for the supported ISA.
--
--  control signals:
--     reg_dst    : asserted for ADD instructions, so that the register
--                  destination number for the 'write_register' comes from
--                  the rd field (bits 3-0). 
--     reg_write  : asserted for ADD and LOAD instructions, so that the
--                  register on the 'write_register' input is written with
--                  the value on the 'write_data' port.
--     alu_src    : asserted for LOAD and STORE instructions, so that the
--                  second ALU operand is the sign-extended, lower 4 bits
--                  of the instruction.
--     mem_write  : asserted for STORE instructions, so that the data 
--                  memory contents designated by the address input are
--                  replaced by the value on the 'write_data' input.
--     mem_to_reg : asserted for LOAD instructions, so that the value fed
--                  to the register 'write_data' input comes from the
--                  data memory.
--
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

entity control_unit is
    port ( opcode     : in  std_logic_vector(7 downto 0);
           control_bus : out std_logic_vector(11 downto 0); -- used as signal for forwarding detection
           control_bus_new : out std_logic_vector(8 downto 0);
           ready    : out std_logic);
end control_unit;

architecture behavioural of control_unit is

constant OP_LOAD  : std_logic_vector(7 downto 0) := X"01";
constant OP_STORE : std_logic_vector(7 downto 0) := X"03";
constant OP_ADD   : std_logic_vector(7 downto 0) := X"08";

-- new opcodes
constant OP_BNE     : std_logic_vector(7 downto 0) := X"06"; 
constant OP_LA      : std_logic_vector(7 downto 0) := X"0a"; 
constant OP_SLRi    : std_logic_vector(7 downto 0) := X"0c"; 

-- more new opcodes
constant OP_SOW     : std_logic_vector(7 downto 0) := X"02"; --6
constant OP_STB     : std_logic_vector(7 downto 0) := X"04"; --a
constant OP_LPA     : std_logic_vector(7 downto 0) := X"05"; --c
constant OP_LLA     : std_logic_vector(7 downto 0) := X"07"; --6
constant OP_LOA     : std_logic_vector(7 downto 0) := X"09"; --a
constant OP_MVI     : std_logic_vector(7 downto 0) := X"0b"; --c
constant OP_END     : std_logic_vector(7 downto 0) := X"0d"; --6
constant OP_JP      : std_logic_vector(7 downto 0) := X"0e"; --a

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
------------------------------------------------------


------------------ may not need -----------------
--	LW 	  R1 R2 imm	    don't think we need
--	SW	  R1 R2 imm	    don't think we need
--	SR	  X  X  X		don't think we need, shift input string registers
-------------------------------------------------

begin
    --- control bus list ---
        --- control for WB stage ---
            -- 0 | c_wb_out_of_bound
            -- 1 | c_wb_data1
            -- 2 | c_wb_wreg1
            -- 3 | c_wb_wreg2
        
        --- control for MEM stage ---
            -- 4 | c_mem_address
            -- 5 | c_mem_write
            
        --- control for EX stage ---
            -- 6 | c_ex_result
            -- 7 | c_ex_data_b
            -- 8 | c_ex_array_op
            -- (this stage will modify c_wb_wreg2 and c_wb_out_of_bound)
            
        --- control for ID stage ---
            -- 9 | c_id_branch_enable
            -- 10| c_id_extension
            -- 11| c_id_wreg1_sel     
    ---------------------------------------
    control_bus(0) <= '0'; -- c_wb_out_of_bound determined in EX stage 
    control_bus(1) <= '1' when (opcode = OP_LOAD or 
                                opcode = OP_LA or 
                                opcode = OP_LPA or
                                opcode = OP_LLA or
                                opcode = OP_LOA) else '0';
    control_bus(2) <= '1' when (opcode = OP_LOAD or
                                opcode = OP_ADD or
                                opcode = OP_LA or 
                                opcode = OP_SLRi or
                                opcode = OP_LPA or
                                opcode = OP_LLA or
                                opcode = OP_LOA or
                                opcode = OP_MVI) else '0';
    control_bus(3) <= '0'; -- c_wb_wreg2 determined in EX stage 
    control_bus(4) <= '1' when (opcode = OP_LA or
                                opcode = OP_LPA or
                                opcode = OP_LLA or
                                opcode = OP_LOA) else '0';
    control_bus(5) <= '1' when opcode = OP_STORE else '0';
    control_bus(6) <= '1' when opcode = OP_SLRi else '0';
    control_bus(7) <= '1' when (opcode = OP_LOAD or
                                opcode = OP_STORE or
                                opcode = OP_SLRi or 
                                opcode = OP_SOW) else '0';
    control_bus(8) <= '1' when (opcode = OP_LA or
                                opcode = OP_LPA or
                                opcode = OP_LLA or
                                opcode = OP_LOA) else '0';
    control_bus(9) <= '1' when opcode = OP_BNE else '0';
    control_bus(10) <= '1' when opcode = OP_SLRi else '0';
    control_bus(11) <= '1' when (opcode = OP_ADD or
                                 opcode = OP_LA or
                                 opcode = OP_LPA or
                                 opcode = OP_LLA or
                                 opcode = OP_LOA or
                                 opcode = OP_MVI) else '0';
    ---------- control bus list 2 -----------------
        -- controls for mem stage
            -- 0 | c_mem_read_char
            -- 1 | c_mem_read_len
            -- 2 | c_mem_read_occur
            -- 3 | c_mem_write_char
            -- 4 | c_mem_write_len
            -- 5 | c_mem_write_occur
            -- 6 | c_mem_wreg_pn
        -- controls for id stage
            -- 7 | c_id_indir_read_a
            -- 8 | c_id_jump
    ------------------------------------------------
    control_bus_new(0) <= '1' when opcode = OP_LPA else '0';
    control_bus_new(1) <= '1' when opcode = OP_LLA else '0';
    control_bus_new(2) <= '1' when opcode = OP_LOA else '0';
    control_bus_new(3) <= '0'; -- currently hard code
    control_bus_new(4) <= '0'; -- currently hard code
    control_bus_new(5) <= '1' when (opcode = OP_SOW) else '0';
    control_bus_new(6) <= '1' when opcode = OP_STB else '0';
    control_bus_new(7) <= '1' when opcode = OP_MVI else '0';
    control_bus_new(8) <= '1' when opcode = OP_JP else '0';
    
    ready <= '1' when opcode = OP_END else '0';

end behavioural;
