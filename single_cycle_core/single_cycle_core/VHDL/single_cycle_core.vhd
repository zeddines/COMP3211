---------------------------------------------------------------------------
-- single_cycle_core.vhd - A Single-Cycle Processor Implementation
--
-- Notes : 
--
-- See single_cycle_core.pdf for the block diagram of this single
-- cycle processor core.
--
-- Instruction Set Architecture (ISA) for the single-cycle-core:
--   Each instruction is 16-bit wide, with four 4-bit fields.
--
--     noop      
--        # no operation or to signal end of program
--        # format:  | opcode = 0 |  0   |  0   |   0    | 
--
--     load  rt, rs, offset     
--        # load data at memory location (rs + offset) into rt
--        # format:  | opcode = 1 |  rs  |  rt  | offset |
--
--     store rt, rs, offset
--        # store data rt into memory location (rs + offset)
--        # format:  | opcode = 3 |  rs  |  rt  | offset |
--
--     add   rd, rs, rt
--        # rd <- rs + rt
--        # format:  | opcode = 8 |  rs  |  rt  |   rd   |
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

-- count can be 7 downto 0, convenience for 16b addeer
entity single_cycle_core is
    port ( reset  : in  std_logic;
           clk    : in  std_logic;
           mode   : in  std_logic;
           IO_buffer   : in std_logic_vector(31 downto 0);
           ready    : out std_logic;
           count    : out std_logic_vector(15 downto 0);
           test_probe : out std_logic_vector(15 downto 0));
end single_cycle_core;

architecture structural of single_cycle_core is

-- address change from 4 bit to 8 bit space
component program_counter is
    port ( reset    : in  std_logic;
           clk      : in  std_logic;
           load     : in  std_logic;
           addr_in  : in  std_logic_vector(7 downto 0);
           addr_out : out std_logic_vector(7 downto 0) );
end component;

-- instructions are now 32 bit
component instruction_memory is
    port ( reset    : in  std_logic;
           clk      : in  std_logic;
           addr_in  : in  std_logic_vector(7 downto 0);
           insn_out : out std_logic_vector(31 downto 0) );
end component;


component mux_2to1_4b is
    port ( mux_select : in  std_logic;
           data_a     : in  std_logic_vector(3 downto 0);
           data_b     : in  std_logic_vector(3 downto 0);
           data_out   : out std_logic_vector(3 downto 0) );
end component;

component mux_2to1_8b is
    port ( mux_select : in  std_logic;
           data_a     : in  std_logic_vector(7 downto 0);
           data_b     : in  std_logic_vector(7 downto 0);
           data_out   : out std_logic_vector(7 downto 0) );
end component;

component mux_2to1_16b is
    port ( mux_select : in  std_logic;
           data_a     : in  std_logic_vector(15 downto 0);
           data_b     : in  std_logic_vector(15 downto 0);
           data_out   : out std_logic_vector(15 downto 0) );
end component;


component adder_4b is
    port ( src_a     : in  std_logic_vector(3 downto 0);
           src_b     : in  std_logic_vector(3 downto 0);
           sum       : out std_logic_vector(3 downto 0);
           carry_out : out std_logic );
end component;

component adder_8b is
    port ( src_a     : in  std_logic_vector(7 downto 0);
           src_b     : in  std_logic_vector(7 downto 0);
           sum       : out std_logic_vector(7 downto 0);
           carry_out : out std_logic );
end component;

component adder_16b is
    port ( src_a     : in  std_logic_vector(15 downto 0);
           src_b     : in  std_logic_vector(15 downto 0);
           sum       : out std_logic_vector(15 downto 0);
           carry_out : out std_logic );
end component;

component data_memory is
    port ( reset        : in  std_logic;
           clk          : in  std_logic;
           read_character: in std_logic;
           read_length  : in std_logic;
           read_occurence : in std_logic;
           write_character : in  std_logic;
           write_length : in std_logic;
           write_occurence : in std_logic;
           write_data   : in  std_logic_vector(15 downto 0);
           addr_in      : in  std_logic_vector(3 downto 0);
           pattern_number : in std_logic_vector(3 downto 0);
           data_out     : out std_logic_vector(15 downto 0);
           total_count  : out std_logic_vector(15 downto 0));
end component;

--modified
component unsign_sign_extend_8to16 is
    port ( data_in  : in  std_logic_vector(7 downto 0);
           unsign_sel : in std_logic;
           data_out : out std_logic_vector(15 downto 0) );
end component;

component control_unit is
    port (  opcode     : in  std_logic_vector(7 downto 0);
            control_bus : out std_logic_vector(11 downto 0); -- used as signal for forwarding detection
            control_bus_new : out std_logic_vector(8 downto 0);
            ready    : out std_logic);
end component;

component register_file is
    port ( reset           : in  std_logic;
       clk             : in  std_logic;
       register_a : in  std_logic_vector(7 downto 0);
       register_b : in  std_logic_vector(7 downto 0);
       indirect_read_a : in std_logic;
       write_enable    : in  std_logic;
       write_enable2   : in  std_logic;
       write_register  : in  std_logic_vector(7 downto 0);
       write_register2 : in  std_logic_vector(7 downto 0);
       write_data      : in  std_logic_vector(15 downto 0);
       write_data2     : in  std_logic_vector(15 downto 0);
       read_data_a     : out std_logic_vector(15 downto 0);
       read_data_b     : out std_logic_vector(15 downto 0));
end component;

-- new defined components
component next_inst_logic is
    Port ( curr : in STD_LOGIC_VECTOR (3 DOWNTO 0);
           offset : in STD_LOGIC_VECTOR (3 DOWNTO 0);
           inst_size : in STD_LOGIC_VECTOR (3 DOWNTO 0); 
           enable_branch : in STD_LOGIC;
           branch : in STD_LOGIC;
           next_inst : out STD_LOGIC_VECTOR (3 DOWNTO 0));
end component;

component comparator_16b is
    Port ( data_a : in STD_LOGIC_VECTOR (15 downto 0);
           data_b : in STD_LOGIC_VECTOR (15 downto 0);
           equal : out STD_LOGIC;
           greater : out STD_LOGIC;
           less : out STD_LOGIC);
end component;

component alu is
    Port ( data_a : in STD_LOGIC_VECTOR (15 downto 0);
           data_b : in STD_LOGIC_VECTOR (15 downto 0);
           result_sel : in STD_LOGIC;
           array_op : in STD_LOGIC;
           result : out STD_LOGIC_VECTOR (15 downto 0);
           carry_out : out STD_LOGIC;
           equal : out STD_LOGIC;
           greater : out STD_LOGIC;
           less : out STD_LOGIC);
end component;

component register_nb is
    Generic (N : integer range 0 to 32 := 16);
    Port ( reset : in std_logic;
           clk : in std_logic;
           load : in std_logic;
           input : in std_logic_vector(N-1 downto 0);
           output : out std_logic_vector(N-1 downto 0));
end component;

component fowarding_unit is
    port (c_mem_data1 : in std_logic; --data1 = 1 when read from memory, data = 0 when read from alu (wreg 1)
          c_wb_data1 : in std_logic;
          c_mem_wreg1 : in std_logic;
          c_wb_wreg1 : in std_logic;
          c_mem_wreg2 : in std_logic;
          c_wb_wreg2 : in std_logic;
          c_mem_out_of_bound : in std_logic;
          c_wb_out_of_bound : in std_logic;
          mem_wreg1 : in std_logic_vector(7 downto 0);
          wb_wreg1 : in std_logic_vector(7 downto 0);
          mem_wreg2 : in std_logic_vector(7 downto 0);
          wb_wreg2 : in std_logic_vector(7 downto 0);
          EX_MEM_result : in std_logic_vector(15 downto 0);
          wb_wdata1 : in std_logic_vector(15 downto 0);
          wb_wdata2 : in std_logic_vector(15 downto 0);
          ex_rreg1 : in std_logic_vector(7 downto 0);
          ex_rreg2 : in std_logic_vector(7 downto 0);
          ex_old_a : in std_logic_vector(15 downto 0);
          ex_old_b : in  std_logic_vector(15 downto 0);
          ex_new_a : out std_logic_vector(15 downto 0);
          ex_new_b : out std_logic_vector(15 downto 0));
end component;

component hazard_detection_unit is
    port (c_ex_data1 : in std_logic;
          c_ex_wreg1 : in std_logic;
          c_ex_wreg2 : in std_logic;
          c_mem_data1 : in std_logic; --used by branch stall
          id_rreg1 : in std_logic_vector(7 downto 0);
          id_rreg2 : in std_logic_vector(7 downto 0);
          ex_wreg1 : in std_logic_vector(7 downto 0); -- no need to check for reg2
          ex_wreg2 : in std_logic_vector(7 downto 0);
          mem_wreg1 : in std_logic_vector(7 downto 0);
          opcode   : in std_logic_vector(7 downto 0);
          hc_if_pc_write : out std_logic;
          hc_if_id_no_branch_write : out std_logic;
          hc_if_id_inst_write : out std_logic;
          hc_id_bubble_sel : out std_logic); 
end component;

component instruction_address_translation is
    port ( current_pc : in STD_LOGIC_VECTOR (7 downto 0);
           mode : in STD_LOGIC;
           real_address : out STD_LOGIC_VECTOR (7 downto 0));
end component;

-- initialize
-- datapath
signal sig_one_8b : std_logic_vector(7 downto 0);
---
---
-- IF stage 

-- signals
signal hc_if_pc_write : std_logic;
signal hc_if_id_inst_write : std_logic;
signal hc_if_id_no_branch_write : std_logic;
signal c_if_branch_sel : std_logic;
signal c_if_flush_insn : std_logic;
-- datapath
signal if_curr_pc           : std_logic_vector(7 downto 0);
signal if_pc_plus_1         : std_logic_vector(7 downto 0);
signal if_next_pc           : std_logic_vector(7 downto 0);
signal if_pc_plus_1_carry   : std_logic; -- not used
signal if_insn              : std_logic_vector(31 downto 0);
signal if_real_insn_address : std_logic_vector(7 downto 0);
---
---

-- ID stage
-- signals
signal c_id_branch_enable : std_logic;
signal c_id_extension : std_logic;
signal c_id_wreg1_sel : std_logic;
signal c_id_indir_read_a : std_logic;
signal c_id_jump : std_logic;
signal hc_id_bubble_sel : std_logic;

-- datapath
signal id_pc_plus_1 : std_logic_vector(7 downto 0);
signal id_pc_plus_1_and_offset_carry : std_logic; -- not used
signal id_inst  : std_logic_vector(31 downto 0);
signal id_pc_plus_1_and_offset : std_logic_vector(7 downto 0);
signal id_extended_offset : std_logic_vector(15 downto 0);
signal id_wreg1: std_logic_vector(7 downto 0);
signal id_wreg2: std_logic_vector(7 downto 0);
signal id_not_eq : std_logic;
signal id_read_data_A : std_logic_vector(15 downto 0);
signal id_read_data_B : std_logic_vector(15 downto 0);

signal id_control_bus : std_logic_vector(11 downto 0);
signal id_control_bus2 : std_logic_vector(8 downto 0);

signal id_control_bus_new: std_logic_vector(8 downto 0);
signal id_control_bus2_new: std_logic_vector(6 downto 0);
-- EX stage
-- signals
signal c_ex_array_op : std_logic;
signal c_ex_data_b : std_logic;
signal c_ex_result : std_logic;

-- datapath
signal ex_read_data_a : std_logic_vector(15 downto 0);
signal ex_read_data_b : std_logic_vector(15 downto 0);
signal ex_extended_offset : std_logic_vector(15 downto 0);
signal ex_wreg1 : std_logic_vector(7 downto 0);
signal ex_wreg2 : std_logic_vector(7 downto 0);

signal ex_alu_src_A : std_logic_vector (15 downto 0);
signal ex_alu_src_B : std_logic_vector (15 downto 0);
signal ex_result : std_logic_vector (15 downto 0);
signal ex_alu_less : std_logic;
signal ex_alu_carry_out : std_logic; --not used
signal ex_alu_greater : std_logic; --not used
signal ex_alu_equal : std_logic; --not used

signal ex_control_bus : std_logic_vector(8 downto 0);
signal ex_control_bus2 : std_logic_vector(5 downto 0);
signal ex_control_bus_new: std_logic_vector(6 downto 0);

-- MEM stage
-- signals
signal c_mem_address : std_logic;
signal c_mem_write : std_logic; -- not used
signal c_mem_read_char : std_logic;
signal c_mem_read_len : std_logic;
signal c_mem_read_occur : std_logic;
signal c_mem_write_char : std_logic;
signal c_mem_write_len : std_logic;
signal c_mem_write_occur : std_logic;
signal c_mem_wreg_pn : std_logic;

-- datapath
signal mem_result : std_logic_vector(15 downto 0);
signal mem_index : std_logic_vector(15 downto 0);
signal mem_address : std_logic_vector(15 downto 0);
signal mem_memory_data : std_logic_vector(15 downto 0);
signal mem_write_data : std_logic_vector(15 downto 0);
signal mem_wreg1 : std_logic_vector(7 downto 0);
signal mem_wreg2 : std_logic_vector(7 downto 0);
signal mem_pattern_number : std_logic_vector(3 downto 0);

signal mem_control_bus : std_logic_vector(5 downto 0);
signal mem_control_bus2 : std_logic_vector(3 downto 0);

signal mem_control_bus_new: std_logic_vector(6 downto 0);
-- WB stage
-- signals
signal c_wb_wreg1 : std_logic;
signal c_wb_wreg2 : std_logic;
signal c_wb_out_of_bound : std_logic;
signal c_wb_data1 : std_logic;
-- datapath
signal wb_memory_data : std_logic_vector(15 downto 0);
signal wb_result : std_logic_vector(15 downto 0);
signal wb_data1_i : std_logic_vector(15 downto 0);

signal wb_wreg1: std_logic_vector(7 downto 0);
signal wb_wdata1 : std_logic_vector(15 downto 0);
signal wb_wreg2: std_logic_vector(7 downto 0);
signal wb_wdata2 : std_logic_vector(15 downto 0); 

signal wb_control_bus : std_logic_vector(3 downto 0);



--- forwarding signals and  data
signal ex_read_reg1 : std_logic_vector(7 downto 0);
signal ex_read_reg2 : std_logic_vector(7 downto 0);
signal ex_read_data_a_f: std_logic_vector(15 downto 0);
signal ex_read_data_b_f: std_logic_vector(15 downto 0);

signal id_read_data_a_f : std_logic_vector(15 downto 0);
signal id_read_data_b_f : std_logic_vector(15 downto 0);
begin
    -- initialize
    -- dummy output to prevent vivado optimizting
    test_probe <= wb_wdata1;
    sig_one_8b <= X"01";
   

    -- IF stage
    pc : program_counter
    port map ( reset    => reset,
               clk      => clk,
               load     => hc_if_pc_write,
               addr_in  => if_next_pc,
               addr_out => if_curr_pc ); 
   
    insn_addr_translation: instruction_address_translation 
    port map ( current_pc   => if_curr_pc,
               mode         => mode,
               real_address => if_real_insn_address);
               
    insn_mem : instruction_memory 
    port map ( reset    => reset,
               clk      => clk,
               addr_in  => if_real_insn_address,
               insn_out => if_insn ); 
               
    pc_plus_one_adder : adder_8b
    port map (src_a     =>  sig_one_8b,
              src_b     =>  if_curr_pc,
              sum       =>  if_pc_plus_1,
              carry_out =>  if_pc_plus_1_carry);
              
   pc_next_inst_mux : mux_2to1_8b
    port map(mux_select =>  c_if_branch_sel,
             data_a     =>  if_pc_plus_1,
             data_b     =>  id_pc_plus_1_and_offset,
             data_out   =>  if_next_pc);
             
    c_if_branch_sel <= (id_not_eq and c_id_branch_enable) or c_id_jump;     
      
    -- don't flush during stalling  
    c_if_flush_insn <= (c_if_branch_sel and (not hc_id_bubble_sel)) or reset;
            
    -- IF/ID pipeline registers
    IF_ID_no_branch_pc: register_nb
    generic map (8)
    port map (reset     =>  reset,
              clk       =>  clk,
              load      =>  hc_if_id_no_branch_write,
              input     =>  if_pc_plus_1,
              output    =>  id_pc_plus_1);
    
    IF_ID_instruction: register_nb
    generic map (32)
    port map (reset     =>  c_if_flush_insn,
              clk       =>  clk,
              load      =>  hc_if_id_inst_write,
              input     =>  if_insn,
              output    =>  id_inst);
    
    -- ID stage
    
    --------------- ID stage controls -------------     
    c_id_wreg1_sel <= id_control_bus(11);
    c_id_extension <= id_control_bus(10);
    c_id_branch_enable <= id_control_bus(9);
    
    c_id_jump <= id_control_bus_new(8);
    c_id_indir_read_a <= id_control_bus_new(7);
    
    -- c_id_indir_read_a
    -- c_id_jump
    mux_control_bubble : 
    id_control_bus2 <= id_control_bus(8 downto 0) when hc_id_bubble_sel = '0' else (others => '0');
    mux_control_bubble_new :
    id_control_bus2_new <= id_control_bus_new(6 downto 0) when hc_id_bubble_sel = '0' else (others => '0');
    
    ------------------------------------------------    
    
    hazard_detection: hazard_detection_unit
    port map (c_ex_data1                => ex_control_bus2(1),
              c_ex_wreg1                => ex_control_bus2(2),
              c_ex_wreg2                => ex_control_bus2(3),
              c_mem_data1               => mem_control_bus2(1),--used by branch stall
              id_rreg1                  => id_inst(23 downto 16),
              id_rreg2                  => id_inst(15 downto 8),
              ex_wreg1                  => ex_wreg1, -- no need to check for reg2
              ex_wreg2                  => ex_wreg2,
              mem_wreg1                 => mem_wreg1,
              opcode                    => id_inst(31 downto 24),
              hc_if_pc_write            => hc_if_pc_write,
              hc_if_id_no_branch_write  => hc_if_id_no_branch_write,
              hc_if_id_inst_write       => hc_if_id_inst_write,
              hc_id_bubble_sel          => hc_id_bubble_sel);
    
    pc_plus_1_and_offset_adder : adder_8b
    port map (src_a     =>  id_pc_plus_1,
              src_b     =>  id_inst(7 downto 0),
              sum       =>  id_pc_plus_1_and_offset,
              carry_out =>  id_pc_plus_1_and_offset_carry);

    
    unsign_sign_extend : unsign_sign_extend_8to16 
    port map ( data_in  => id_inst(7 downto 0),
               unsign_sel => c_id_extension,
               data_out => id_extended_offset);
               
    ctrl_unit : control_unit 
    port map ( opcode     => id_inst(31 downto 24),
               control_bus => id_control_bus,
               control_bus_new => id_control_bus_new,
               ready    => ready);
   
    mux_write_reg1 : mux_2to1_8b 
    port map ( mux_select => c_id_wreg1_sel,
               data_a     => id_inst(15 downto 8),
               data_b     => id_inst(7 downto 0),
               data_out   => id_wreg1);
               
    id_wreg2 <= id_inst(15 downto 8);

    reg_file : register_file 
    port map ( reset           => reset, 
               clk             => clk,
               register_a      => id_inst(23 downto 16),
               register_b      => id_inst(15 downto 8),
               indirect_read_a => c_id_indir_read_a,
               write_enable    => c_wb_wreg1,
               write_enable2   => c_wb_wreg2,
               write_register  => wb_wreg1,
               write_register2 => wb_wreg2,
               write_data      => wb_wdata1,
               write_data2     => wb_wdata2,
               read_data_a     => id_read_data_a,
               read_data_b     => id_read_data_b);               
       
    branch_comparator: 
    process(id_read_data_a_f, id_read_data_b_f)
    begin
        if id_read_data_a_f /= id_read_data_b_f then
            id_not_eq <= '1';
        else
            id_not_eq <= '0';
        end if;
    end process;
    -- ID/EX pipeline registers
    ID_EX_control_new_register: register_nb
    generic map(7)
    port map (reset     =>  reset,
              clk       =>  clk,
              load      =>  '1',
              input     =>  id_control_bus2_new,
              output    =>  ex_control_bus_new); 
    
    ID_EX_control_register: register_nb
    generic map (9)
    port map (reset     =>  reset,
              clk       =>  clk,
              load      =>  '1',
              input     =>  id_control_bus2,
              output    =>  ex_control_bus);
    
    ID_EX_register_1: register_nb
    generic map (16)
    port map (reset     =>  reset,
              clk       =>  clk,
              load      =>  '1',
              input     =>  id_read_data_a_f,
              output    =>  ex_read_data_a);
              
    ID_EX_register_2: register_nb
    generic map (16)
    port map (reset     =>  reset,
              clk       =>  clk,
              load      =>  '1',
              input     =>  id_read_data_b_f,
              output    =>  ex_read_data_b);
              
    ID_EX_register_imm: register_nb
    generic map (16)
    port map (reset     =>  reset,
              clk       =>  clk,
              load      =>  '1',
              input     =>  id_extended_offset,
              output    =>  ex_extended_offset);
    
    ID_EX_write_register_1: register_nb
    generic map (8)
    port map (reset     =>  reset,
              clk       =>  clk,
              load      =>  '1',
              input     =>  id_wreg1,
              output    =>  ex_wreg1);
              
    ID_EX_write_register_2: register_nb
    generic map (8)
    port map (reset     =>  reset,
              clk       =>  clk,
              load      =>  '1',
              input     =>  id_wreg2,
              output    =>  ex_wreg2);
    -- EX stage
    
    --------------- EX stage controls -------------     
    c_ex_array_op <= ex_control_bus(8);
    c_ex_data_b <= ex_control_bus(7);
    c_ex_result <= ex_control_bus(6);

    ex_control_bus2(5) <= ex_control_bus(5);
    ex_control_bus2(4) <= ex_control_bus(4);
    ex_control_bus2(3) <= (not ex_alu_less) and c_ex_array_op; -- modify c_wb_wreg2
    ex_control_bus2(2) <= ex_control_bus(2);
    ex_control_bus2(1) <= ex_control_bus(1);
    ex_control_bus2(0) <= ex_alu_less and c_ex_array_op; -- modify c_wb_out_of_bound 
    ------------------------------------------------    
    mux_alu_src_b : mux_2to1_16b 
    port map ( mux_select => c_ex_data_b,
               data_a     => ex_read_data_b_f,
               data_b     => ex_extended_offset,
               data_out   => ex_alu_src_b);
    alu_src_a : 
    ex_alu_src_a <= ex_read_data_a_f;

    alu_unit : alu 
    port map ( data_a       => ex_alu_src_a,
               data_b       => ex_alu_src_b,
               result_sel   => c_ex_result,
               result       => ex_result,
               array_op     => c_ex_array_op,
               carry_out    => ex_alu_carry_out,
               equal        => ex_alu_equal,
               greater      => ex_alu_greater,
               less         => ex_alu_less);

    -- EX/MEM pipeline registers
    EX_MEM_control_new_register: register_nb
    generic map (7)
    port map (reset     =>  reset,
              clk       =>  clk,
              load      =>  '1',
              input     =>  ex_control_bus_new,
              output    =>  mem_control_bus_new); 
    
    EX_MEM_control_register: register_nb
    generic map (6)
    port map (reset     =>  reset,
              clk       =>  clk,
              load      =>  '1',
              input     =>  ex_control_bus2,
              output    =>  mem_control_bus);
     
    EX_MEM_result: register_nb
    generic map (16)
    port map (reset     =>  reset,
              clk       =>  clk,
              load      =>  '1',
              input     =>  ex_result,
              output    =>  mem_result);
    
    EX_MEM_index: register_nb
    generic map (16)
    port map (reset     =>  reset,
              clk       =>  clk,
              load      =>  '1',
              input     =>  ex_alu_src_b,
              output    =>  mem_index);
              
    EX_MEM_write_data: register_nb
    generic map (16)
    port map (reset     =>  reset,
              clk       =>  clk,
              load      =>  '1',
              input     =>  ex_read_data_b_f,
              output    =>  mem_write_data);
              
    EX_MEM_write_register_1: register_nb
    generic map (8)
    port map (reset     =>  reset,
              clk       =>  clk,
              load      =>  '1',
              input     =>  ex_wreg1,
              output    =>  mem_wreg1);
              
    EX_MEM_write_register_2: register_nb
    generic map (8)
    port map (reset     =>  reset,
              clk       =>  clk,
              load      =>  '1',
              input     =>  ex_wreg2,
              output    =>  mem_wreg2);
    -- MEM stage
    
    --------------- MEM stage controls -------------     
    c_mem_write <= mem_control_bus(5);
    c_mem_address <= mem_control_bus(4);
    
    c_mem_read_char <= mem_control_bus_new(0);
    c_mem_read_len <= mem_control_bus_new(1);
    c_mem_read_occur <= mem_control_bus_new(2);
    c_mem_write_char <= mem_control_bus_new(3);
    c_mem_write_len <= mem_control_bus_new(4);
    c_mem_write_occur <= mem_control_bus_new(5);
    c_mem_wreg_pn <= mem_control_bus_new(6);
    
    mem_control_bus2 <= mem_control_bus(3 downto 0);
    ------------------------------------------------  
    mux_mem_address : mux_2to1_16b
    port map (mux_select => c_mem_address,
              data_a => mem_result,
              data_b => mem_index,
              data_out => mem_address);
              
    pattern_number: register_nb
    generic map (4)
    port map (reset     =>  reset,
              clk       =>  clk,
              load      =>  c_mem_wreg_pn,
              input     =>  mem_result(3 downto 0),
              output    =>  mem_pattern_number);           
    
    data_mem : data_memory 
    port map ( reset        => reset,
               clk          => clk,
               read_character => c_mem_read_char,
               read_length  => c_mem_read_len,
               read_occurence => c_mem_read_occur,
               write_character => c_mem_write_char,
               write_length => c_mem_write_len,
               write_occurence => c_mem_write_occur,
               --write_enable => c_mem_write,
               write_data   => mem_write_data,
               -- 3 downto 0 is correct since 2^4 = 16
               addr_in      => mem_address(3 downto 0),
               pattern_number => mem_pattern_number,
               data_out     => mem_memory_data,
               total_count  => count);
    
    -- MEM/WB pipeline register
    MEM_WB_control_register: register_nb
    generic map (4)
    port map (reset     =>  reset,
              clk       =>  clk,
              load      =>  '1',
              input     =>  mem_control_bus2,
              output    =>  wb_control_bus);

    MEM_WB_memory_data_register: register_nb
    generic map (16)
    port map (reset     =>  reset,
              clk       =>  clk,
              load      =>  '1',
              input     =>  mem_memory_data,
              output    =>  wb_memory_data);

    MEM_WB_alu_result: register_nb
    generic map (16)
    port map (reset     =>  reset,
              clk       =>  clk,
              load      =>  '1',
              input     =>  mem_result,
              output    =>  wb_result);
    
    MEM_WB_write_reg_1: register_nb
    generic map (8)
    port map (reset     =>  reset,
              clk       =>  clk,
              load      =>  '1',
              input     =>  mem_wreg1,
              output    =>  wb_wreg1);  
              
    MEM_WB_write_reg_2: register_nb
    generic map (8)
    port map (reset     =>  reset,
              clk       =>  clk,
              load      =>  '1',
              input     =>  mem_wreg2,
              output    =>  wb_wreg2); 
    -- WB stage
    
    --------------- WB stage controls -------------     
    c_wb_wreg2 <= wb_control_bus(3);
    c_wb_wreg1 <= wb_control_bus(2);
    c_wb_data1 <= wb_control_bus(1);
    c_wb_out_of_bound <= wb_control_bus(0);   
    ------------------------------------------------ 
    
    mux_wdata1_src : mux_2to1_16b 
    port map ( mux_select => c_wb_data1,
               data_a     => wb_result,
               data_b     => wb_memory_data,
               data_out   => wb_data1_i );

    mux_out_of_bound_check : mux_2to1_16b
    port map ( mux_select => c_wb_out_of_bound,
               data_a     => wb_data1_i,
               data_b     => (others => '1'),
               data_out   => wb_wdata1);
   
    wb_wdata2 <= wb_result;
    
    ----------------------------------------------------
    ---- components for forwarding-----------
    
    read_reg1: register_nb
    generic map (8)
    port map (reset     => reset,
              clk       => clk,   
              load      => '1',         
              input     => id_inst(23 downto 16),
              output    => ex_read_reg1);
    
    read_reg2: register_nb
    generic map (8)
    port map (reset     => reset,
              clk       => clk,    
              load      => '1',         
              input     => id_inst(15 downto 8),
              output    => ex_read_reg2);
    
    EX_forwarding_unit: fowarding_unit
    port map (c_mem_data1           => mem_control_bus2(1),
              c_wb_data1            => c_wb_data1,
              c_mem_wreg1           => mem_control_bus2(2),
              c_wb_wreg1            => c_wb_wreg1,
              c_mem_wreg2           => mem_control_bus2(3),
              c_wb_wreg2            => c_wb_wreg2,
              c_mem_out_of_bound    => mem_control_bus2(0),
              c_wb_out_of_bound     => c_wb_out_of_bound,
              mem_wreg1             => mem_wreg1,
              wb_wreg1              => wb_wreg1,
              mem_wreg2             => mem_wreg2,
              wb_wreg2              => wb_wreg2,
              EX_MEM_result         => mem_result,
              wb_wdata1             => wb_wdata1,
              wb_wdata2             => wb_wdata2,
              ex_rreg1              => ex_read_reg1,
              ex_rreg2              => ex_read_reg2,
              ex_old_a              => ex_read_data_a,
              ex_old_b              => ex_read_data_b,
              ex_new_a              => ex_read_data_a_f,
              ex_new_b              => ex_read_data_b_f);    
    
    ID_forwarding_unit: fowarding_unit
    port map (c_mem_data1           => mem_control_bus2(1),
              c_wb_data1            => c_wb_data1,
              c_mem_wreg1           => mem_control_bus2(2),
              c_wb_wreg1            => c_wb_wreg1,
              c_mem_wreg2           => mem_control_bus2(3),
              c_wb_wreg2            => c_wb_wreg2,
              c_mem_out_of_bound    => mem_control_bus2(0),
              c_wb_out_of_bound     => c_wb_out_of_bound,
              mem_wreg1             => mem_wreg1,
              wb_wreg1              => wb_wreg1,
              mem_wreg2             => mem_wreg2,
              wb_wreg2              => wb_wreg2,
              EX_MEM_result         => mem_result,
              wb_wdata1             => wb_wdata1,
              wb_wdata2             => wb_wdata2,
              ex_rreg1              => id_inst(23 downto 16),
              ex_rreg2              => id_inst(15 downto 8),
              ex_old_a              => id_read_data_a,
              ex_old_b              => id_read_data_b,
              ex_new_a              => id_read_data_a_f,
              ex_new_b              => id_read_data_b_f);  
    ---------------------------------------
    
end structural;
