---------------------------------------------------------------------------
-- data_memory.vhd - Implementation of A Single-Port, 16 x 16-bit Data
--                   Memory.
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

entity data_memory is
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
end data_memory;

architecture behavioral of data_memory is
component character_data is
    port ( reset        : in  std_logic;
           clk          : in  std_logic;
           write_enable : in  std_logic;
           write_data   : in  std_logic_vector(15 downto 0);
           addr_in      : in  std_logic_vector(7 downto 0);
           data_out     : out std_logic_vector(15 downto 0) );
end component;

component length_data is
    port ( reset        : in  std_logic;
           clk          : in  std_logic;
           write_enable : in  std_logic;
           write_data   : in  std_logic_vector(15 downto 0);
           addr_in      : in  std_logic_vector(3 downto 0);
           data_out     : out std_logic_vector(15 downto 0) );
end component;

component occurrence_data is
    port ( reset        : in  std_logic;
           clk          : in  std_logic;
           write_enable : in  std_logic;
           write_data   : in  std_logic_vector(15 downto 0);
           addr_in      : in  std_logic_vector(3 downto 0);
           data_out     : out std_logic_vector(15 downto 0);
           p0_occurence: out std_logic_vector(15 downto 0);
           p1_occurence: out std_logic_vector(15 downto 0);
           p2_occurence: out std_logic_vector(15 downto 0);
           p3_occurence: out std_logic_vector(15 downto 0);
           p4_occurence: out std_logic_vector(15 downto 0);
           p5_occurence: out std_logic_vector(15 downto 0);
           p6_occurence: out std_logic_vector(15 downto 0);
           p7_occurence: out std_logic_vector(15 downto 0));
end component;

-- length is max 16, so it should be 4 downto 0, but since we got 16 bit adder, we use 16 bit
component translation_unit is
    Port ( pattern_base_index : in STD_LOGIC_VECTOR (3 downto 0);
           offset   : in STD_LOGIC_VECTOR(3 DOWNTO 0);
           p0_length : in STD_LOGIC_VECTOR (15 downto 0);
           p1_length : in STD_LOGIC_VECTOR (15 downto 0);
           p2_length : in STD_LOGIC_VECTOR (15 downto 0);
           p3_length : in STD_LOGIC_VECTOR (15 downto 0);
           p4_length : in STD_LOGIC_VECTOR (15 downto 0);
           p5_length : in STD_LOGIC_VECTOR (15 downto 0);
           p6_length : in STD_LOGIC_VECTOR (15 downto 0);
           p7_length : in STD_LOGIC_VECTOR (15 downto 0);
           character_address : out STD_LOGIC_VECTOR (15 downto 0));
end component;

component occurence_adder is
    Port ( p0_occurence : in STD_LOGIC_VECTOR (15 downto 0);
           p1_occurence : in STD_LOGIC_VECTOR (15 downto 0);
           p2_occurence : in STD_LOGIC_VECTOR (15 downto 0);
           p3_occurence : in STD_LOGIC_VECTOR (15 downto 0);
           p4_occurence : in STD_LOGIC_VECTOR (15 downto 0);
           p5_occurence : in STD_LOGIC_VECTOR (15 downto 0);
           p6_occurence : in STD_LOGIC_VECTOR (15 downto 0);
           p7_occurence : in STD_LOGIC_VECTOR (15 downto 0);
           count : out STD_LOGIC_VECTOR (15 downto 0));
end component;


-- signals

-- datapaths
signal p0_length: std_logic_vector(15 downto 0);
signal p1_length: std_logic_vector(15 downto 0);
signal p2_length: std_logic_vector(15 downto 0);
signal p3_length: std_logic_vector(15 downto 0);
signal p4_length: std_logic_vector(15 downto 0);
signal p5_length: std_logic_vector(15 downto 0);
signal p6_length: std_logic_vector(15 downto 0);
signal p7_length: std_logic_vector(15 downto 0);

signal p0_occurence: std_logic_vector(15 downto 0);
signal p1_occurence: std_logic_vector(15 downto 0);
signal p2_occurence: std_logic_vector(15 downto 0);
signal p3_occurence: std_logic_vector(15 downto 0);
signal p4_occurence: std_logic_vector(15 downto 0);
signal p5_occurence: std_logic_vector(15 downto 0);
signal p6_occurence: std_logic_vector(15 downto 0);
signal p7_occurence: std_logic_vector(15 downto 0);

signal character_address_16b : std_logic_vector(15 downto 0);
signal character_address_8b : std_logic_vector(7 downto 0);
signal length_address : std_logic_vector(3 downto 0);
signal occurence_address : std_logic_vector(3 downto 0);

signal char_d: std_logic_vector(15 downto 0);
signal length_d  : std_logic_vector(15 downto 0);
signal occurence_d : std_logic_vector(15 downto 0);

signal read_mem : std_logic_vector(2 downto 0);
begin
    
    
    character_address_8b <= character_address_16b(7 downto 0);
    char_mem: character_data
    port map ( reset        => reset,
               clk          => clk,
               write_enable => write_character,
               write_data   => write_data,
               addr_in      => character_address_8b,
               data_out     => char_d);
    
    length_address <= addr_in;
    length_mem: length_data
    port map ( reset        =>  reset,
               clk          => clk,
               write_enable => write_length,
               write_data   => write_data,
               addr_in      => length_address,
               data_out     => length_d);
    
    occurence_address <= addr_in;
    occur_mem: occurrence_data
    port map ( reset    => reset,
               clk          => clk,
               write_enable => write_occurence,
               write_data   => write_data,
               addr_in      => occurence_address,
               data_out     => occurence_d,
               p0_occurence => p0_occurence,
               p1_occurence => p1_occurence,
               p2_occurence => p2_occurence,
               p3_occurence => p3_occurence,
               p4_occurence => p4_occurence,
               p5_occurence => p5_occurence,
               p6_occurence => p6_occurence,
               p7_occurence => p7_occurence);
               
    read_mem <= read_character & read_length & read_occurence;
    mux_16b: 
    with read_mem select
        data_out <= char_d when "100",
                    length_d when "010",
                    occurence_d when "001",
                    (others => '0') when others;
        
    
    t_unit: translation_unit
    port map ( pattern_base_index => pattern_number,
           offset => addr_in,
           p0_length => p0_length,
           p1_length => p1_length,
           p2_length => p2_length,
           p3_length => p3_length,
           p4_length => p4_length,
           p5_length => p5_length,
           p6_length => p6_length,
           p7_length => p7_length,
           character_address => character_address_16b);
           
    c_unit: occurence_adder
    port map ( p0_occurence => p0_occurence,
               p1_occurence => p1_occurence,
               p2_occurence => p2_occurence,
               p3_occurence => p3_occurence,
               p4_occurence => p4_occurence,
               p5_occurence => p5_occurence,
               p6_occurence => p6_occurence,
               p7_occurence => p7_occurence,
               count        => total_count);
end behavioral;
