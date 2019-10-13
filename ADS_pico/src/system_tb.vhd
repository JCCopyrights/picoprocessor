library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_signed.all;
use IEEE.std_logic_arith.all;

library WORK;
use WORK.pico_types.all;

entity system_tb is
end system_tb;


architecture structural of system_tb is

  ------------------------------------------------------------------------
  -- Componentes
  ------------------------------------------------------------------------
  
  component system is
    port (
      Clk     : in std_logic;
      Reset   : in std_logic;  
      Address : out std_logic_vector(AddrWidth - 1 downto 0);
      Data    : inout std_logic_vector(DataWidth - 1 downto 0);
      RW      : out std_logic;
      Me      : out std_logic
    );
  end component;
  
  ------------------------------------------------------------------------
  -- Señales
  ------------------------------------------------------------------------
  signal  Clk     : std_logic;
  signal  Reset   : std_logic;  
  signal  Address : std_logic_vector(AddrWidth - 1 downto 0);
  signal  Data    : std_logic_vector(DataWidth - 1 downto 0);
  signal  RW      : std_logic;
  signal  Me      : std_logic;

  ------------------------------------------------------------------------
  -- Constantes, typos y otras
  ------------------------------------------------------------------------
  constant CLK_Period  : time := 100 ns;

begin
  
  uut: system 
    port map(
		Clk      =>  Clk,   
	    Reset    =>  Reset,  
	    Address  =>  Address,
	    Data     =>  Data,  
	    RW       =>  RW,     
	    Me       =>  Me     
	);

  clk_process:process
  begin
    Clk<='0';
	wait for CLK_Period/2;
	Clk<='1';
	wait for CLK_Period/2;
  end process;
  
  stim_proc: process
  begin
	Reset<='0';
	wait for 100 ns;
	Reset<='1';
	wait;
  end process;




end structural;
