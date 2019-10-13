library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_signed.all;
use IEEE.std_logic_arith.all;

library WORK;
use WORK.pico_types.all;

entity system is
    port (
      Clk     : in std_logic;
      Reset   : in std_logic;  
      Address : out std_logic_vector(AddrWidth - 1 downto 0);
      Data    : inout std_logic_vector(DataWidth - 1 downto 0);
      RW      : out std_logic;
      Me      : out std_logic
    );
end system;

architecture structural of system is

  ------------------------------------------------------------------------
  -- Componentes
  ------------------------------------------------------------------------
  component picoCPU
    port (
      Clk    : in std_logic;
      Reset  : in std_logic;  
      Addr   : out std_logic_vector(AddrWidth - 1 downto 0);
      Data   : inout std_logic_vector(DataWidth - 1 downto 0);
      R_W    : out std_logic;
      MemAcc : out std_logic
    );
  end component;

  component Memory is
    generic (
      gAddressWidth  : integer := AddrWidth;
      gDataWidth     : integer := DataWidth );
    port (
      Clock         : in    std_logic;
      Address       : in    std_logic_vector (gAddressWidth - 1 downto 0);
      ChipEnableN   : in    std_logic;
      WriteEnableN  : in    std_logic;
      DataIn        : in    std_logic_vector (gDataWidth - 1 downto 0);
      DataOut       : out   std_logic_vector (gDataWidth - 1 downto 0)
    );
  end component;

  for all:memory use entity work.memory(XILINX_SYNTH_BLOCK_RAM);

  ------------------------------------------------------------------------
  -- Señales
  ------------------------------------------------------------------------

  signal  Addr     : std_logic_vector(AddrWidth - 1 downto 0);
  signal  DataMIn  : std_logic_vector(DataWidth - 1 downto 0);
  signal  DataMOut : std_logic_vector(DataWidth - 1 downto 0);

  signal  R_W      : std_logic;
  signal  MEn      : std_logic;
  signal  MEnN     : std_logic;

  

  signal Simulating : boolean := true;

  ------------------------------------------------------------------------
  -- Constantes, typos y otras
  ------------------------------------------------------------------------
  constant Period  : time := 50 ns;
  constant LowVal  : std_logic := '0';
  constant HighVal : std_logic := '1';

begin

  Address <= Addr;
  RW      <= R_W;
  Me      <= Men;

  CPU: picoCPU
    port map (
      Clk    => Clk,
      Reset  => Reset,
      Addr   => Addr,
      Data   => Data,
      R_W    => R_W,
      MemAcc => MEn
    );

  DataMIn  <= Data;
  Data     <= DataMOut when MEn = '1' and R_W = '1' else (others => 'Z');
  MEnN     <= not MEn;
  Mem: Memory
    port map (
      Clock         => Clk,
      Address       => Addr,
      ChipEnableN   => MEnN,
      WriteEnableN  => R_W,
      DataIn        => DataMIn,
      DataOut       => DataMout
    );

end structural;
