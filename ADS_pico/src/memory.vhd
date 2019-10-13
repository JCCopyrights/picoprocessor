-----------------------------------------------------------------------------
-- %Z% Fichero:  memory.vhd  
-----------------------------------------------------------------------------
-- Autores: Yago Torroja                               yago@upmdie.upm.es  --
-----------------------------------------------------------------------------
-- Copyright Universidad Politecnica de Madrid                             --
-----------------------------------------------------------------------------
-- E.T.S. de Ingenieros Industriales                                       --
-- Division de Ingenieria Electronica (UPM-DIE)                            --
-- E.T.S.I Industriales                                                    --
-- Jose Gutierrez Abascal, 2                                               --
-- MADRID 28006                                                            --
-- Tel : +34-1-3363191/92/93/94   Fax : +34-1-5645966                      --
-----------------------------------------------------------------------------
-- PROYECTO: picoProcessor                                                 --
--                                                                         --
-----------------------------------------------------------------------------
-- Bloque     | MEMORY                                                     --
--            |                                                            --
-- Descripcion| Modelo de la memoria                                       --
--            |                                                            --
-- Parametros | gAddressWidth, gDataWidth,                                 --
--            |                                                            --
-- Entradas   | Address, ChipEnableN, WriteEnableN, OutputEnableN, DataIn  --
--            |                                                            --
-- Salidas    | DataOut                                                    --
-----------------------------------------------------------------------------
-- Dependencias:                                                           --
--               std_logic_1164, std_logic_unsigned                        --
-----------------------------------------------------------------------------
-- Version: 9.0                                        Fecha: 28/02/05     --
-----------------------------------------------------------------------------
-- Ver   |  fecha   | Autor | Descripcion de la modificacion               --
---------|----------|-------|-------------------------------------------------
-- 10.00 | 28/01/14 |  YT   | Adaptacion a XILINX                          -- 
--       |          |       |
-----------------------------------------------------------------------------

use std.textio.all;

library IEEE;
  use IEEE.std_logic_1164.all;
  use IEEE.std_logic_arith.all;
  use IEEE.std_logic_unsigned.all;
  use IEEE.std_logic_textio.all;

library WORK;
  use WORK.pico_types.all;

entity Memory is
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

  ------------------------------------------------------------------------- 
  -- Types
  -------------------------------------------------------------------------

  subtype RAM_Data_Word  is std_logic_vector (gDataWidth - 1  downto 0);
  
  type RAM_Data_Table is array (0 to 2**gAddressWidth - 1) of RAM_Data_Word;

end Memory;

use std.textio.all;

library IEEE;
  use IEEE.std_logic_1164.all;
  use IEEE.std_logic_arith.all;
  use IEEE.std_logic_unsigned.all;
  use IEEE.std_logic_textio.all;

architecture XILINX_SYNTH_BLOCK_RAM of Memory is

  -------------------------------------------------------------------------
  -- Initialization function
  -------------------------------------------------------------------------     
    impure function Init_Ram (ram_file_name : in string) return RAM_Data_Table is                                                   
      FILE ram_file     : text is in ram_file_name;                                                     
      variable L        : LINE;
      variable ram      : RAM_Data_Table;                                       
      variable DV       : std_logic_vector(gDataWidth - 1 downto 0);
      variable Pos      : integer;
    begin
      for I in RAM_Data_Table'range loop
        ram(I) := (others => '0');
      end loop;
      Pos := 0;
      while not EndFile(ram_file) loop
        ReadLine(ram_file, L);
        Read(L, DV);
        ram(Pos) := DV;
        Pos := Pos + 1;
      end loop;      
      return ram;      
    end function; 

  -------------------------------------------------------------------------
  -- Signals
  -------------------------------------------------------------------------  

    -- RAM
    signal RAM : RAM_Data_Table := Init_Ram("PROGRAM.BIN");

    signal CLK    : std_logic;
    signal EN     : std_logic;
    signal WE     : std_logic;
    signal SSR    : std_logic;
    signal DO     : std_logic_vector (gDataWidth-1 downto 0);
    signal DI     : std_logic_vector (gDataWidth-1 downto 0);                                               
    
begin
  
  ------------------------------------------------------------------------
  -- Concurrent statements
  ------------------------------------------------------------------------
  DataOut <= DO;
  DI      <= DataIn;
  CLK     <= Clock;
  EN      <= not ChipEnableN;
  WE      <= not WriteEnableN;
  
  -- YT: Modificado para eliminar registro de salida para DO 
  -- (se ha comentado esta linea en el proceso más abajo)
  DO      <= RAM(conv_integer(Address));

  process (CLK)
  begin
    if CLK'event and CLK = '1' then
      if EN = '1' then
        if WE = '1' then
          RAM(conv_integer(Address)) <= DI;
        end if;
        -- DO <= RAM(conv_integer(Addr));
      end if;
    end if;
  end process;

end XILINX_SYNTH_BLOCK_RAM;

configuration CFG_MEMORY_XILINX_SYNTH_BLOCK_RAM of Memory is
   for XILINX_SYNTH_BLOCK_RAM
   end for;
end CFG_MEMORY_XILINX_SYNTH_BLOCK_RAM;



