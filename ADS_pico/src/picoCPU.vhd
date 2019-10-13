library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_signed.all;
use IEEE.std_logic_arith.all;

library WORK;
use WORK.pico_types.all;

entity picoCPU is
  port (
    Clk    : in std_logic;
    Reset  : in std_logic;
    Addr   : out std_logic_vector(AddrWidth - 1 downto 0);
    Data   : inout std_logic_vector(DataWidth - 1 downto 0);
    R_W    : out std_logic;
    MemAcc : out std_logic
  );
end picoCPU;

architecture SemiFunc of picoCPU is

  -- Constantes, tipos y otras definiciones
  constant AluDelay   : time := 4 ns;
  constant DecodDelay : time := 2 ns;
  type AluOpType is (A_NOP, A_ADD, A_SUB, A_AND, A_OR, A_NOT);
  type StateType is (S_FETCH, S_DECOD, S_GETMSB, S_GETLSB,
                     S_EXECI, S_RDWR, S_PCINC);
  type InstrType is (
    I_NOP,  -- No operacion
    I_LDAI, -- Carga de A en modo inmediato (A <- M(PC + 1))
    I_LDAM, -- Carga de A en modo directo   (A <- M(M(PC+1):M(PC+2))
    I_STA,  -- Guarda A en modo directo     (M(M(PC+1):M(PC+2) <- A)
    I_MOV,  -- Copia A en B                 (B <- A)
    I_ADD,  -- Suma   (A <- A + B)
    I_SUB,  -- Resta  (A <- A - B)
    I_AND,  -- And lógico bit a bit (A <- A and B)
    I_OR,   -- Or lógico bit a bit  (A <- A or B)
    I_NOT,  -- Negación lógica      (A <- not A)
    I_BEQ,  -- Salto relativo si cero (Z = 1) entonces PC <- PC + M(PC+1)
    I_BNE,  -- Idem si no cero    (Z = 0)
    I_BCS,  -- Idem si acarreo    (C = 1)
    I_BCC,  -- Idem si no acarreo (C = 0)
    I_BMI,  -- Idem si negativo   (N = 1)
    I_BPL,  -- Idem si positivo   (N = 0)
    I_JMP,  -- Salto incondicional  (PC <- M(PC+1):M(PC+2)) 
    I_LDXI, -- Carga de X en modo inmediato (XL <- M(PC+1), XH <- M(PC+2))
    I_INX,  -- Incremento de X (XL <- XL+1)
    I_DEX,  -- Decremento de X (XL <- XL-1)
    I_LDAX, -- Carga de A en modo indexado (A <- M(IX))
    I_STAX, -- Guarda A en modo indexado (M(IX) <- A)
    I_NOI   -- Instrucción no válida (para el resto de codificaciones)
  );


  -- Señales que serán registros
  signal AReg   : std_logic_vector(DataWidth - 1 downto 0);
  signal BReg   : std_logic_vector(DataWidth - 1 downto 0);
  signal StReg  : std_logic_vector(DataWidth - 1 downto 0);
  signal InReg  : std_logic_vector(DataWidth - 1 downto 0);

  signal PCReg  : std_logic_vector(AddrWidth - 1 downto 0);
  signal AdReg  : std_logic_vector(AddrWidth - 1 downto 0);
  signal IXReg  : std_logic_vector(AddrWidth - 1 downto 0);
  
  -- Señales del camino de datos
  signal AluOut  : std_logic_vector(DataWidth - 1 downto 0);
  signal DataIn  : std_logic_vector(DataWidth - 1 downto 0);
  signal DataOut : std_logic_vector(DataWidth - 1 downto 0);
  signal AluIsZ  : std_logic;
  signal AluIsC  : std_logic;
  signal AluIsN  : std_logic;

  -- Señales de control
  signal IrEn    : std_logic;
  signal AEn     : std_logic;
  signal BEn     : std_logic;
  signal PCEn    : std_logic;
  signal PCRel   : std_logic;
  signal StEn    : std_logic;
  signal AdEn    : std_logic;
  signal IxEn    : std_logic;
  signal FromAlu : std_logic;
  signal PCInc   : std_logic;
  signal IxInc   : std_logic;
  signal IxDec   : std_logic;
  signal AdLow   : std_logic;
  signal R_WAux  : std_logic;
  signal AddRegOut   : std_logic;
  signal IxRegOut    : std_logic;
  signal AluOp   : AluOpType;

  -- Señales de la unidad de control
  signal State    : StateType;
  signal NewState : StateType;
  signal IrDecod  : InstrType;  -- Instrucción decodificada

begin

  Addr    <= PCReg after 1 ns when AddRegOut = '0' and IxRegOut='0' 
             else IXReg after 1 ns when AddRegOut = '0' and IxRegOut='1'
             else AdReg after 1 ns;
               
  R_W     <= R_WAux;    
  Data    <= DataOut after 2 ns when R_WAux = '0' 
               else (others => 'Z') after 2 ns;
  DataIn  <= Data;
  DataOut <= AReg;
  
  Registros : process (Clk, Reset)
  begin
    if Reset = '0' then 
      AReg  <= (others => '0');
      BReg  <= (others => '0');
      PCReg <= (others => '0');
      AdReg <= (others => '0');
      StReg <= (others => '0');
      InReg <= (others => '0');
      IXReg <= (others => '0');
    elsif Clk'event and Clk = '1' then
      -- Registro A
      if AEn = '1' then 
        if FromAlu = '1' then
          AReg <= AluOut;
        else
          AReg <= DataIn;
        end if;
      end if;
      -- Registro B
      if BEn = '1' then
        BReg <= AReg;
      end if;
      -- Contador de programa
      if PCEn = '1' then
        if PCInc = '1' then
          PCReg <= PCReg + 1;
        elsif PCRel = '1' then
          PCReg <= PCReg + AdReg(DataWidth - 1 downto 0);
        else
          PCReg <= AdReg;
        end if;
      end if;
      -- Registro de direcciones
      if AdEn = '1' then
        if AdLow = '1' then
          AdReg(DataWidth - 1 downto 0) <= DataIn;
          -- Extensión de signo
          AdReg(AddrWidth - 1 downto DataWidth) 
            <= (others => DataIn(DataWidth - 1));
        else 
          AdReg(AddrWidth - 1 downto DataWidth) 
            <= DataIn(AddrWidth - DataWidth - 1 downto 0);
        end if;
      end if;
      -- Registro de indexado
      if IxEn = '1' then
        if IxInc = '1' then --Por poner una prioridad...
          IXReg<=IXReg+1;
        elsif IxDec = '1' then
          IXReg<=IXReg-1;        
        else --Carga de datos
          if AdLow = '1' then
            IXReg(DataWidth - 1 downto 0) <= DataIn;
            -- Extensión de signo
            IXReg(AddrWidth - 1 downto DataWidth) 
              <= (others => DataIn(DataWidth - 1));
          elsif AdLow = '0' then 
            IXReg(AddrWidth - 1 downto DataWidth) 
              <= DataIn(AddrWidth - DataWidth - 1 downto 0);
          end if;
        end if;
      end if; 
      -- Registro de condiciones
      if StEn = '1' then
        StReg <= (others => '0');
        StReg(DataWidth - 1 downto DataWidth - 3) <= AluIsZ & AluIsN & AluIsC;
      end if;
      -- Registro de instrucciones
      if IrEn = '1' then
        InReg <= DataIn;
      end if;
    end if;
  end process;

  ALU : process (AReg, BReg, AluOp)
    -- result tiene un bit más que los datos para recoger el acarreo
    variable result : std_logic_vector(DataWidth downto 0);
    variable A      : std_logic_vector(DataWidth downto 0);
    variable B      : std_logic_vector(DataWidth downto 0);
  begin
    -- Ampliamos los datos con extension de signo (por el tema del acarreo)
    A := AReg(DataWidth - 1) & AReg;
    B := BReg(DataWidth - 1) & BReg;
    -- Operaciones de la ALU
    case AluOp is
      when A_NOP => 
        result := B; -- si acaso,lo aprovecharíamos para hacer A <- B
      when A_ADD => 
        result := A + B;
      when A_SUB =>
        result := A - B;
      when A_AND => 
        result := A and B;
      when A_OR  =>
        result := A or B;
      when A_NOT => 
        result := not A;
    end case;
    -- Actualización de señales para el registro de condiciones
    AluIsZ <= '0';
    AluIsN <= '0';
    AluIsC <= '0';
    if result = 0 then 
      AluIsZ <= '1';
    elsif result < 0 then
      AluIsN <= '1';
    end if;
    if (result(DataWidth) xor result(DataWidth - 1)) = '1' then
      AluIsC <= '1';
    end if;
    -- Actualización de la salida con simulación de retardo
    AluOut <= result(DataWidth - 1 downto 0) after AluDelay;
  end process;

  ControlSeq  : process (Clk, Reset)
  begin
    if reset = '0' then
      State   <= S_FETCH;
    elsif Clk'event and Clk = '1' then
      State   <= NewState;
    end if;
  end process;

  ControlComb : process (State, IrDecod, StReg)
    variable RelJump : std_logic;
  begin
      -- Por defecto todas las señales están inactivas
      RelJump := '0';
      IrEn    <= '0';
      AEn     <= '0';
      BEn     <= '0';
      PCEn    <= '0';
      StEn    <= '0';
      AdEn    <= '0';
      FromAlu <= '0';
      PCInc   <= '0';
      IxInc   <= '0';
      IxDec   <= '0';
      PCRel   <= '0';
      AdLow   <= '0';
      IxEn    <= '0';
      AddRegOut <= '0';
      IxRegOut  <= '0';
      R_WAux    <= '1';
      MemAcc    <= '0';
      AluOp     <= A_NOP; 
      -- Empezamos a decodificar
      case State is
        when S_FETCH => 
          MemAcc   <= '1';
          IrEn     <= '1';
          PCEn     <= '1';
          PCInc    <= '1';
          NewState <= S_DECOD;
        when S_DECOD =>
          case IrDecod is 
            when I_NOP  => 
              NewState <= S_PCINC;
            when I_LDAI | I_STAX | I_LDAX  => 
              NewState <= S_RDWR;
            when I_LDAM | I_STA =>
              NewState <= S_GETLSB;
            when I_MOV  => 
              NewState <= S_EXECI;
            when I_ADD | I_SUB | I_AND | I_OR | I_NOT | I_INX | I_DEX => 
              NewState <= S_EXECI;
            when I_BEQ | I_BNE | I_BCS | I_BCC | I_BMI | I_BPL | I_LDXI => 
              NewState <= S_GETLSB;
            when I_JMP  =>
              NewState <= S_GETLSB;
            when I_NOI  => 
              -- Aquí debería dar error !! 
              NewState <= S_PCINC;
          end case;
        when S_GETLSB => 
          MemAcc <= '1'; 
          AdLow  <= '1';
          case IrDecod is 
            --when I_LDAI =>
              --AdEn   <= '1'; 
              --NewState <= S_RDWR;
            when I_LDAM | I_STA | I_JMP =>
              AdEn   <= '1';
              PCEn   <= '1'; 
              PCInc  <= '1'; 
              NewState <= S_GETMSB;
            when I_BEQ | I_BNE | I_BCS | I_BCC | I_BMI | I_BPL => 
              AdEn     <= '1';
              NewState <= S_EXECI;
            when I_LDXI =>
              IxEn     <= '1';
              PCEn   <= '1'; 
              PCInc  <= '1'; 
              NewState <= S_GETMSB;
            when others => 
              -- Aquí debería dar error !! 
              NewState <= S_PCINC;
          end case;
        when S_GETMSB => 
          MemAcc <= '1'; 
          case IrDecod is 
            when I_LDAM | I_STA =>
              AdEn   <= '1';
              NewState <= S_RDWR;
            when I_JMP  =>
              AdEn   <= '1';
              NewState <= S_EXECI;
            when I_LDXI =>
              IxEn    <= '1';
              NewState <= S_PCINC;
            when others => 
              -- Aquí debería dar error !! 
              NewState <= S_PCINC;
          end case;
        when S_EXECI => 
          NewState   <= S_PCINC;
          case IrDecod is
            when I_MOV  => 
              BEn     <= '1';
            when I_ADD  => 
              AluOp <= A_ADD; 
              AEn     <= '1'; FromAlu <= '1'; StEn    <= '1';
            when I_SUB  => 
              AluOp <= A_SUB;
              AEn     <= '1'; FromAlu <= '1'; StEn    <= '1';
            when I_AND  => 
              AluOp <= A_AND;
              AEn     <= '1'; FromAlu <= '1'; StEn    <= '1';
            when I_OR   => 
              AluOp <= A_OR ;
              AEn     <= '1'; FromAlu <= '1'; StEn    <= '1';
            when I_NOT  => 
              AluOp <= A_NOT;
              AEn     <= '1'; FromAlu <= '1'; StEn    <= '1';
            when I_BEQ => 
              RelJump := StReg(DataWidth - 1);  -- AluIsZ
            when I_BNE  => 
              RelJump := not StReg(DataWidth - 1); 
              PCRel  <= '1';              
            when I_BCS  => 
              RelJump := StReg(DataWidth - 2);  -- AluIsC
            when I_BCC  => 
              RelJump := not StReg(DataWidth - 2);
            when I_BPL  => 
              RelJump := StReg(DataWidth - 3);  -- AluIsN
            when I_BMI  => 
              RelJump := not StReg(DataWidth - 3);
            when I_JMP => 
              PCEn  <= '1';
              PCRel <= '0';
              PCInc <= '0';
            when I_INX =>
              IxEn    <= '1';
              IxInc   <= '1';
            when I_DEX =>
              IxEn    <= '1';
              IxDec   <= '1';  
            when others => 
              -- Aquí debería dar error !! 
              NewState <= S_PCINC;
          end case;
          if RelJump = '1' then
            PCEn     <= '1';
            PCRel    <= '1';
            PCInc    <= '0';
            NewState <= S_FETCH;
          end if;             
        when S_RDWR =>
          NewState  <= S_PCINC;
          case IrDecod is
            when I_LDAI => 
              MemAcc    <= '1';
              AEn       <= '1';   
            when I_LDAM =>
              AddRegOut <= '1';
              MemAcc    <= '1';
              AEn       <= '1';   
            when I_STA  =>
              AddRegOut <= '1';
              MemAcc    <= '1';
              R_WAux    <= '0'; 
            when I_STAX =>
              IxRegOut  <= '1';
              MemAcc    <= '1';
              R_WAux    <= '0'; 
            when I_LDAX =>  
              IxRegOut  <= '1';
              MemAcc    <= '1';
              AEn       <= '1';              
            when others =>
              -- Aquí debería dar error !! 
              NewState  <= S_PCINC;
          end case;
        when S_PCINC =>
          case IrDecod is 
            when I_LDAI | I_LDAM | I_STA =>
              PCEn  <= '1';
              PCInc <= '1';
            when I_BEQ | I_BNE | I_BCS | I_BCC | I_BMI | I_BPL => 
              PCEn  <= '1';
              PCInc <= '1';
            when others  => 
          end case;       
          NewState   <= S_FETCH;
      end case;
  end process;

  -- Por compatibilidad del código y comodidad en la simulación
  -- se decodifican las instrucciones que vienen en std_logic_vector
  -- a un tipo interno
  IRDecoder : process (InReg)
    -- variable añadida para simular retardos en la unidad de control
    variable ird : InstrType;
  begin
    case InReg is
      when "00000000" => ird := I_NOP;  -- No operacion
      when "11000000" => ird := I_LDAI; -- Carga de A en modo inmediato
      when "11000001" => ird := I_LDAM; -- Carga de A en modo directo
      when "11000010" => ird := I_STA;  -- Guarda A en modo directo
      when "11000110" => ird := I_MOV;  -- Copia A en B
      when "11100000" => ird := I_ADD;  -- Suma
      when "11100001" => ird := I_SUB;  -- Resta
      when "11100010" => ird := I_AND;  -- And lógico bit a bit
      when "11100011" => ird := I_OR;   -- Or lógico bit a bit
      when "11100110" => ird := I_NOT;  -- Negación lógica
      when "11110000" => ird := I_BEQ;  -- Salto relativo si cero (Z = 1)
      when "11110001" => ird := I_BNE;  -- Idem si no cero (Z = 0)
      when "11110010" => ird := I_BCS;  -- Idem si acarreo    (C = 1)
      when "11110011" => ird := I_BCC;  -- Idem si no acarreo (C = 0)
      when "11110110" => ird := I_BMI;  -- Idem si negativo (N = 1)
      when "11110111" => ird := I_BPL;  -- Idem si positivo (N = 0)
      when "11111000" => ird := I_JMP;  -- Salto incondicional
      when "10000000" => ird := I_LDXI; -- Carga de X en modo inmediato
      when "10000001" => ird := I_INX;  -- Incremento de X
      when "10000010" => ird := I_DEX;  -- Decremento de X
      when "10000011" => ird := I_LDAX; -- Carga de A en modo indexado
      when "10000100" => ird := I_STAX; -- Guarda A en modo indexado
      when others     => ird := I_NOI;  -- No Instrumentacion
    end case;
    IrDecod <= ird after DecodDelay;
  end process;

end SemiFunc;
