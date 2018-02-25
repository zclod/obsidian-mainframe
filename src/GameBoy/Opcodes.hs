module GameBoy.Opcodes (

    ) where

import           Protolude
import           Data.Word

data Register
    -- accumulator / flags
    = A | F
    -- general purpose
    | B | C
    | D | E
    | H | L
    -- stack pointer
    | SP
    -- program counter
    | PC

data Flag
    = Zf -- zero
    | Nf -- subtract
    | Hf -- half carry
    | Cf -- carry

data Operand
    = Reg8   Register             -- 8 bit register
    | Reg16 (Register, Register)  -- 16 bit register
    | D8                          -- 8 bit data
    | D16                         -- 16 bit data
    | A8                          -- 8 bit address addedd to 0xff00
    | A16                         -- 16 bit address
    | R8                          -- 8 bit signed data that are addedd to program counter

data Args
    = Unit
    | Unary Operand
    | Bynary Operand Operand

-- istruction set
data Mnemonic
    = LD  | LDH  | LDHL | PUSH
    | POP | ADD  | ADC  | SUB
    | SBC | AND  | OR   | XOR
    | CP  | INC  | DEC  | SWAP
    | DDA | CPL  | CCF  | SCF
    | NOP | HALT | STOP | DI
    | EI  | RLCA | RLA  | RRCA
    | RRA | RLC  | RL   | RRC
    | RR  | SLA  | SRA  | SRL
    | BIT | SET  | RES  | JP
    | JR  | CALL | RST  | RET
    | RETI

data Opcode
    = Opcode
        { _raw       :: Either Word8 Word16
        , _mnem      :: Mnemonic
        , _len       :: Int
        , _cycles    :: Int
        , _flags     :: [Flag]
        , _arguments :: Args
        }
