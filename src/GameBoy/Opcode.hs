module GameBoy.Opcode (
      Register8 (..)
    , Register16 (..)
    , Flag (..)
    , Operand (..)
    , Args (..)
    , Istruction (..)
    , Opcode (..)
    , decodeOpcode
    ) where

import           Protolude
import           Data.Word

data Register8
    -- accumulator / flags
    = A | F
    -- general purpose
    | B | C
    | D | E
    | H | L

data Register16
    = AF | BC | DE | HL
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
    = Reg8  Register8            -- register
    | Reg16 Register16           -- pair of register
    | Cond [Flag]                -- condition based on flags
    | Bit Int                    -- index for bitwise operations
    | D8                         -- 8 bit data
    | D16                        -- 16 bit data (LS byte first)
    | A8                         -- 8 bit address addedd to 0xff00
    | AC                         -- address located at 0xff00 + value in C register
    | AH Int                     -- address located at 0x0000 + Int
    | AReg16 Register16          -- address contained in the 16 bit register
    | A16                        -- 16 bit address
    | R8                         -- 8 bit signed data that are addedd to program counter

data Args
    = Unit
    | UnOp  Operand
    | BinOp Operand Operand

-- istruction set
data Istruction
    = LD   | LDH  | LDD  | LDI
    | LDHL | PUSH | POP  | ADD
    | ADC  | SUB  | SBC  | AND
    | OR   | XOR  | CP   | INC
    | DEC  | SWAP | DDA  | CPL
    | CCF  | SCF  | NOP  | HALT
    | STOP | DI   | EI   | RLCA
    | RLA  | RRCA | RRA  | RLC
    | RL   | RRC  | RR   | SLA
    | SRA  | SRL  | BIT  | SET
    | RES  | JP   | JR   | CALL
    | RST  | RET  | RETI | DAA

data Opcode
    = Opcode
        { _istruction :: Istruction
        , _arguments  :: Args
        , _cycles     :: [Int]                 -- clock cycle required
        , _len        :: Int                   -- istruction lenght in bytes
        , _raw        :: Either Word8 Word16   -- binary opcode
        }

decodeOpcode :: Either Word8 Word16 -> Opcode
decodeOpcode (Left w) = decodeOpcodeW8 w
decodeOpcode (Right w) = decodeOpcodeW16 w

decodeOpcodeW8 :: Word8 -> Opcode
decodeOpcodeW8 w = Opcode istr args cycles len (Left w)
    where
    (istr, args, cycles, len) = case w of
        0x00 -> (NOP, Unit, [4], 1)
        0x01 -> (LD, BinOp (Reg16 BC) D16, [12], 3)
        0x10 -> (STOP, Unit, [4], 2)
        0x11 -> (LD, BinOp (Reg16 DE) D16, [12], 3)
        0x12 -> (LD, BinOp (AReg16 DE) (Reg8 A), [8], 1)
        0x13 -> (INC, UnOp (Reg16 DE), [8], 1)
        0x14 -> (INC, UnOp (Reg8 D), [4], 1)
        0x15 -> (DEC, UnOp (Reg8 D), [4], 1)
        0x16 -> (LD, BinOp (Reg8 D) D8, [8], 2)
        0x17 -> (RLA, Unit, [4], 1)
        0x18 -> (JR, UnOp R8, [12], 2)
        0x19 -> (ADD, BinOp (Reg16 HL) (Reg16 DE), [8], 1)
        0x1a -> (LD, BinOp (Reg8 A) (AReg16 DE), [8], 1)
        0x1b -> (DEC, UnOp (Reg16 DE), [8], 1)
        0x1c -> (INC, UnOp (Reg8 E), [4], 1)
        0x1d -> (DEC, UnOp (Reg8 E), [4], 1)
        0x1e -> (LD, BinOp (Reg8 E) D8, [8], 2)
        0x1f -> (RRA, Unit, [4], 1)
        0x02 -> (LD, BinOp (AReg16 BC) (Reg8 A), [8], 1)
        0x20 -> (JR, BinOp (Cond [Nf, Zf]) R8, [12 , 8], 2)
        0x21 -> (LD, BinOp (Reg16 HL) D16, [12], 3)
        0x22 -> (LDI, BinOp (AReg16 HL) (Reg8 A), [8], 1)
        0x23 -> (INC, UnOp (Reg16 HL), [8], 1)
        0x24 -> (INC, UnOp (Reg8 H), [4], 1)
        0x25 -> (DEC, UnOp (Reg8 H), [4], 1)
        0x26 -> (LD, BinOp (Reg8 H) D8, [8], 2)
        0x27 -> (DAA, Unit, [4], 1)
        0x28 -> (JR, BinOp (Cond [Zf]) R8, [12 , 8], 2)
        0x29 -> (ADD, BinOp (Reg16 HL) (Reg16 HL), [8], 1)
        0x2a -> (LDI, BinOp (Reg8 A) (AReg16 HL), [8], 1)
        0x2b -> (DEC, UnOp (Reg16 HL), [8], 1)
        0x2c -> (INC, UnOp (Reg8 L), [4], 1)
        0x2d -> (DEC, UnOp (Reg8 L), [4], 1)
        0x2e -> (LD, BinOp (Reg8 L) D8, [8], 2)
        0x2f -> (CPL, Unit, [4], 1)
        0x03 -> (INC, UnOp (Reg16 BC), [8], 1)
        0x30 -> (JR, BinOp (Cond [Nf, Cf]) R8, [12 , 8], 2)
        0x31 -> (LD, BinOp (Reg16 SP) D16, [12], 3)
        0x32 -> (LDD, BinOp (AReg16 HL) (Reg8 A), [8], 1)
        0x33 -> (INC, UnOp (Reg16 SP), [8], 1)
        0x34 -> (INC, UnOp (AReg16 HL), [12], 1)
        0x35 -> (DEC, UnOp (AReg16 HL), [12], 1)
        0x36 -> (LD, BinOp (AReg16 HL) D8, [12], 2)
        0x37 -> (SCF, Unit, [4], 1)
        0x38 -> (JR, BinOp (Reg8 C) R8, [12 , 8], 2)
        0x39 -> (ADD, BinOp (Reg16 HL) (Reg16 SP), [8], 1)
        0x3a -> (LDD, BinOp (Reg8 A) (AReg16 HL), [8], 1)
        0x3b -> (DEC, UnOp (Reg16 SP), [8], 1)
        0x3c -> (INC, UnOp (Reg8 A), [4], 1)
        0x3d -> (DEC, UnOp (Reg8 A), [4], 1)
        0x3e -> (LD, BinOp (Reg8 A) D8, [8], 2)
        0x3f -> (CCF, Unit, [4], 1)
        0x04 -> (INC, UnOp (Reg8 B), [4], 1)
        0x40 -> (LD, BinOp (Reg8 B) (Reg8 B), [4], 1)
        0x41 -> (LD, BinOp (Reg8 B) (Reg8 C), [4], 1)
        0x42 -> (LD, BinOp (Reg8 B) (Reg8 D), [4], 1)
        0x43 -> (LD, BinOp (Reg8 B) (Reg8 E), [4], 1)
        0x44 -> (LD, BinOp (Reg8 B) (Reg8 H), [4], 1)
        0x45 -> (LD, BinOp (Reg8 B) (Reg8 L), [4], 1)
        0x46 -> (LD, BinOp (Reg8 B) (AReg16 HL), [8], 1)
        0x47 -> (LD, BinOp (Reg8 B) (Reg8 A), [4], 1)
        0x48 -> (LD, BinOp (Reg8 C) (Reg8 B), [4], 1)
        0x49 -> (LD, BinOp (Reg8 C) (Reg8 C), [4], 1)
        0x4a -> (LD, BinOp (Reg8 C) (Reg8 D), [4], 1)
        0x4b -> (LD, BinOp (Reg8 C) (Reg8 E), [4], 1)
        0x4c -> (LD, BinOp (Reg8 C) (Reg8 H), [4], 1)
        0x4d -> (LD, BinOp (Reg8 C) (Reg8 L), [4], 1)
        0x4e -> (LD, BinOp (Reg8 C) (AReg16 HL), [8], 1)
        0x4f -> (LD, BinOp (Reg8 C) (Reg8 A), [4], 1)
        0x05 -> (DEC, UnOp (Reg8 B), [4], 1)
        0x50 -> (LD, BinOp (Reg8 D) (Reg8 B), [4], 1)
        0x51 -> (LD, BinOp (Reg8 D) (Reg8 C), [4], 1)
        0x52 -> (LD, BinOp (Reg8 D) (Reg8 D), [4], 1)
        0x53 -> (LD, BinOp (Reg8 D) (Reg8 E), [4], 1)
        0x54 -> (LD, BinOp (Reg8 D) (Reg8 H), [4], 1)
        0x55 -> (LD, BinOp (Reg8 D) (Reg8 L), [4], 1)
        0x56 -> (LD, BinOp (Reg8 D) (AReg16 HL), [8], 1)
        0x57 -> (LD, BinOp (Reg8 D) (Reg8 A), [4], 1)
        0x58 -> (LD, BinOp (Reg8 E) (Reg8 B), [4], 1)
        0x59 -> (LD, BinOp (Reg8 E) (Reg8 C), [4], 1)
        0x5a -> (LD, BinOp (Reg8 E) (Reg8 D), [4], 1)
        0x5b -> (LD, BinOp (Reg8 E) (Reg8 E), [4], 1)
        0x5c -> (LD, BinOp (Reg8 E) (Reg8 H), [4], 1)
        0x5d -> (LD, BinOp (Reg8 E) (Reg8 L), [4], 1)
        0x5e -> (LD, BinOp (Reg8 E) (AReg16 HL), [8], 1)
        0x5f -> (LD, BinOp (Reg8 E) (Reg8 A), [4], 1)
        0x06 -> (LD, BinOp (Reg8 B) D8, [8], 2)
        0x60 -> (LD, BinOp (Reg8 H) (Reg8 B), [4], 1)
        0x61 -> (LD, BinOp (Reg8 H) (Reg8 C), [4], 1)
        0x62 -> (LD, BinOp (Reg8 H) (Reg8 D), [4], 1)
        0x63 -> (LD, BinOp (Reg8 H) (Reg8 E), [4], 1)
        0x64 -> (LD, BinOp (Reg8 H) (Reg8 H), [4], 1)
        0x65 -> (LD, BinOp (Reg8 H) (Reg8 L), [4], 1)
        0x66 -> (LD, BinOp (Reg8 H) (AReg16 HL), [8], 1)
        0x67 -> (LD, BinOp (Reg8 H) (Reg8 A), [4], 1)
        0x68 -> (LD, BinOp (Reg8 L) (Reg8 B), [4], 1)
        0x69 -> (LD, BinOp (Reg8 L) (Reg8 C), [4], 1)
        0x6a -> (LD, BinOp (Reg8 L) (Reg8 D), [4], 1)
        0x6b -> (LD, BinOp (Reg8 L) (Reg8 E), [4], 1)
        0x6c -> (LD, BinOp (Reg8 L) (Reg8 H), [4], 1)
        0x6d -> (LD, BinOp (Reg8 L) (Reg8 L), [4], 1)
        0x6e -> (LD, BinOp (Reg8 L) (AReg16 HL), [8], 1)
        0x6f -> (LD, BinOp (Reg8 L) (Reg8 A), [4], 1)
        0x07 -> (RLCA, Unit, [4], 1)
        0x70 -> (LD, BinOp (AReg16 HL) (Reg8 B), [8], 1)
        0x71 -> (LD, BinOp (AReg16 HL) (Reg8 C), [8], 1)
        0x72 -> (LD, BinOp (AReg16 HL) (Reg8 D), [8], 1)
        0x73 -> (LD, BinOp (AReg16 HL) (Reg8 E), [8], 1)
        0x74 -> (LD, BinOp (AReg16 HL) (Reg8 H), [8], 1)
        0x75 -> (LD, BinOp (AReg16 HL) (Reg8 L), [8], 1)
        0x76 -> (HALT, Unit, [4], 1)
        0x77 -> (LD, BinOp (AReg16 HL) (Reg8 A), [8], 1)
        0x78 -> (LD, BinOp (Reg8 A) (Reg8 B), [4], 1)
        0x79 -> (LD, BinOp (Reg8 A) (Reg8 C), [4], 1)
        0x7a -> (LD, BinOp (Reg8 A) (Reg8 D), [4], 1)
        0x7b -> (LD, BinOp (Reg8 A) (Reg8 E), [4], 1)
        0x7c -> (LD, BinOp (Reg8 A) (Reg8 H), [4], 1)
        0x7d -> (LD, BinOp (Reg8 A) (Reg8 L), [4], 1)
        0x7e -> (LD, BinOp (Reg8 A) (AReg16 HL), [8], 1)
        0x7f -> (LD, BinOp (Reg8 A) (Reg8 A), [4], 1)
        0x08 -> (LD, BinOp A16 (Reg16 SP), [20], 3)
        0x80 -> (ADD, BinOp (Reg8 A) (Reg8 B), [4], 1)
        0x81 -> (ADD, BinOp (Reg8 A) (Reg8 C), [4], 1)
        0x82 -> (ADD, BinOp (Reg8 A) (Reg8 D), [4], 1)
        0x83 -> (ADD, BinOp (Reg8 A) (Reg8 E), [4], 1)
        0x84 -> (ADD, BinOp (Reg8 A) (Reg8 H), [4], 1)
        0x85 -> (ADD, BinOp (Reg8 A) (Reg8 L), [4], 1)
        0x86 -> (ADD, BinOp (Reg8 A) (AReg16 HL), [8], 1)
        0x87 -> (ADD, BinOp (Reg8 A) (Reg8 A), [4], 1)
        0x88 -> (ADC, BinOp (Reg8 A) (Reg8 B), [4], 1)
        0x89 -> (ADC, BinOp (Reg8 A) (Reg8 C), [4], 1)
        0x8a -> (ADC, BinOp (Reg8 A) (Reg8 D), [4], 1)
        0x8b -> (ADC, BinOp (Reg8 A) (Reg8 E), [4], 1)
        0x8c -> (ADC, BinOp (Reg8 A) (Reg8 H), [4], 1)
        0x8d -> (ADC, BinOp (Reg8 A) (Reg8 L), [4], 1)
        0x8e -> (ADC, BinOp (Reg8 A) (AReg16 HL), [8], 1)
        0x8f -> (ADC, BinOp (Reg8 A) (Reg8 A), [4], 1)
        0x09 -> (ADD, BinOp (Reg16 HL) (Reg16 BC), [8], 1)
        0x90 -> (SUB, UnOp (Reg8 B), [4], 1)
        0x91 -> (SUB, UnOp (Reg8 C), [4], 1)
        0x92 -> (SUB, UnOp (Reg8 D), [4], 1)
        0x93 -> (SUB, UnOp (Reg8 E), [4], 1)
        0x94 -> (SUB, UnOp (Reg8 H), [4], 1)
        0x95 -> (SUB, UnOp (Reg8 L), [4], 1)
        0x96 -> (SUB, UnOp (AReg16 HL), [8], 1)
        0x97 -> (SUB, UnOp (Reg8 A), [4], 1)
        0x98 -> (SBC, BinOp (Reg8 A) (Reg8 B), [4], 1)
        0x99 -> (SBC, BinOp (Reg8 A) (Reg8 C), [4], 1)
        0x9a -> (SBC, BinOp (Reg8 A) (Reg8 D), [4], 1)
        0x9b -> (SBC, BinOp (Reg8 A) (Reg8 E), [4], 1)
        0x9c -> (SBC, BinOp (Reg8 A) (Reg8 H), [4], 1)
        0x9d -> (SBC, BinOp (Reg8 A) (Reg8 L), [4], 1)
        0x9e -> (SBC, BinOp (Reg8 A) (AReg16 HL), [8], 1)
        0x9f -> (SBC, BinOp (Reg8 A) (Reg8 A), [4], 1)
        0x0a -> (LD, BinOp (Reg8 A) (AReg16 BC), [8], 1)
        0xa0 -> (AND, UnOp (Reg8 B), [4], 1)
        0xa1 -> (AND, UnOp (Reg8 C), [4], 1)
        0xa2 -> (AND, UnOp (Reg8 D), [4], 1)
        0xa3 -> (AND, UnOp (Reg8 E), [4], 1)
        0xa4 -> (AND, UnOp (Reg8 H), [4], 1)
        0xa5 -> (AND, UnOp (Reg8 L), [4], 1)
        0xa6 -> (AND, UnOp (AReg16 HL), [8], 1)
        0xa7 -> (AND, UnOp (Reg8 A), [4], 1)
        0xa8 -> (XOR, UnOp (Reg8 B), [4], 1)
        0xa9 -> (XOR, UnOp (Reg8 C), [4], 1)
        0xaa -> (XOR, UnOp (Reg8 D), [4], 1)
        0xab -> (XOR, UnOp (Reg8 E), [4], 1)
        0xac -> (XOR, UnOp (Reg8 H), [4], 1)
        0xad -> (XOR, UnOp (Reg8 L), [4], 1)
        0xae -> (XOR, UnOp (AReg16 HL), [8], 1)
        0xaf -> (XOR, UnOp (Reg8 A), [4], 1)
        0x0b -> (DEC, UnOp (Reg16 BC), [8], 1)
        0xb0 -> (OR, UnOp (Reg8 B), [4], 1)
        0xb1 -> (OR, UnOp (Reg8 C), [4], 1)
        0xb2 -> (OR, UnOp (Reg8 D), [4], 1)
        0xb3 -> (OR, UnOp (Reg8 E), [4], 1)
        0xb4 -> (OR, UnOp (Reg8 H), [4], 1)
        0xb5 -> (OR, UnOp (Reg8 L), [4], 1)
        0xb6 -> (OR, UnOp (AReg16 HL), [8], 1)
        0xb7 -> (OR, UnOp (Reg8 A), [4], 1)
        0xb8 -> (CP, UnOp (Reg8 B), [4], 1)
        0xb9 -> (CP, UnOp (Reg8 C), [4], 1)
        0xba -> (CP, UnOp (Reg8 D), [4], 1)
        0xbb -> (CP, UnOp (Reg8 E), [4], 1)
        0xbc -> (CP, UnOp (Reg8 H), [4], 1)
        0xbd -> (CP, UnOp (Reg8 L), [4], 1)
        0xbe -> (CP, UnOp (AReg16 HL), [8], 1)
        0xbf -> (CP, UnOp (Reg8 A), [4], 1)
        0x0c -> (INC, UnOp (Reg8 C), [4], 1)
        0xc0 -> (RET, UnOp (Cond [Nf, Zf]), [20 , 8], 1)
        0xc1 -> (POP, UnOp (Reg16 BC), [12], 1)
        0xc2 -> (JP, BinOp (Cond [Nf, Zf]) A16, [16 , 12], 3)
        0xc3 -> (JP, UnOp A16, [16], 3)
        0xc4 -> (CALL, BinOp (Cond [Nf, Zf]) A16, [24 , 12], 3)
        0xc5 -> (PUSH, UnOp (Reg16 BC), [16], 1)
        0xc6 -> (ADD, BinOp (Reg8 A) D8, [8], 2)
        0xc7 -> (RST, UnOp (AH 0), [16], 1)
        0xc8 -> (RET, UnOp (Cond [Zf]), [20 , 8], 1)
        0xc9 -> (RET, Unit, [16], 1)
        0xca -> (JP, BinOp (Cond [Zf]) A16, [16 , 12], 3)
        -- 0xcb -> (PREFIX, UnOp (Reg16 CB), [4], 1)
        0xcc -> (CALL, BinOp (Cond [Zf]) A16, [24 , 12], 3)
        0xcd -> (CALL, UnOp A16, [24], 3)
        0xce -> (ADC, BinOp (Reg8 A) D8, [8], 2)
        0xcf -> (RST, UnOp (AH 8), [16], 1)
        0x0d -> (DEC, UnOp (Reg8 C), [4], 1)
        0xd0 -> (RET, UnOp (Cond [Nf, Cf]), [20 , 8], 1)
        0xd1 -> (POP, UnOp (Reg16 DE), [12], 1)
        0xd2 -> (JP, BinOp (Cond [Nf, Cf]) A16, [16 , 12], 3)
        0xd4 -> (CALL, BinOp (Cond [Nf, Cf]) A16, [24 , 12], 3)
        0xd5 -> (PUSH, UnOp (Reg16 DE), [16], 1)
        0xd6 -> (SUB, UnOp D8, [8], 2)
        0xd7 -> (RST, UnOp (AH 10), [16], 1)
        0xd8 -> (RET, UnOp (Reg8 C), [20 , 8], 1)
        0xd9 -> (RETI, Unit, [16], 1)
        0xda -> (JP, BinOp (Reg8 C) A16, [16 , 12], 3)
        0xdc -> (CALL, BinOp (Reg8 C) A16, [24 , 12], 3)
        0xde -> (SBC, BinOp (Reg8 A) D8, [8], 2)
        0xdf -> (RST, UnOp (AH 18), [16], 1)
        0x0e -> (LD, BinOp (Reg8 C) D8, [8], 2)
        0xe0 -> (LDH, BinOp A8 (Reg8 A), [12], 2)
        0xe1 -> (POP, UnOp (Reg16 HL), [12], 1)
        0xe2 -> (LD, BinOp AC (Reg8 A), [8], 2)
        0xe5 -> (PUSH, UnOp (Reg16 HL), [16], 1)
        0xe6 -> (AND, UnOp D8, [8], 2)
        0xe7 -> (RST, UnOp (AH 20), [16], 1)
        0xe8 -> (ADD, BinOp (Reg16 SP) R8, [16], 2)
        0xe9 -> (JP, UnOp (Reg16 HL), [4], 1)
        0xea -> (LD, BinOp A16 (Reg8 A), [16], 3)
        0xee -> (XOR, UnOp D8, [8], 2)
        0xef -> (RST, UnOp (AH 28), [16], 1)
        0x0f -> (RRCA, Unit, [4], 1)
        0xf0 -> (LDH, BinOp (Reg8 A) A8, [12], 2)
        0xf1 -> (POP, UnOp (Reg16 AF), [12], 1)
        0xf2 -> (LD, BinOp (Reg8 A) AC, [8], 2)
        0xf3 -> (DI, Unit, [4], 1)
        0xf5 -> (PUSH, UnOp (Reg16 AF), [16], 1)
        0xf6 -> (OR, UnOp D8, [8], 2)
        0xf7 -> (RST, UnOp (AH 30), [16], 1)
        0xf8 -> (LDHL, BinOp (Reg16 SP) R8, [12], 2)
        0xf9 -> (LD, BinOp (Reg16 SP) (Reg16 HL), [8], 1)
        0xfa -> (LD, BinOp (Reg8 A) A16, [16], 3)
        0xfb -> (EI, Unit, [4], 1)
        0xfe -> (CP, UnOp D8, [8], 2)
        0xff -> (RST, UnOp (AH 38), [16], 1)

decodeOpcodeW16 :: Word16 -> Opcode
decodeOpcodeW16 w = Opcode istr args cycles len (Right w)
    where
    (istr, args, cycles, len) = case w of
        0xcb00 -> (RLC, UnOp (Reg8 B), [8], 2)
        0xcb01 -> (RLC, UnOp (Reg8 C), [8], 2)
        0xcb10 -> (RL, UnOp (Reg8 B), [8], 2)
        0xcb11 -> (RL, UnOp (Reg8 C), [8], 2)
        0xcb12 -> (RL, UnOp (Reg8 D), [8], 2)
        0xcb13 -> (RL, UnOp (Reg8 E), [8], 2)
        0xcb14 -> (RL, UnOp (Reg8 H), [8], 2)
        0xcb15 -> (RL, UnOp (Reg8 L), [8], 2)
        0xcb16 -> (RL, UnOp (AReg16 HL), [16], 2)
        0xcb17 -> (RL, UnOp (Reg8 A), [8], 2)
        0xcb18 -> (RR, UnOp (Reg8 B), [8], 2)
        0xcb19 -> (RR, UnOp (Reg8 C), [8], 2)
        0xcb1a -> (RR, UnOp (Reg8 D), [8], 2)
        0xcb1b -> (RR, UnOp (Reg8 E), [8], 2)
        0xcb1c -> (RR, UnOp (Reg8 H), [8], 2)
        0xcb1d -> (RR, UnOp (Reg8 L), [8], 2)
        0xcb1e -> (RR, UnOp (AReg16 HL), [16], 2)
        0xcb1f -> (RR, UnOp (Reg8 A), [8], 2)
        0xcb02 -> (RLC, UnOp (Reg8 D), [8], 2)
        0xcb20 -> (SLA, UnOp (Reg8 B), [8], 2)
        0xcb21 -> (SLA, UnOp (Reg8 C), [8], 2)
        0xcb22 -> (SLA, UnOp (Reg8 D), [8], 2)
        0xcb23 -> (SLA, UnOp (Reg8 E), [8], 2)
        0xcb24 -> (SLA, UnOp (Reg8 H), [8], 2)
        0xcb25 -> (SLA, UnOp (Reg8 L), [8], 2)
        0xcb26 -> (SLA, UnOp (AReg16 HL), [16], 2)
        0xcb27 -> (SLA, UnOp (Reg8 A), [8], 2)
        0xcb28 -> (SRA, UnOp (Reg8 B), [8], 2)
        0xcb29 -> (SRA, UnOp (Reg8 C), [8], 2)
        0xcb2a -> (SRA, UnOp (Reg8 D), [8], 2)
        0xcb2b -> (SRA, UnOp (Reg8 E), [8], 2)
        0xcb2c -> (SRA, UnOp (Reg8 H), [8], 2)
        0xcb2d -> (SRA, UnOp (Reg8 L), [8], 2)
        0xcb2e -> (SRA, UnOp (AReg16 HL), [16], 2)
        0xcb2f -> (SRA, UnOp (Reg8 A), [8], 2)
        0xcb03 -> (RLC, UnOp (Reg8 E), [8], 2)
        0xcb30 -> (SWAP, UnOp (Reg8 B), [8], 2)
        0xcb31 -> (SWAP, UnOp (Reg8 C), [8], 2)
        0xcb32 -> (SWAP, UnOp (Reg8 D), [8], 2)
        0xcb33 -> (SWAP, UnOp (Reg8 E), [8], 2)
        0xcb34 -> (SWAP, UnOp (Reg8 H), [8], 2)
        0xcb35 -> (SWAP, UnOp (Reg8 L), [8], 2)
        0xcb36 -> (SWAP, UnOp (AReg16 HL), [16], 2)
        0xcb37 -> (SWAP, UnOp (Reg8 A), [8], 2)
        0xcb38 -> (SRL, UnOp (Reg8 B), [8], 2)
        0xcb39 -> (SRL, UnOp (Reg8 C), [8], 2)
        0xcb3a -> (SRL, UnOp (Reg8 D), [8], 2)
        0xcb3b -> (SRL, UnOp (Reg8 E), [8], 2)
        0xcb3c -> (SRL, UnOp (Reg8 H), [8], 2)
        0xcb3d -> (SRL, UnOp (Reg8 L), [8], 2)
        0xcb3e -> (SRL, UnOp (AReg16 HL), [16], 2)
        0xcb3f -> (SRL, UnOp (Reg8 A), [8], 2)
        0xcb04 -> (RLC, UnOp (Reg8 H), [8], 2)
        0xcb40 -> (BIT, BinOp (Bit 0) (Reg8 B), [8], 2)
        0xcb41 -> (BIT, BinOp (Bit 0) (Reg8 C), [8], 2)
        0xcb42 -> (BIT, BinOp (Bit 0) (Reg8 D), [8], 2)
        0xcb43 -> (BIT, BinOp (Bit 0) (Reg8 E), [8], 2)
        0xcb44 -> (BIT, BinOp (Bit 0) (Reg8 H), [8], 2)
        0xcb45 -> (BIT, BinOp (Bit 0) (Reg8 L), [8], 2)
        0xcb46 -> (BIT, BinOp (Bit 0) (AReg16 HL), [16], 2)
        0xcb47 -> (BIT, BinOp (Bit 0) (Reg8 A), [8], 2)
        0xcb48 -> (BIT, BinOp (Bit 1) (Reg8 B), [8], 2)
        0xcb49 -> (BIT, BinOp (Bit 1) (Reg8 C), [8], 2)
        0xcb4a -> (BIT, BinOp (Bit 1) (Reg8 D), [8], 2)
        0xcb4b -> (BIT, BinOp (Bit 1) (Reg8 E), [8], 2)
        0xcb4c -> (BIT, BinOp (Bit 1) (Reg8 H), [8], 2)
        0xcb4d -> (BIT, BinOp (Bit 1) (Reg8 L), [8], 2)
        0xcb4e -> (BIT, BinOp (Bit 1) (AReg16 HL), [16], 2)
        0xcb4f -> (BIT, BinOp (Bit 1) (Reg8 A), [8], 2)
        0xcb05 -> (RLC, UnOp (Reg8 L), [8], 2)
        0xcb50 -> (BIT, BinOp (Bit 2) (Reg8 B), [8], 2)
        0xcb51 -> (BIT, BinOp (Bit 2) (Reg8 C), [8], 2)
        0xcb52 -> (BIT, BinOp (Bit 2) (Reg8 D), [8], 2)
        0xcb53 -> (BIT, BinOp (Bit 2) (Reg8 E), [8], 2)
        0xcb54 -> (BIT, BinOp (Bit 2) (Reg8 H), [8], 2)
        0xcb55 -> (BIT, BinOp (Bit 2) (Reg8 L), [8], 2)
        0xcb56 -> (BIT, BinOp (Bit 2) (AReg16 HL), [16], 2)
        0xcb57 -> (BIT, BinOp (Bit 2) (Reg8 A), [8], 2)
        0xcb58 -> (BIT, BinOp (Bit 3) (Reg8 B), [8], 2)
        0xcb59 -> (BIT, BinOp (Bit 3) (Reg8 C), [8], 2)
        0xcb5a -> (BIT, BinOp (Bit 3) (Reg8 D), [8], 2)
        0xcb5b -> (BIT, BinOp (Bit 3) (Reg8 E), [8], 2)
        0xcb5c -> (BIT, BinOp (Bit 3) (Reg8 H), [8], 2)
        0xcb5d -> (BIT, BinOp (Bit 3) (Reg8 L), [8], 2)
        0xcb5e -> (BIT, BinOp (Bit 3) (AReg16 HL), [16], 2)
        0xcb5f -> (BIT, BinOp (Bit 3) (Reg8 A), [8], 2)
        0xcb06 -> (RLC, UnOp (AReg16 HL), [16], 2)
        0xcb60 -> (BIT, BinOp (Bit 4) (Reg8 B), [8], 2)
        0xcb61 -> (BIT, BinOp (Bit 4) (Reg8 C), [8], 2)
        0xcb62 -> (BIT, BinOp (Bit 4) (Reg8 D), [8], 2)
        0xcb63 -> (BIT, BinOp (Bit 4) (Reg8 E), [8], 2)
        0xcb64 -> (BIT, BinOp (Bit 4) (Reg8 H), [8], 2)
        0xcb65 -> (BIT, BinOp (Bit 4) (Reg8 L), [8], 2)
        0xcb66 -> (BIT, BinOp (Bit 4) (AReg16 HL), [16], 2)
        0xcb67 -> (BIT, BinOp (Bit 4) (Reg8 A), [8], 2)
        0xcb68 -> (BIT, BinOp (Bit 5) (Reg8 B), [8], 2)
        0xcb69 -> (BIT, BinOp (Bit 5) (Reg8 C), [8], 2)
        0xcb6a -> (BIT, BinOp (Bit 5) (Reg8 D), [8], 2)
        0xcb6b -> (BIT, BinOp (Bit 5) (Reg8 E), [8], 2)
        0xcb6c -> (BIT, BinOp (Bit 5) (Reg8 H), [8], 2)
        0xcb6d -> (BIT, BinOp (Bit 5) (Reg8 L), [8], 2)
        0xcb6e -> (BIT, BinOp (Bit 5) (AReg16 HL), [16], 2)
        0xcb6f -> (BIT, BinOp (Bit 5) (Reg8 A), [8], 2)
        0xcb07 -> (RLC, UnOp (Reg8 A), [8], 2)
        0xcb70 -> (BIT, BinOp (Bit 6) (Reg8 B), [8], 2)
        0xcb71 -> (BIT, BinOp (Bit 6) (Reg8 C), [8], 2)
        0xcb72 -> (BIT, BinOp (Bit 6) (Reg8 D), [8], 2)
        0xcb73 -> (BIT, BinOp (Bit 6) (Reg8 E), [8], 2)
        0xcb74 -> (BIT, BinOp (Bit 6) (Reg8 H), [8], 2)
        0xcb75 -> (BIT, BinOp (Bit 6) (Reg8 L), [8], 2)
        0xcb76 -> (BIT, BinOp (Bit 6) (AReg16 HL), [16], 2)
        0xcb77 -> (BIT, BinOp (Bit 6) (Reg8 A), [8], 2)
        0xcb78 -> (BIT, BinOp (Bit 7) (Reg8 B), [8], 2)
        0xcb79 -> (BIT, BinOp (Bit 7) (Reg8 C), [8], 2)
        0xcb7a -> (BIT, BinOp (Bit 7) (Reg8 D), [8], 2)
        0xcb7b -> (BIT, BinOp (Bit 7) (Reg8 E), [8], 2)
        0xcb7c -> (BIT, BinOp (Bit 7) (Reg8 H), [8], 2)
        0xcb7d -> (BIT, BinOp (Bit 7) (Reg8 L), [8], 2)
        0xcb7e -> (BIT, BinOp (Bit 7) (AReg16 HL), [16], 2)
        0xcb7f -> (BIT, BinOp (Bit 7) (Reg8 A), [8], 2)
        0xcb08 -> (RRC, UnOp (Reg8 B), [8], 2)
        0xcb80 -> (RES, BinOp (Bit 0) (Reg8 B), [8], 2)
        0xcb81 -> (RES, BinOp (Bit 0) (Reg8 C), [8], 2)
        0xcb82 -> (RES, BinOp (Bit 0) (Reg8 D), [8], 2)
        0xcb83 -> (RES, BinOp (Bit 0) (Reg8 E), [8], 2)
        0xcb84 -> (RES, BinOp (Bit 0) (Reg8 H), [8], 2)
        0xcb85 -> (RES, BinOp (Bit 0) (Reg8 L), [8], 2)
        0xcb86 -> (RES, BinOp (Bit 0) (AReg16 HL), [16], 2)
        0xcb87 -> (RES, BinOp (Bit 0) (Reg8 A), [8], 2)
        0xcb88 -> (RES, BinOp (Bit 1) (Reg8 B), [8], 2)
        0xcb89 -> (RES, BinOp (Bit 1) (Reg8 C), [8], 2)
        0xcb8a -> (RES, BinOp (Bit 1) (Reg8 D), [8], 2)
        0xcb8b -> (RES, BinOp (Bit 1) (Reg8 E), [8], 2)
        0xcb8c -> (RES, BinOp (Bit 1) (Reg8 H), [8], 2)
        0xcb8d -> (RES, BinOp (Bit 1) (Reg8 L), [8], 2)
        0xcb8e -> (RES, BinOp (Bit 1) (AReg16 HL), [16], 2)
        0xcb8f -> (RES, BinOp (Bit 1) (Reg8 A), [8], 2)
        0xcb09 -> (RRC, UnOp (Reg8 C), [8], 2)
        0xcb90 -> (RES, BinOp (Bit 2) (Reg8 B), [8], 2)
        0xcb91 -> (RES, BinOp (Bit 2) (Reg8 C), [8], 2)
        0xcb92 -> (RES, BinOp (Bit 2) (Reg8 D), [8], 2)
        0xcb93 -> (RES, BinOp (Bit 2) (Reg8 E), [8], 2)
        0xcb94 -> (RES, BinOp (Bit 2) (Reg8 H), [8], 2)
        0xcb95 -> (RES, BinOp (Bit 2) (Reg8 L), [8], 2)
        0xcb96 -> (RES, BinOp (Bit 2) (AReg16 HL), [16], 2)
        0xcb97 -> (RES, BinOp (Bit 2) (Reg8 A), [8], 2)
        0xcb98 -> (RES, BinOp (Bit 3) (Reg8 B), [8], 2)
        0xcb99 -> (RES, BinOp (Bit 3) (Reg8 C), [8], 2)
        0xcb9a -> (RES, BinOp (Bit 3) (Reg8 D), [8], 2)
        0xcb9b -> (RES, BinOp (Bit 3) (Reg8 E), [8], 2)
        0xcb9c -> (RES, BinOp (Bit 3) (Reg8 H), [8], 2)
        0xcb9d -> (RES, BinOp (Bit 3) (Reg8 L), [8], 2)
        0xcb9e -> (RES, BinOp (Bit 3) (AReg16 HL), [16], 2)
        0xcb9f -> (RES, BinOp (Bit 3) (Reg8 A), [8], 2)
        0xcb0a -> (RRC, UnOp (Reg8 D), [8], 2)
        0xcba0 -> (RES, BinOp (Bit 4) (Reg8 B), [8], 2)
        0xcba1 -> (RES, BinOp (Bit 4) (Reg8 C), [8], 2)
        0xcba2 -> (RES, BinOp (Bit 4) (Reg8 D), [8], 2)
        0xcba3 -> (RES, BinOp (Bit 4) (Reg8 E), [8], 2)
        0xcba4 -> (RES, BinOp (Bit 4) (Reg8 H), [8], 2)
        0xcba5 -> (RES, BinOp (Bit 4) (Reg8 L), [8], 2)
        0xcba6 -> (RES, BinOp (Bit 4) (AReg16 HL), [16], 2)
        0xcba7 -> (RES, BinOp (Bit 4) (Reg8 A), [8], 2)
        0xcba8 -> (RES, BinOp (Bit 5) (Reg8 B), [8], 2)
        0xcba9 -> (RES, BinOp (Bit 5) (Reg8 C), [8], 2)
        0xcbaa -> (RES, BinOp (Bit 5) (Reg8 D), [8], 2)
        0xcbab -> (RES, BinOp (Bit 5) (Reg8 E), [8], 2)
        0xcbac -> (RES, BinOp (Bit 5) (Reg8 H), [8], 2)
        0xcbad -> (RES, BinOp (Bit 5) (Reg8 L), [8], 2)
        0xcbae -> (RES, BinOp (Bit 5) (AReg16 HL), [16], 2)
        0xcbaf -> (RES, BinOp (Bit 5) (Reg8 A), [8], 2)
        0xcb0b -> (RRC, UnOp (Reg8 E), [8], 2)
        0xcbb0 -> (RES, BinOp (Bit 6) (Reg8 B), [8], 2)
        0xcbb1 -> (RES, BinOp (Bit 6) (Reg8 C), [8], 2)
        0xcbb2 -> (RES, BinOp (Bit 6) (Reg8 D), [8], 2)
        0xcbb3 -> (RES, BinOp (Bit 6) (Reg8 E), [8], 2)
        0xcbb4 -> (RES, BinOp (Bit 6) (Reg8 H), [8], 2)
        0xcbb5 -> (RES, BinOp (Bit 6) (Reg8 L), [8], 2)
        0xcbb6 -> (RES, BinOp (Bit 6) (AReg16 HL), [16], 2)
        0xcbb7 -> (RES, BinOp (Bit 6) (Reg8 A), [8], 2)
        0xcbb8 -> (RES, BinOp (Bit 7) (Reg8 B), [8], 2)
        0xcbb9 -> (RES, BinOp (Bit 7) (Reg8 C), [8], 2)
        0xcbba -> (RES, BinOp (Bit 7) (Reg8 D), [8], 2)
        0xcbbb -> (RES, BinOp (Bit 7) (Reg8 E), [8], 2)
        0xcbbc -> (RES, BinOp (Bit 7) (Reg8 H), [8], 2)
        0xcbbd -> (RES, BinOp (Bit 7) (Reg8 L), [8], 2)
        0xcbbe -> (RES, BinOp (Bit 7) (AReg16 HL), [16], 2)
        0xcbbf -> (RES, BinOp (Bit 7) (Reg8 A), [8], 2)
        0xcb0c -> (RRC, UnOp (Reg8 H), [8], 2)
        0xcbc0 -> (SET, BinOp (Bit 0) (Reg8 B), [8], 2)
        0xcbc1 -> (SET, BinOp (Bit 0) (Reg8 C), [8], 2)
        0xcbc2 -> (SET, BinOp (Bit 0) (Reg8 D), [8], 2)
        0xcbc3 -> (SET, BinOp (Bit 0) (Reg8 E), [8], 2)
        0xcbc4 -> (SET, BinOp (Bit 0) (Reg8 H), [8], 2)
        0xcbc5 -> (SET, BinOp (Bit 0) (Reg8 L), [8], 2)
        0xcbc6 -> (SET, BinOp (Bit 0) (AReg16 HL), [16], 2)
        0xcbc7 -> (SET, BinOp (Bit 0) (Reg8 A), [8], 2)
        0xcbc8 -> (SET, BinOp (Bit 1) (Reg8 B), [8], 2)
        0xcbc9 -> (SET, BinOp (Bit 1) (Reg8 C), [8], 2)
        0xcbca -> (SET, BinOp (Bit 1) (Reg8 D), [8], 2)
        0xcbcb -> (SET, BinOp (Bit 1) (Reg8 E), [8], 2)
        0xcbcc -> (SET, BinOp (Bit 1) (Reg8 H), [8], 2)
        0xcbcd -> (SET, BinOp (Bit 1) (Reg8 L), [8], 2)
        0xcbce -> (SET, BinOp (Bit 1) (AReg16 HL), [16], 2)
        0xcbcf -> (SET, BinOp (Bit 1) (Reg8 A), [8], 2)
        0xcb0d -> (RRC, UnOp (Reg8 L), [8], 2)
        0xcbd0 -> (SET, BinOp (Bit 2) (Reg8 B), [8], 2)
        0xcbd1 -> (SET, BinOp (Bit 2) (Reg8 C), [8], 2)
        0xcbd2 -> (SET, BinOp (Bit 2) (Reg8 D), [8], 2)
        0xcbd3 -> (SET, BinOp (Bit 2) (Reg8 E), [8], 2)
        0xcbd4 -> (SET, BinOp (Bit 2) (Reg8 H), [8], 2)
        0xcbd5 -> (SET, BinOp (Bit 2) (Reg8 L), [8], 2)
        0xcbd6 -> (SET, BinOp (Bit 2) (AReg16 HL), [16], 2)
        0xcbd7 -> (SET, BinOp (Bit 2) (Reg8 A), [8], 2)
        0xcbd8 -> (SET, BinOp (Bit 3) (Reg8 B), [8], 2)
        0xcbd9 -> (SET, BinOp (Bit 3) (Reg8 C), [8], 2)
        0xcbda -> (SET, BinOp (Bit 3) (Reg8 D), [8], 2)
        0xcbdb -> (SET, BinOp (Bit 3) (Reg8 E), [8], 2)
        0xcbdc -> (SET, BinOp (Bit 3) (Reg8 H), [8], 2)
        0xcbdd -> (SET, BinOp (Bit 3) (Reg8 L), [8], 2)
        0xcbde -> (SET, BinOp (Bit 3) (AReg16 HL), [16], 2)
        0xcbdf -> (SET, BinOp (Bit 3) (Reg8 A), [8], 2)
        0xcb0e -> (RRC, UnOp (AReg16 HL), [16], 2)
        0xcbe0 -> (SET, BinOp (Bit 4) (Reg8 B), [8], 2)
        0xcbe1 -> (SET, BinOp (Bit 4) (Reg8 C), [8], 2)
        0xcbe2 -> (SET, BinOp (Bit 4) (Reg8 D), [8], 2)
        0xcbe3 -> (SET, BinOp (Bit 4) (Reg8 E), [8], 2)
        0xcbe4 -> (SET, BinOp (Bit 4) (Reg8 H), [8], 2)
        0xcbe5 -> (SET, BinOp (Bit 4) (Reg8 L), [8], 2)
        0xcbe6 -> (SET, BinOp (Bit 4) (AReg16 HL), [16], 2)
        0xcbe7 -> (SET, BinOp (Bit 4) (Reg8 A), [8], 2)
        0xcbe8 -> (SET, BinOp (Bit 5) (Reg8 B), [8], 2)
        0xcbe9 -> (SET, BinOp (Bit 5) (Reg8 C), [8], 2)
        0xcbea -> (SET, BinOp (Bit 5) (Reg8 D), [8], 2)
        0xcbeb -> (SET, BinOp (Bit 5) (Reg8 E), [8], 2)
        0xcbec -> (SET, BinOp (Bit 5) (Reg8 H), [8], 2)
        0xcbed -> (SET, BinOp (Bit 5) (Reg8 L), [8], 2)
        0xcbee -> (SET, BinOp (Bit 5) (AReg16 HL), [16], 2)
        0xcbef -> (SET, BinOp (Bit 5) (Reg8 A), [8], 2)
        0xcb0f -> (RRC, UnOp (Reg8 A), [8], 2)
        0xcbf0 -> (SET, BinOp (Bit 6) (Reg8 B), [8], 2)
        0xcbf1 -> (SET, BinOp (Bit 6) (Reg8 C), [8], 2)
        0xcbf2 -> (SET, BinOp (Bit 6) (Reg8 D), [8], 2)
        0xcbf3 -> (SET, BinOp (Bit 6) (Reg8 E), [8], 2)
        0xcbf4 -> (SET, BinOp (Bit 6) (Reg8 H), [8], 2)
        0xcbf5 -> (SET, BinOp (Bit 6) (Reg8 L), [8], 2)
        0xcbf6 -> (SET, BinOp (Bit 6) (AReg16 HL), [16], 2)
        0xcbf7 -> (SET, BinOp (Bit 6) (Reg8 A), [8], 2)
        0xcbf8 -> (SET, BinOp (Bit 7) (Reg8 B), [8], 2)
        0xcbf9 -> (SET, BinOp (Bit 7) (Reg8 C), [8], 2)
        0xcbfa -> (SET, BinOp (Bit 7) (Reg8 D), [8], 2)
        0xcbfb -> (SET, BinOp (Bit 7) (Reg8 E), [8], 2)
        0xcbfc -> (SET, BinOp (Bit 7) (Reg8 H), [8], 2)
        0xcbfd -> (SET, BinOp (Bit 7) (Reg8 L), [8], 2)
        0xcbfe -> (SET, BinOp (Bit 7) (AReg16 HL), [16], 2)
        0xcbff -> (SET, BinOp (Bit 7) (Reg8 A), [8], 2)
