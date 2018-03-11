{-# LANGUAGE TemplateHaskell #-}

module GameBoy.CPU where

import           Protolude
import           Control.Monad.Cont
import           Data.Bits

import           GameBoy.Opcode (Flag(..))
import           Control.Lens.TH (makeLenses)

data CPU = CPU
    { _a :: Word8
    , _f :: Word8
    , _b :: Word8
    , _c :: Word8
    , _d :: Word8
    , _e :: Word8
    , _h :: Word8
    , _l :: Word8
    , _pc :: Word16
    , _sp :: Word16
    , _ram :: Seq Word8
    }

makeLenses ''CPU

type ArithOp a = a -> a -> a
type Arith8 = ArithOp Word8
type Arith16 = ArithOp Word16

halfAdder :: Bool -> Bool -> (Bool, Bool)
halfAdder a b = (out, carry)
    where
        out = a `xor` b
        carry = a .&. b

halfSubtractor :: Bool -> Bool -> (Bool, Bool)
halfSubtractor a b = (out, borrow)
    where
        out = a `xor` b
        borrow = (complement a) .&. b

full :: (Bool -> Bool -> (Bool, Bool)) -> Bool -> Bool -> Bool -> (Bool, Bool)
full half a b c = (g, h)
    where
        (d, e) = half a b
        (g, f) = half d c
        h = e .|. f

fullAdder = full halfAdder

fullSubtractor = full halfSubtractor

chain :: (Bool -> Bool -> Bool -> (Bool, Bool)) -> [Bool] -> [Bool]
chain adder bits = scanl f 0 bits
    where f c a b = snd $ adder a b c

decodeFlag :: Flag -> Word8
decodeFlag Zf = bit 7
decodeFlag Nf = bit 6
decodeFlag Hf = bit 5
decodeFlag Cf = bit 4

-- decodeRegister A  = a
-- decodeRegister F  = f
-- decodeRegister B  = b
-- decodeRegister C  = c
-- decodeRegister D  = d
-- decodeRegister E  = e
-- decodeRegister H  = h
-- decodeRegister L  = l
-- decodeRegister PC = pc
-- decodeRegister SP = sp
