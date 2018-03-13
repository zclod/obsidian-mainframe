{-# LANGUAGE TemplateHaskell #-}

module GameBoy.CPU where

import           Protolude
import           Data.List ((!!))
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

-------------------------------------------------------
-- carry flags utils
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

-- chain a series of fullAdder or fullSubtractor
chain :: (Bool -> Bool -> Bool -> (Bool,Bool)) -> [Bool] -> [Bool] -> [(Bool, Bool)]
chain basicOp w1 w2 = scanr f z xs
    where
        f (a, b) (res, carry) = basicOp a b carry
        z = (False, False)
        xs = zip w1 w2

unpackBits :: FiniteBits a => a -> [Bool]
unpackBits word = go word (finiteBitSize word)
    where
        go w 0 = []
        go w n = testBit w (n - 1) : go w (n - 1)

testCarryBitNumber :: FiniteBits a => (Bool -> Bool -> Bool -> (Bool,Bool)) -> Int -> a -> a -> Bool
testCarryBitNumber op n w1 w2 = snd $ (chain op (unpackBits w1) (unpackBits w2)) !! (finiteBitSize w1 - n)
----------------------------------------------------------
{-

:l GameBoy.CPU
chain fullSubtractor [True, True, False, False] [True, False, True, True]
chain fullAdder [False, False, True, False] [True, False, True, True]

a = 15 :: Word8
b = 1 :: Word8
chain fullAdder (unpackBits a) (unpackBits b)
addcarry = testCarryBitNumber fullAdder
addcarry 4 a b

-}

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
