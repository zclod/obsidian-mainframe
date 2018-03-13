{-# LANGUAGE TemplateHaskell #-}

module GameBoy.CPU where

import           Protolude
import           Data.List ((!!))
import           Foreign.Marshal.Utils (fromBool)
import           Control.Monad.Cont
import           Control.Arrow ((&&&))
import           Data.Bits

-- import           GameBoy.Opcode (Flag(..))
import           Control.Lens.TH (makeLenses)

data Flag
    = Z -- zero
    | N -- subtract
    | H -- half carry
    | C -- carry

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
-- TODO parametrizzare lo zero
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
testCarryBitNumber op n w1 w2 = snd $ (chain op (unpackBits w1) (unpackBits w2)) !! (finiteBitSize w1 - n - 1)
----------------------------------------------------------
{-

:l GameBoy.CPU
:r
chain fullSubtractor [True, True, False, False] [True, False, True, True]
chain fullAdder [False, False, True, False] [True, False, True, True]

a = 0xe1 :: Word8
b = 0x0f :: Word8
c = 0x1e :: Word8
f = 0 & set C

adc8 f a c
testFlag H 128
-}

flagNum :: Num a => Flag -> a
flagNum Z = 7
flagNum N = 6
flagNum H = 5
flagNum C = 4

testFlag :: Flag -> Word8 -> Bool
testFlag = flip testBit . flagNum

set :: Flag -> Word8 -> Word8
set f = flip setBit $ flagNum f

reset :: Flag -> Word8 -> Word8
reset f = flip clearBit $ flagNum f

setIf :: Flag -> Bool -> Word8 -> Word8
setIf f p = if p then set f else reset f

add8carry :: Word8 -> Word8 -> Bool
add8carry = testCarryBitNumber fullAdder 7

add8halfcarry :: Word8 -> Word8 -> Bool
add8halfcarry = testCarryBitNumber fullAdder 3

add8 :: Word8 -> Word8 -> (Word8, Word8)
add8 w1 w2 = (result, flags)
    where
        result = w1 + w2
        flags = 0 & reset N
                  & setIf Z (result == 0)
                  & setIf C (add8carry w1 w2)
                  & setIf H (add8halfcarry w1 w2)

-- TODO da controllare
adc8 :: Word8 -> Word8 -> Word8 -> (Word8, Word8)
adc8 flags w1 w2 = (result, flags')
    where
        result = w1 + w2 + (fromBool (testFlag C flags))
        flags' = 0 & reset N
                   & setIf Z (result == 0)
                   & setIf C (add8carry w1 w2)
                   & setIf H (add8halfcarry w1 w2)

-- sub8 :: Word8 -> Word8 -> (Word8, Word8)

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
