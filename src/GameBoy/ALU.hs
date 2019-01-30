{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}

module GameBoy.ALU where

import           Protolude
import           GHC.Generics

import           Data.Bits
import           Data.List ((!!))
import           Foreign.Marshal.Utils (fromBool)

import           Control.Lens.TH (makeLenses)
import           Control.Lens.Wrapped

import           GameBoy.Opcode (Flag(..))


newtype FlagRegister = FR {unReg :: Word8} deriving (Eq, Show, Num, Bits, FiniteBits, Generic)
instance Wrapped FlagRegister

data ALU a = Result {_res :: a, _flags :: FlagRegister} deriving (Eq, Show)
makeLenses ''ALU

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
chain :: (Bool -> Bool -> Bool -> (Bool,Bool)) -> Bool -> [Bool] -> [Bool] -> [(Bool, Bool)]
chain basicOp z w1 w2 = scanr f (False, z) xs
    where
        f (a, b) (res, carry) = basicOp a b carry
        xs = zip w1 w2

unpackBits :: FiniteBits a => a -> [Bool]
unpackBits word = go word (finiteBitSize word)
    where
        go w 0 = []
        go w n = testBit w (n - 1) : go w (n - 1)

testCarryBitNumber :: FiniteBits a => (Bool -> Bool -> Bool -> (Bool,Bool)) -> Int -> Bool -> a -> a -> Bool
testCarryBitNumber op n z w1 w2 = snd $ (chain op z (unpackBits w1) (unpackBits w2)) !! (finiteBitSize w1 - n - 1)

flagNum :: Num a => Flag -> a
flagNum Zf = 7
flagNum Nf = 6
flagNum Hf = 5
flagNum Cf = 4

testFlag :: Flag -> FlagRegister -> Bool
testFlag = flip testBit . flagNum

set :: Flag -> FlagRegister -> FlagRegister
set f = flip setBit $ flagNum f

reset :: Flag -> FlagRegister -> FlagRegister
reset f = flip clearBit $ flagNum f

setIf :: Flag -> Bool -> FlagRegister -> FlagRegister
setIf f p = if p then set f else reset f

----------------------------------------------------------
--ALU operations
--
-- add w1 w2 and carry flag
adc8 :: FlagRegister -> Word8 -> Word8 -> ALU Word8
adc8 flags w1 w2 = Result result flags'
    where
        result = w1 + w2 + (fromBool (testFlag Cf flags))
        flags' = FR 0 & reset Nf
                     & setIf Zf (result == 0)
                     & setIf Cf (add8carry (testFlag Cf flags) w1 w2)
                     & setIf Hf (add8halfcarry (testFlag Cf flags) w1 w2)

        add8carry :: Bool -> Word8 -> Word8 -> Bool
        add8carry = testCarryBitNumber fullAdder 7
        add8halfcarry :: Bool -> Word8 -> Word8 -> Bool
        add8halfcarry = testCarryBitNumber fullAdder 3

--add w1 w2
add8 :: Word8 -> Word8 -> ALU Word8
add8 = adc8 0

-- attenzione non deve toccare il carry
inc8 :: FlagRegister -> Word8 -> ALU Word8
inc8 flags = (\(Result r f) -> Result r (setIf Cf (testFlag Cf flags) f)) . (add8 1)

sbc8 :: FlagRegister -> Word8 -> Word8 -> ALU Word8
sbc8 flags w1 w2 = Result result flags'
    where
        result = w1 - w2 - (fromBool (testFlag Cf flags))
        flags' = 0 & set Nf
                   & setIf Zf (result == 0)
                   & setIf Cf (sub8carry (testFlag Cf flags) w1 w2)
                   & setIf Hf (sub8halfcarry (testFlag Cf flags) w1 w2)

        sub8carry :: Bool -> Word8 -> Word8 -> Bool
        sub8carry = testCarryBitNumber fullSubtractor 7
        sub8halfcarry :: Bool -> Word8 -> Word8 -> Bool
        sub8halfcarry = testCarryBitNumber fullSubtractor 3

sub8 :: Word8 -> Word8 -> ALU Word8
sub8 = sbc8 0

-- attenzione non deve toccare il carry
dec8 :: FlagRegister -> Word8 -> ALU Word8
dec8 flags = (\(Result r f) -> Result r (setIf Cf (testFlag Cf flags) f)) . (flip sub8 1)

and8 :: Word8 -> Word8 -> ALU Word8
and8 w1 w2 = Result result flags
    where
        result = w1 .&. w2
        flags = FR 0 & setIf Zf (result == 0)
                    & set Hf
                    & reset Cf
                    & reset Nf

or8 :: Word8 -> Word8 -> ALU Word8
or8 w1 w2 = Result result flags
    where
        result = w1 .|. w2
        flags = FR 0 & setIf Zf (result == 0)
                    & reset Hf
                    & reset Cf
                    & reset Nf

xor8 :: Word8 -> Word8 -> ALU Word8
xor8 w1 w2 = Result result flags
    where
        result = w1 `xor` w2
        flags = FR 0 & setIf Zf (result == 0)
                    & reset Hf
                    & reset Cf
                    & reset Nf

cp8 :: Word8 -> Word8 -> ALU ()
cp8 w1 w2 = Result () (_flags (sub8 w1 w2))
