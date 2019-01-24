{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module GameBoy.CPU where

import           Protolude hiding ((<&>))
import           GHC.Generics

import qualified Data.Sequence as Seq
import           Data.Bits
import           Data.List ((!!))
import           Foreign.Marshal.Utils (fromBool)

import           Control.Monad.Cont
import           Control.Monad.State
import           Control.Arrow ((&&&))

import           Control.Lens (use, ix, preview, Lens', Index, IxValue, Ixed)
import           Control.Lens.Operators
import           Control.Lens.TH (makeLenses)
import           Control.Lens.Wrapped

import           GameBoy.Opcode --(Flag(..))

newtype FlagRegistry = FR {unReg :: Word8} deriving (Eq, Show, Num, Bits, FiniteBits, Generic)
instance Wrapped FlagRegistry

newtype Address = Adr {unAddress :: Word16} deriving (Eq, Show, Num, Bits, FiniteBits, Generic)

newtype Memory = Memory {unMemory :: Seq Word8} deriving (Eq, Show)

type instance Index Memory = Word16

type instance IxValue Memory = Word8

instance Ixed Memory where
  ix i f m
    | 0 <= i && w16tInt i < Seq.length (unMemory m) =
        f (Seq.index (unMemory m) (w16tInt i)) <&> \a -> Memory $ Seq.update (w16tInt i) a (unMemory m)
    | otherwise                  = pure m

        where
            w16tInt :: Word16 -> Int
            -- w16tInt =  fromInteger . toInteger
            w16tInt =  fromIntegral
  {-# INLINE ix #-}


data ALU a = Result {_res :: a, _flags :: FlagRegistry} deriving (Eq, Show)
makeLenses ''ALU

data CPU = CPU
    { _a :: Word8
    , _f :: FlagRegistry
    , _b :: Word8
    , _c :: Word8
    , _d :: Word8
    , _e :: Word8
    , _h :: Word8
    , _l :: Word8
    , _pc :: Word16
    , _sp :: Word16
    , _ram :: Memory
    } deriving (Show)
makeLenses ''CPU

type Instruction a = State CPU a

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

testFlag :: Flag -> FlagRegistry -> Bool
testFlag = flip testBit . flagNum

set :: Flag -> FlagRegistry -> FlagRegistry
set f = flip setBit $ flagNum f

reset :: Flag -> FlagRegistry -> FlagRegistry
reset f = flip clearBit $ flagNum f

setIf :: Flag -> Bool -> FlagRegistry -> FlagRegistry
setIf f p = if p then set f else reset f

----------------------------------------------------------
--ALU operations
--
-- add w1 w2 and carry flag
adc8 :: FlagRegistry -> Word8 -> Word8 -> ALU Word8
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
inc8 :: FlagRegistry -> Word8 -> ALU Word8
inc8 flags = (\(Result r f) -> Result r (setIf Cf (testFlag Cf flags) f)) . (add8 1)

sbc8 :: FlagRegistry -> Word8 -> Word8 -> ALU Word8
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
dec8 :: FlagRegistry -> Word8 -> ALU Word8
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

----------------------------------------------------------------------------

--utilities
decodeRegister8 :: Register8 -> Lens' CPU Word8
decodeRegister8 A = a
decodeRegister8 B = b
decodeRegister8 C = c
decodeRegister8 D = d
decodeRegister8 E = e
decodeRegister8 F = f . _Wrapped'
decodeRegister8 H = h
decodeRegister8 L = l

decodeRegister16 :: Register16 -> Lens' CPU Word16
decodeRegister16 = undefined

-- instructions
--
nop :: Instruction ()
nop = pure ()

ld :: Operand -> Operand -> Instruction ()
ld _ _ = nop

-- memo l = ram . ix i
--     where
--         i = l

-- ld' dst l = execState $ do
--     x <- use l
--     dst .= ram . ix (fromInteger x)
{-
:set -XRankNTypes
:set -XGADTs
:set -XFlexibleContexts

:l GameBoy.CPU
:m + GameBoy.CPU
:m + Control.Lens
:m + Data.Sequence
:m - Protolude
import Protolude (const)
:r

:t ld
:t memo
defcpu = CPU 0 (FR 0) 1 0 0 0 0 0 0 0 (Memory $ Seq.fromList [0,1,2,3,4])
set (decodeRegister C) $ 1 $ defcpu
set c 2 defcpu
defcpu ^? ram . ix 2
ld a (ram . ix 4) defcpu
ld b a defcpu
ld' a b defcpu


defcpu ^? to (const 5)
("hello","world")^.to snd
x (to $ const 5) defcpu
preview a defcpu
:t preview a
defcpu ^? a
chain fullSubtractor [True, True, False, False] [True, False, True, True]
chain fullAdder [False, False, True, False] [True, False, True, True]

a = 0x3b :: Word8
b = 0x4f :: Word8
c = 0x2a :: Word8
d = 0x3a :: Word8
f = FR 0 & set C

sbc8 f a b
inc8 0 0xff
testFlag Z $ FR 112
testFlag C $ FR 112
testFlag N $ FR 112
testFlag H $ FR 112
-}
