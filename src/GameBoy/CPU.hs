{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module GameBoy.CPU where

import           Protolude
import           Data.Sequence (fromList)
import           Control.Lens (ix, preview, Lens')
import           Control.Lens.Operators
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

newtype FlagRegistry = F {unReg :: Word8} deriving (Eq, Show, Num, Bits, FiniteBits)
newtype Address = A {unAddress :: Word16} deriving (Eq, Show, Num, Bits, FiniteBits)

data Result a = Result {_res :: a, _flags :: FlagRegistry} deriving (Eq, Show)
makeLenses ''Result

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
    , _ram :: Seq Word8
    } deriving (Show)
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
flagNum Z = 7
flagNum N = 6
flagNum H = 5
flagNum C = 4

testFlag :: Flag -> FlagRegistry -> Bool
testFlag = flip testBit . flagNum

set :: Flag -> FlagRegistry -> FlagRegistry
set f = flip setBit $ flagNum f

reset :: Flag -> FlagRegistry -> FlagRegistry
reset f = flip clearBit $ flagNum f

setIf :: Flag -> Bool -> FlagRegistry -> FlagRegistry
setIf f p = if p then set f else reset f

----------------------------------------------------------
--istruction implementation
--
-- add w1 w2 and carry flag
adc8 :: FlagRegistry -> Word8 -> Word8 -> Result Word8
adc8 flags w1 w2 = Result result flags'
    where
        result = w1 + w2 + (fromBool (testFlag C flags))
        flags' = F 0 & reset N
                     & setIf Z (result == 0)
                     & setIf C (add8carry (testFlag C flags) w1 w2)
                     & setIf H (add8halfcarry (testFlag C flags) w1 w2)

        add8carry :: Bool -> Word8 -> Word8 -> Bool
        add8carry = testCarryBitNumber fullAdder 7
        add8halfcarry :: Bool -> Word8 -> Word8 -> Bool
        add8halfcarry = testCarryBitNumber fullAdder 3

--add w1 w2
add8 :: Word8 -> Word8 -> Result Word8
add8 = adc8 0

-- attenzione non deve toccare il carry
inc8 :: FlagRegistry -> Word8 -> Result Word8
inc8 flags = (\(Result r f) -> Result r (setIf C (testFlag C flags) f)) . (add8 1)

sbc8 :: FlagRegistry -> Word8 -> Word8 -> Result Word8
sbc8 flags w1 w2 = Result result flags'
    where
        result = w1 - w2 - (fromBool (testFlag C flags))
        flags' = 0 & set N
                   & setIf Z (result == 0)
                   & setIf C (sub8carry (testFlag C flags) w1 w2)
                   & setIf H (sub8halfcarry (testFlag C flags) w1 w2)

        sub8carry :: Bool -> Word8 -> Word8 -> Bool
        sub8carry = testCarryBitNumber fullSubtractor 7
        sub8halfcarry :: Bool -> Word8 -> Word8 -> Bool
        sub8halfcarry = testCarryBitNumber fullSubtractor 3

sub8 :: Word8 -> Word8 -> Result Word8
sub8 = sbc8 0

-- attenzione non deve toccare il carry
dec8 :: FlagRegistry -> Word8 -> Result Word8
dec8 flags = (\(Result r f) -> Result r (setIf C (testFlag C flags) f)) . (flip sub8 1)

and8 :: Word8 -> Word8 -> Result Word8
and8 w1 w2 = Result result flags
    where
        result = w1 .&. w2
        flags = F 0 & setIf Z (result == 0)
                    & set H
                    & reset C
                    & reset N

or8 :: Word8 -> Word8 -> Result Word8
or8 w1 w2 = Result result flags
    where
        result = w1 .|. w2
        flags = F 0 & setIf Z (result == 0)
                    & reset H
                    & reset C
                    & reset N

xor8 :: Word8 -> Word8 -> Result Word8
xor8 w1 w2 = Result result flags
    where
        result = w1 `xor` w2
        flags = F 0 & setIf Z (result == 0)
                    & reset H
                    & reset C
                    & reset N

cp8 :: Word8 -> Word8 -> Result ()
cp8 w1 w2 = Result () (_flags (sub8 w1 w2))

-- load instruction, can fail if src or dst lens fails
ld dst src cpu = cpu & dst %%~ const (cpu^?src)

memo l = 
{-
:set -XRankNTypes
:l GameBoy.CPU
:r
:t ld
defcpu = CPU 0 (F 0) 1 0 0 0 0 0 0 0 (fromList [0,1,2,3,4])
ld a (ram . ix 4) defcpu
ld a (ram . ix . a) defcpu
ld b a defcpu

defcpu ^? ram . (ix 2)
preview a defcpu
:t preview a
defcpu ^? a

chain fullSubtractor [True, True, False, False] [True, False, True, True]
chain fullAdder [False, False, True, False] [True, False, True, True]

a = 0x3b :: Word8
b = 0x4f :: Word8
c = 0x2a :: Word8
d = 0x3a :: Word8
f = F 0 & set C

sbc8 f a b
inc8 0 0xff
testFlag Z $ F 112
testFlag C $ F 112
testFlag N $ F 112
testFlag H $ F 112
-}
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
