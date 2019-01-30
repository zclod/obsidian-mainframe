{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module GameBoy.CPU where

import           Protolude hiding (from)

import           Control.Monad.State

-- import           Control.Lens (use, ix, preview, iso, over, both, Lens', Index, IxValue, Ixed)
import           Control.Lens
import           Control.Lens.Operators
import           Control.Lens.Unsound(lensProduct)
import           Control.Lens.TH (makeLenses)
import           Control.Lens.Wrapped

import           GameBoy.Opcode
import           GameBoy.ALU (FlagRegister(..))
import           GameBoy.Memory


data CPU = CPU
    { _a :: Word8
    , _f :: FlagRegister
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

--utilities
reg8 :: Register8 -> Lens' CPU Word8
reg8 A = a
reg8 B = b
reg8 C = c
reg8 D = d
reg8 E = e
reg8 F = f . _Wrapped'
reg8 H = h
reg8 L = l

splitReg :: Word16 -> (Word8, Word8)
splitReg w16 = (wH, wL)
    where
        wL = fromIntegral $ w16 .&. 255
        wH = fromIntegral $ shiftR (w16 .&. shiftL 255 8) 8

combineReg :: (Word8, Word8) -> Word16
combineReg (wH, wL) = shiftL (fromIntegral wH) 8 .|. fromIntegral wL

-- isomorphism to work with 16 bit data
as16 :: Iso' (Word8, Word8) Word16
as16 = iso combineReg splitReg

mkLens16Bit r1 r2 = (lensProduct (reg8 r1) (reg8 r2)).as16

reg16 :: Register16 -> Lens' CPU Word16
reg16 AF = (lensProduct (reg8 A) (reg8 F)).as16
reg16 BC = (lensProduct (reg8 B) (reg8 C)).as16
reg16 DE = (lensProduct (reg8 D) (reg8 E)).as16
reg16 HL = (lensProduct (reg8 H) (reg8 L)).as16
reg16 SP = sp
reg16 PC = pc
-- lens from scratch
-- decodeRegister16 AF f cpu = f (combineReg (_a cpu) (_f cpu)) <&> \af' -> case splitReg af' of (a', f') -> cpu { _a = a', f = f'  }


-- instructions
--
nop :: Instruction ()
nop = pure ()

ld :: Args -> Instruction ()
ld (BinOp (Reg8 r1) (Reg8 r2)) = do
    s <- get
    (reg8 r1) .= (s^.(reg8 r2))
ld (BinOp (Reg16 r1) (Reg16 r2)) = do
    s <- get
    (reg16 r1) .= (s^.(reg16 r2))
ld _ = nop

{-
:set -XRankNTypes
:set -XGADTs
:set -XFlexibleContexts

:r
import qualified Data.Sequence as Seq
defcpu = CPU 0 (FR 0) 1 0 0 0 0 0 0 0 (Memory $ Seq.fromList [0,1,2,3,4])
execState (ld (BinOp (Reg8 A) (Reg8 B))) defcpu
execState (ld (BinOp (Reg16 AF) (Reg16 BC))) defcpu
set (decodeRegister16 BC) 257 defcpu

-}
