{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module GameBoy.CPU where

import           Protolude hiding ((<&>))

import           Control.Monad.State

import           Control.Lens (use, ix, preview, Lens', Index, IxValue, Ixed)
import           Control.Lens.Operators
import           Control.Lens.TH (makeLenses)
import           Control.Lens.Wrapped

import           GameBoy.Opcode
import           GameBoy.ALU
import           GameBoy.Memory


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

{-
:set -XRankNTypes
:set -XGADTs
:set -XFlexibleContexts

:r

defcpu = CPU 0 (FR 0) 1 0 0 0 0 0 0 0 (Memory $ Seq.fromList [0,1,2,3,4])
-}
