{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}

module GameBoy.Memory where

import           Protolude
import           GHC.Generics

import qualified Data.Sequence as Seq
import           Control.Lens (Index, IxValue, Ixed(..))

newtype Address = Adr {unAddress :: Word16} deriving (Eq, Show, Num, Bits, FiniteBits, Generic)

newtype Memory = Memory {unMemory :: Seq Word8} deriving (Eq, Show)
type instance Index Memory = Word16
type instance IxValue Memory = Word8

instance Ixed Memory where
  ix i f m
    | 0 <= i && fromIntegral i < Seq.length (unMemory m) =
        f (Seq.index (unMemory m) (fromIntegral i)) <&> \a -> Memory $ Seq.update (fromIntegral i) a (unMemory m)
    | otherwise                  = pure m
  {-# INLINE ix #-}
