module GameBoy.CPU (

    ) where

import           Protolude
import           GameBoy.Opcodes


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
