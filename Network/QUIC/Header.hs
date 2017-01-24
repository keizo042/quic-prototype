{-# LANGUAGE CPP #-}
module Network.QUIC.Header
  (
      Header(..)
    , Flags(..)
    , word82flags
    , flags2word8
  ) where
import           Data.Int
import Data.Bits
import Data.Word 

data Header = Header { 
   headerFlags                :: Flags
  ,headerConnectionId         :: Maybe Int64
  ,headerQuicVersion          :: Maybe Int32
  ,headerDiversificationNonce :: Integer
  ,headerPacketNumber         :: Integer
  } deriving Show


data Flags = Flags {
    flagsVersion                  :: Bool
  , flagsPublicReset          :: Bool
  , flagsDiversificationNonce :: Bool
  , flagsConnectionId         :: Bool
  , flagsPacketNumberSize     :: Int
  , flagsMultipath            :: Int
} deriving (Show)



flags2word8 :: Flags -> Word8
flags2word8 (Flags v p nonce conn n mp)  =  (a v) .|. (b p) .|. (c nonce) .|. (d conn) .|. (e n) .|. (f mp)
  where
      a :: Bool -> Word8
      a True = undefined
      a False = undefined

      b :: Bool -> Word8
      b True = undefined
      b False = undefined

      c :: Bool -> Word8
      c True = undefined
      c False = undefined

      d :: Bool -> Word8
      d True = undefined
      d False = undefined

      e :: Int -> Word8
      e n = undefined

      f :: Int -> Word8
      f n = undefined

word82flags :: Word8 -> Flags
word82flags w = Flags (a w) (b w) (c w) (d w) (e w) (f w) 
  where
    a = undefined
    b = undefined
    c = undefined
    d = undefined
    e = undefined
    f = undefined
