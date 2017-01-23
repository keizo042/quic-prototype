{-# LANGUAGE CPP #-}
module Network.QUIC.Header
  (
      Header(..)
    , Flags(..)
  ) where
import           Data.Int
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
flags2word8 f =  undefined

word82flags :: Word8 -> Flags
word82flags w = undefined
