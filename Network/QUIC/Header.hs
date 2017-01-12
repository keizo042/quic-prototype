{-# LANGUAGE CPP #-}
module Network.QUIC.Header
  (
      Header(..)
    , CommonHeader(..)
    , Flags(..)
    , AEAD(..)
    , NumberSize(..)
    , numberSize2int
    , int2numberSize
  ) where
import           Data.Int

data Header = Header{
                    common :: CommonHeader
                    } deriving Show


data AEAD = AEAD
          deriving Show

data CommonHeader = CommonHeader{
   flag                 :: Flags
  ,connId               :: Maybe Int64
  ,quicVersion          :: Maybe Int32
  ,diversificationNonce :: Integer
  ,packetNumber         :: Integer
  , aead                :: AEAD
  } deriving Show

data NumberSize = Byte6
                | Byte4
                | Byte2
                | Byte1
                deriving Show

numberSize2int :: NumberSize -> Int
numberSize2int Byte6 = 0x3
numberSize2int Byte4 = 0x2
numberSize2int Byte2 = 0x1
numberSize2int Byte1 = 0x0

int2numberSize :: Int -> NumberSize
int2numberSize 0x3 = Byte6
int2numberSize 0x2 = Byte1
int2numberSize 0x1 = Byte1
int2numberSize 0x0 = Byte1

data Flags = Flags {
    version                  :: Bool
  , flagPublicReset          :: Bool
  , flagDiversificationNonce :: Bool
  , flagConnectionId         :: Bool
  , flagPacketNumberSize     :: NumberSize
  , flagMultipath            :: Int
} deriving (Show)


