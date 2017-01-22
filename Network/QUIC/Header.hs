{-# LANGUAGE CPP #-}
module Network.QUIC.Header
  (
      Header(..)
    , CommonHeader(..)
    , Flags(..)
    , ByteSize(..)
    , byteSize2int
    , int2byteSize
  ) where
import           Data.Int
import Data.Word 

data Header = Header{ common :: CommonHeader
                    } deriving Show



data CommonHeader = CommonHeader{
   headerFlags                :: Flags
  ,headerConnectionId         :: Maybe Int64
  ,headerQuicVersion          :: Maybe Int32
  ,headerDiversificationNonce :: Integer
  ,headerPacketNumber         :: Integer
  } deriving Show

data ByteSize = Byte6
                | Byte4
                | Byte2
                | Byte1
                deriving (Show, Eq)

byteSize2int :: ByteSize -> Int
byteSize2int Byte6 = 0x3
byteSize2int Byte4 = 0x2
byteSize2int Byte2 = 0x1
byteSize2int Byte1 = 0x0

int2byteSize :: Int -> ByteSize
int2byteSize 0x3 = Byte6
int2byteSize 0x2 = Byte1
int2byteSize 0x1 = Byte1
int2byteSize 0x0 = Byte1

data Flags = Flags {
    flagsVersion                  :: Bool
  , flagsPublicReset          :: Bool
  , flagsDiversificationNonce :: Bool
  , flagsConnectionId         :: Bool
  , flagsPacketNumberSize     :: ByteSize
  , flagsMultipath            :: Int
} deriving (Show)



flags2word8 :: Flags -> Word8
flags2word8 f =  undefined

