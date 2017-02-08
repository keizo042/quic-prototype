module Network.QUIC.Internal.Frame.Stream where
import Data.ByteString.Lazy
import qualified Data.Binary.Get as BG (getLazyByteString, getRemainingLazyByteString,runGetOrFail, Get)
import           Data.ByteString.Lazy (ByteString (..))
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSC8
import           Data.Int
import qualified Data.Word            as Word
import qualified Network.QUIC.Error   as Error

class Frame a where
    toFrame :: ByteString -> a
    fromFrame :: a - > ByteString

isStreamFrame :: Word8 -> Bool
isStreamFrame i = i .&. 0x80 == 0x80

isAckFrame :: Word8 -> Bool
isAckFrame i = i .&. 0x40 == 0x40


streamHasFin :: Word8 -> Bool
streamHasFin i = i .&. 0x40  == 0x40

streamLengthIs :: Word8 -> bool
streamLengthIs i = i .&. 0x20  == 0x20

streamOffsetIs :: Word8 -> Int
streamOffsetIs i = offset
  where
          offset = case i .&. 0x1c of
                        0x00 -> 0
                        0x04 -> 8
                        0x08 -> 16
                        0x0c -> 24
                        0x10 -> 32
                        0x14 -> 48
                        0x18 -> 56
                        0x1c -> 64

streamStreamIdIs :: Word8 -> Int
streamStreamIdIs i = case i .&. 0x03 of
                       0x00 -> 8
                       0x01 -> 16
                       0x02 -> 32
                       0x03 -> 64
                       _    -> 0
