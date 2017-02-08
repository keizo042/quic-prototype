module Network.QUIC.Internal.Frame.Stream where
import Network.QUIC.Error
import Data.Word
import Data.Bits
import Data.ByteString.Lazy
import Data.Binary.Get
import Data.Binary.Put
import Network.QUIC.Internal

data StreamFrame = StreamFrame { streamHasFin :: Bool
                     , streamHasDataField :: Bool
                     , streamHeaderField :: Int
                     , streamStreamId :: Int
                     , streamOffset :: Int
                     , streamLength :: Int
                     , sStreamData :: ByteString
                     } deriving (Show, Eq)

decodeStreamFrame :: ByteString -> QUICResult (StreamFrame, ByteString)
decodeStreamFrame  bs = case runGetOrFail (get f) bs of
                                                             Right (bs, _, frame) -> Right (frame, bs)
                                                             Left _ -> Left InvalidFrameData
  where
    get :: Get StreamFrame
    get  = getWord8 >>= (\ x -> StreamFrame frame <$> (sid $ streamStreamIdIs x) <*> (getIntN $ streamHasOffset x) <*> (body $ streamHasBody x))

    sid ::  Int -> Get Int
    sid 0 =  return 0
    sid n = getIntN n

    body :: Bool -> Get ByteString
    body False  = getRemainingLazyByteString
    body True = fromIntegral <$> getInt16 >>= getLazyByteString 


hasStreamFin :: Word8 -> Bool
hasStreamFin i = i .&. 0x40  == 0x40

streamLengthIs :: Word8 -> bool
streamLengthIs i = i .&. 0x20  == 0x20

streamOffsetIs :: Word8 -> Int
streamOffsetIs i = case i .&. 0x1c of
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
