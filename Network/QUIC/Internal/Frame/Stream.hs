module Network.QUIC.Internal.Frame.Stream where
import Network.QUIC.Error
import Data.Word
import Data.Binary.Get
import Data.Binary.Put

data StreamFrame = StreamFrame { streamHasFin :: Bool
                     , streamHasDataField :: Bool
                     , streamHeaderField :: Int
                     , streamStreamId :: Int
                     , streamOffset :: Int
                     , streamLength :: Int
                     , sStreamData :: ByteString
                     } deriving (Show, Eq)

decoeStreamFrame :: ByteString -> QUICResult (Frame, ByteString)
decodeStreamFrame  bs = case runGetOrFail (get f) bs of
                                                             Right (bs, _, frame) -> Right (frame, bs)
                                                             Left _ -> Left Error.InvalidFrameData
  where
    get :: BG.Get StreamFrame
    get  = getWord8 >>= (\ x -> StreamFrame frame <$> (sid $ streamStreamIdIs x) <*> (BG.getIntN $ streamHasOffset x) <*> (body $ streamHasBody x))

    sid ::  Int -> BG.Get Int
    sid 0 =  return 0
    sid n = BG.getIntN n

    body :: Bool -> BG.Get BSL.ByteString
    body False  = BG.getRemainingLazyByteString
    body True = fromIntegral <$> BG.getInt16 >>= BG.getLazyByteString 


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
