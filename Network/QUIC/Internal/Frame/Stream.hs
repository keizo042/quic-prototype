module Network.QUIC.Internal.Frame.Stream where
import Network.QUIC.Error
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
    get  = StreamFrame frame <$> (sid stream) <*> (BG.getIntN off) <*> (body dlen)

    sid ::  Int -> BG.Get Int
    sid 0 =  return 0
    sid n = BG.getIntN n

    body :: Bool -> BG.Get BSL.ByteString
    body False  = BG.getRemainingLazyByteString
    body True = fromIntegral <$> BG.getInt16 >>= BG.getLazyByteString 

decodeFrameStream _ _ _ = Left Error.InvalidFrameData
