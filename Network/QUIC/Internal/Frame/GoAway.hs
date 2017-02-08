module Network.QUIC.Internal.Frame.GoAway where
import Network.QUIC.Error
import Data.Binary.Get
import Data.Binary.Put

data GoawayFrame = GoawayFrame { goAwayErrorCode        :: ErrorCodes
                               , goAwayLastGoodStreamId :: Int
                               , goAwayReasonPhase      :: ByteString
                               } deriving Show
                              


decodeFrameGoaway :: Settings -> ByteString -> QUICResult (Frame, ByteString)
decodeFrameGoaway s bs =  case BG.runGetOrFail get bs of
                          Right (bs, _, frame) -> Right (frame, bs)
                          Left _ -> Left Error.InvalidGoAwayData
  where 
    get :: BG.Get Frame
    get = Goaway <$> err <*> BG.getInt32 <*> reason
      where
        err :: BG.Get Error.ErrorCodes
        err = Error.int2err <$> BG.getInt32
        reason :: BG.Get ByteString
        reason = fromIntegral <$> BG.getInt16 >>= BG.getLazyByteString

