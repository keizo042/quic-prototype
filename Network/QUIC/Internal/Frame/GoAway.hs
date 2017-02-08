module Network.QUIC.Internal.Frame.GoAway where
import Network.QUIC.Error
import Data.Word
import Data.Binary.Get
import Data.Binary.Put
import Data.ByteString.Lazy

data GoAwayFrame = GoAwayFrame { goAwayErrorCode        :: ErrorCodes
                               , goAwayLastGoodStreamId :: Int
                               , goAwayReasonPhase      :: ByteString
                               } deriving Show
                              


decodeGoAwayFrame ::  ByteString -> QUICResult (GoAwayFrame, ByteString)
decodeGoAwayFrame bs =  case BG.runGetOrFail get bs of
                          Right (bs, _, frame) -> Right (frame, bs)
                          Left _ -> Left Error.InvalidGoAwayData
  where 
    get :: BG.Get Frame
    get = GoAwayFrame <$> err <*> getInt32 <*> reason
      where
        err :: BG.Get ErrorCodes
        err = Error.int2err <$> getInt32
        reason :: Get ByteString
        reason = fromIntegral <$> getInt16 >>= getLazyByteString


encodeGoAwayFrame :: GoAwayFrame -> ByteString
encodeGoAwayFrame = runPut put
  where
    put = undefined
