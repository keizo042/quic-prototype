module Network.QUIC.Internal.Frame.RstStream where
import Network.QUIC.Error
import Data.Binary.Get
import Data.Binary.Put


data RstStreamFrame = RstStreamFrame  { rstStreamStreamId   :: Int
                                      , rstStreamByteOffset :: Int
                                      , rstStreamErrCode    :: ErrorCodes
                                      } deriving Show


encodeFrameRstStream :: RstStreamFrame -> ByteString
encodeFrameRstStream = undefined

decodeFrameRstStream :: Settings -> ByteString -> QUICResult (Frame, ByteString)
decodeFrameRstStream s bs =  case BG.runGetOrFail get bs of
                                  Right (bs, _, frame) -> Right (frame, bs)
                                  Left _    -> Left Error.InvalidRstStreamData
  where
    get :: BG.Get Frame
    get = RstStream <$> BG.getInt32 <*> BG.getInt64 <*> (Error.int2err <$> BG.getInt32)
