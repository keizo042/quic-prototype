module Network.QUIC.Internal.Frame.Stream where
import Network.QUIC.Error
import Data.Binary.Get
import Data.Binary.Put


data ConnectionCloseFrame =  ConnectionCloseFrame  { conClosedErrorCode :: ErrorCodes
                                                   , conClosedreasonPahse         :: ByteString
                                                   } deriving Show


encodeConnectionCloseFrame :: ConnectionCloseFrame -> ByteString
encodeConnectionCloseFrame = undefined

decodeConnectionClosedFrame ::  ByteString -> QUICResult (Frame, ByteString)
decodeConnectionClosedFrame bs =  case BG.runGetOrFail get bs of
                                         Right (bs, _, frame) -> Right (frame, bs)
                                         Left _ -> Left Error.InvalidConnectionCloseData
  where
    get :: BG.Get Frame
    get = ConnectionClose <$> err <*> reason
      where
        err :: BG.Get Error.ErrorCodes
        err = Error.int2err <$> BG.getInt32
        reason :: BG.Get ByteString
        reason = fromIntegral <$> BG.getInt16 >>= BG.getLazyByteString



