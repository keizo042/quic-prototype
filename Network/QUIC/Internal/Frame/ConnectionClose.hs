module Network.QUIC.Internal.Frame.ConnectionClose where
import Data.Binary.Get
import Data.Binary.Put

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

import qualified Network.QUIC.Error as E
import Network.QUIC.Internal.Util.Binary


data ConnectionCloseFrame =  ConnectionCloseFrame  { connectionClosedErrorCode :: !E.ErrorCodes
                                                   , connectionClosedreasonPahse :: !BS.ByteString
                                                   } deriving Show


encodeConnectionCloseFrame :: ConnectionCloseFrame -> BSL.ByteString
encodeConnectionCloseFrame = undefined

decodeConnectionClosedFrame ::  BSL.ByteString -> E.QUICResult (ConnectionCloseFrame, BSL.ByteString)
decodeConnectionClosedFrame bs =  case runGetOrFail get bs of
                                         Right (bs, _, frame) -> Right (frame, bs)
                                         Left _ -> Left E.InvalidConnectionCloseData
  where
    get :: Get ConnectionCloseFrame
    get = ConnectionCloseFrame <$> err <*> reason
      where
        err :: Get E.ErrorCodes
        err = E.int2err <$> getInt4byte
        reason :: Get BS.ByteString
        reason = BSL.toStrict <$> (fromIntegral <$> getInt2byte >>=  getLazyByteString)




