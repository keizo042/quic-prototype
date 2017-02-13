module Network.QUIC.Internal.Frame.ConnectionClose where
import Data.Binary.Get
import Data.Binary.Put

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

import qualified Network.QUIC.Error as E
import Network.QUIC.Internal.Util.Binary


data ConnectionCloseFrame =  ConnectionCloseFrame  { connectionClosedErrorCode :: !E.ErrorCodes
                                                   , connectionClosedreasonPhase :: !BS.ByteString
                                                   } deriving Show


encodeConnectionCloseFrame :: ConnectionCloseFrame -> BSL.ByteString
encodeConnectionCloseFrame (ConnectionCloseFrame errCode reasonPhase) = runPut put
  where
    put :: Put
    put = putErrorCode errCode >>
          putReasonPhase reasonPhase

decodeConnectionClosedFrame ::  BSL.ByteString -> E.QUICResult (ConnectionCloseFrame, BSL.ByteString)
decodeConnectionClosedFrame bs =  case runGetOrFail get bs of
                                         Right (bs, _, frame) -> Right (frame, bs)
                                         Left _ -> Left E.InvalidConnectionCloseData
  where
    get :: Get ConnectionCloseFrame
    get = ConnectionCloseFrame <$> getErrorCode <*> reason
      where
        reason :: Get BS.ByteString
        reason = BSL.toStrict <$> (fromIntegral <$> getInt2byte >>=  getLazyByteString)

