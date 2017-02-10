module Network.QUIC.Internal.Frame.RstStream where
import Data.Binary.Get
import Data.Binary.Put

import Data.ByteString.Lazy

import qualified Network.QUIC.Error as E
import Network.QUIC.Internal.Util.Binary

data RstStreamFrame = RstStreamFrame  { rstStreamStreamID   :: !Int
                                      , rstStreamByteOffset :: !Int
                                      , rstStreamErrCode    :: !E.ErrorCodes
                                      } deriving Show


encodeFrameRstStream :: RstStreamFrame -> ByteString
encodeFrameRstStream = undefined

decodeFrameRstStream :: ByteString -> E.QUICResult (RstStreamFrame, ByteString)
decodeFrameRstStream bs =  case runGetOrFail (get n) bs of
                                  Right (bs, _, frame) -> Right (frame, bs)
                                  Left _    -> Left E.InvalidRstStreamData
  where
    n = 4
    get :: Int -> Get RstStreamFrame
    get n = RstStreamFrame <$> getStreamID n <*> getInt8byte <*> getErrorCode
