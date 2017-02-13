module Network.QUIC.Internal.Frame.GoAway 
  (
  )where
import Network.QUIC.Error
import Network.QUIC.Types
import Network.QUIC.Internal.Util.Binary

import Data.Word
import Data.Binary.Get
import Data.Binary.Put
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS

data GoAwayFrame = GoAwayFrame { goAwayErrorCode        :: !ErrorCodes
                               , goAwayLastGoodStreamId :: !StreamID
                               , goAwayReasonPhase      :: !BS.ByteString
                               } deriving Show
                              


decodeGoAwayFrame ::  BSL.ByteString -> ByteSize -> QUICResult (GoAwayFrame, BSL.ByteString)
decodeGoAwayFrame bs n =  case runGetOrFail get bs of
                          Right (bs, _, frame) -> Right (frame, bs)
                          Left _ -> Left InvalidGoAwayData
  where 
    get :: Get GoAwayFrame
    get = GoAwayFrame <$> getErrorCode <*> getStreamID n  <*> getReasonPhase

encodeGoAwayFrame :: GoAwayFrame -> BSL.ByteString
encodeGoAwayFrame (GoAwayFrame errCode streamID reasonPhase) = runPut put
  where
    put = do 
      putErrorCode errCode 
      putStreamID (countStreamIDbyteSize streamID) streamID
      putReasonPhase reasonPhase
