module Network.QUIC.Internal.Frame.Blocked 
  (
  ) where
import Data.Binary.Get
import Data.Binary.Put
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

import qualified Network.QUIC.Error as E
import Network.QUIC.Types
import Network.QUIC.Internal.Util.Binary


data BlockedFrame = BlockedFrame { blockedStreamID :: !StreamID } deriving (Show, Eq)

decodeBlockedFrame  :: BSL.ByteString -> ByteSize -> E.QUICResult (BlockedFrame, BSL.ByteString)
decodeBlockedFrame bs n = case runGetOrFail get bs of
                               Right (bs, _, frame) -> Right (frame, bs)
                               Left _               -> Left E.InvalidBlockedData
  where
    get :: Get BlockedFrame
    get = BlockedFrame <$> getStreamID n

encodeBlockedFrame :: BlockedFrame -> BSL.ByteString
encodeBlockedFrame (BlockedFrame streamID) = runPut put
  where
    put =  putStreamID (countStreamIDbyteSize streamID) streamID
