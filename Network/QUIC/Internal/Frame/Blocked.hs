module Network.QUIC.Internal.Frame.Blocked where
import Data.Binary.Get
import Data.Binary.Put
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

import qualified Network.QUIC.Error as E
import Network.QUIC.Types
import Network.QUIC.Internal.Util.Binary


data BlockedFrame = BlockedFrame { blockedStreamID :: StreamID } deriving Show

decodeBlockedFrame  :: BSL.ByteString -> E.QUICResult (BlockedFrame, BSL.ByteString)
decodeBlockedFrame bs = case runGetOrFail get bs of
                               Right (bs, _, frame) -> Right (frame, bs)
                               Left _               -> Left E.InvalidFrameData
  where
    get :: Get BlockedFrame
    get = BlockedFrame <$> getInt4byte

encodeBlockedFrame :: BlockedFrame -> BSL.ByteString
encodeBlockedFrame (BlockedFrame streamID) = runPut $ putStreamID len streamID
  where
    len = countStreamIDbyteSize streamID
