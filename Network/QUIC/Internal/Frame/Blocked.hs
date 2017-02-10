module Network.QUIC.Internal.Frame.Blocked where
import Data.Binary.Get
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

import qualified Network.QUIC.Error as E
import Network.QUIC.Internal.Util.Binary


data BlockedFrame = BlockedFrame { blockedStreamID :: Int } deriving Show

decodeBlockedFrame  :: ByteString -> E.QUICResult (BlockedFrame, ByteString)
decodeBlockedFrame s bs = case runGetOrFail get bs of
                               Right (bs, _, frame) -> Right (frame, bs)
                               Left _               -> Left E.InvalidFrameData
  where
    get :: BG.Get BlockedFrame
    get = BlockedFrame <$> getInt4byte

encodeBlockedFrame :: BlockedFrame -> ByteString
encodeBlockedFrame = undefined
