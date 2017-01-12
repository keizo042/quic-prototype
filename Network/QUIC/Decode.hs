module Network.QUIC.Decode
  (
    decodeHeader
  , decodeFrame
  ) where
import qualified Data.Binary          as B
import           Data.ByteString.Lazy (ByteString (..))
import qualified Data.ByteString.Lazy as BSL
import           Data.Int
import qualified Data.Word            as Word

import           Network.QUIC.Error   (QUICResult (..))
import qualified Network.QUIC.Error   as Error
import           Network.QUIC.Frame   (Frame (..), FrameTypes (..),
                                       int2FrameTypes)
import           Network.QUIC.Header  (CommonHeader (..), Flags (..),
                                       Header (..))
import           Network.QUIC.Types   (Nonce, Settings (..))


decodeHeader :: Settings -> ByteString -> Header
decodeHeader s b = undefined

decodeCommonHeader :: Settings -> ByteString -> QUICResult CommonHeader
decodeCommonHeader s bs = undefined

decodeFrame :: Settings -> ByteString -> QUICResult Frame
decodeFrame s bs  =  case ( int2FrameTypes $ fromIntegral $ toInteger $ BSL.head bs  ) of
                              (STREAM f d id) -> decodeFrameStream s  rest
                              (ACK frame l len) -> decodeFrameAck s  rest
                              PADDING -> decodeFramePadding s  rest
                              RST_STREAM ->decodeFrameRstStream s  rest
                              CONNECTION_CLOSE -> decodeFrameConnectionClosed s rest
                              GOAWAY -> decodeFrameGoaway s rest
                              BLOCKED -> decodeFrameBlocked s rest
                              STOP_WAITING -> decodeFrameStopWaiting s rest
                              PING -> decodeFramePing s rest
                              _ ->  Left Error.InvalidPacketHeader
  where
    rest = BSL.tail bs

-- maybe exire
    decodeFrameType :: Settings -> ByteString -> QUICResult FrameTypes
    decodeFrameType s bs = undefined

    decodeFrameStream :: Settings -> ByteString -> QUICResult Frame
    decodeFrameStream = undefined

    decodeFrameAck :: Settings -> ByteString -> QUICResult Frame
    decodeFrameAck = undefined

    decodeFrameStopWaiting :: Settings -> ByteString -> QUICResult Frame
    decodeFrameStopWaiting = undefined

    decodeFrameBlocked :: Settings -> ByteString -> QUICResult Frame
    decodeFrameBlocked = undefined

    decodeFrameCongestionFeedBack :: Settings -> ByteString -> Frame
    decodeFrameCongestionFeedBack = undefined

    decodeFramePadding :: Settings -> ByteString -> QUICResult Frame
    decodeFramePadding = undefined

    decodeFrameRstStream :: Settings -> ByteString -> QUICResult Frame
    decodeFrameRstStream = undefined

    decodeFramePing :: Settings -> ByteString -> QUICResult Frame
    decodeFramePing = undefined

    decodeFrameConnectionClosed :: Settings -> ByteString -> QUICResult Frame
    decodeFrameConnectionClosed = undefined

    decodeFrameGoaway :: Settings -> ByteString -> QUICResult Frame
    decodeFrameGoaway = undefined


decodeNonce :: Settings -> ByteString -> Nonce
decodeNonce = undefined
