module Network.QUIC.Encode
  (
    encodeHeader
  , encodeFrame
  ) where

import Data.ByteString      (ByteString (..))
import qualified Data.ByteString.Lazy as BSL
import           Network.QUIC.Error   (QUICResult (..))
import qualified Network.QUIC.Error   as Error
import Network.QUIC.Frame   (Frame(..), FrameType(..))
import qualified Network.QUIC.Frame   as Frame
import Network.QUIC.Header  (Header (..))
import Network.QUIC.Types   (Settings(..))

encodeHeader :: Settings ->  Header -> ByteString
encodeHeader = undefined

encodeFrame :: Settings -> Frame -> QUICResult ByteString
encodeFrame s frame = case frame of
                           Stream{}                 -> encodeFrameStream s frame
                           Ack{}                    -> encodeFrameAck s frame
                           RstStream{}              -> encodeFrameRstStream s frame
                           WindowUpdate{}           -> encodeFrameWindowUpdate s frame
                           Blocked{}                -> encodeFrameBlocked s frame
                           Padding                  -> encodeFramePadding  s  frame
                           Ping                     -> encodeFramePing s  frame
                           ConnectionClose{}    -> encodeFrameConnectionClose s frame
                           Goaway{}           -> encodeFrameGoAway  s  frame
                           StopWaiting{}          -> encodeFrameStopWaiting s frame
                           FrameError e -> Left e
  where
    encodeFrameStream :: Settings -> Frame -> QUICResult ByteString
    encodeFrameStream = undefined

    encodeFrameAck :: Settings -> Frame -> QUICResult ByteString
    encodeFrameAck = undefined

    encodeFramePadding :: Settings -> Frame -> QUICResult ByteString
    encodeFramePadding = undefined

    encodeFrameRstStream :: Settings -> Frame -> QUICResult ByteString
    encodeFrameRstStream = undefined

    encodeFrameConnectionClose :: Settings -> Frame -> QUICResult ByteString
    encodeFrameConnectionClose = undefined

    encodeFrameGoAway :: Settings -> Frame -> QUICResult ByteString
    encodeFrameGoAway = undefined

    encodeFrameWindowUpdate :: Settings -> Frame -> QUICResult ByteString
    encodeFrameWindowUpdate = undefined

    encodeFrameBlocked :: Settings -> Frame -> QUICResult ByteString
    encodeFrameBlocked = undefined

    encodeFrameStopWaiting :: Settings -> Frame -> QUICResult ByteString
    encodeFrameStopWaiting = undefined

    encodeFramePing :: Settings -> Frame -> QUICResult ByteString
    encodeFramePing = undefined

