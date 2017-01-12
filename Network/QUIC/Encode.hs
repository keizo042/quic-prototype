module Network.QUIC.Encode
  (
    encodeHeader
  , encodeFrame
  ) where
import Data.ByteString      (ByteString (..))
import qualified Data.ByteString.Lazy as BSL
import           Network.QUIC.Error   (QUICResult (..))
import qualified Network.QUIC.Error   as Error
import Network.QUIC.Frame   (Frame(..), FrameTypes(..))
import qualified Network.QUIC.Frame   as Frame
import Network.QUIC.Header  (CommonHeader (..), Header (..))
import Network.QUIC.Types   (Settings(..))

encodeHeader :: Settings ->  Header -> ByteString
encodeHeader = undefined

encodeFrame :: Settings -> Frame -> Maybe ByteString
encodeFrame s frame = case frame of
                           _ ->  Nothing
  where
    encodeFrameStream :: Settings -> Frame -> ByteString
    encodeFrameStream = undefined

    encodeFrameAck :: Settings -> Frame -> ByteString
    encodeFrameAck = undefined

    encodeFramePadding :: Settings -> Frame -> ByteString
    encodeFramePadding = undefined

    encodeFrameRstStream :: Settings -> Frame -> ByteString
    encodeFrameRstStream = undefined

    encodeFrameConnClose :: Settings -> Frame -> ByteString
    encodeFrameConnClose = undefined

    encodeFrameGoAway :: Settings -> Frame -> ByteString
    encodeFrameGoAway = undefined

    encodeFrameWindowUpdate :: Settings -> Frame -> ByteString
    encodeFrameWindowUpdate = undefined

    encodeFrameBlocked :: Settings -> Frame -> ByteString
    encodeFrameBlocked = undefined

    encodeFrameStopWaiting :: Settings -> Frame -> ByteString
    encodeFrameStopWaiting = undefined

    encodeFramePing :: Settings -> Frame -> ByteString
    encodeFramePing = undefined

encodeCommonHeader :: Settings -> CommonHeader -> ByteString
encodeCommonHeader = undefined
