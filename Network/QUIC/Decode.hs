module Network.QUIC.Decode
  (
    decodeHeader
  , decodeFrame
  ) where
import qualified Data.Binary          as B
import qualified Data.Binary.Get      as BG
import qualified Data.Binary.Put      as BP
import           Data.ByteString.Lazy (ByteString (..))
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSC8
import           Data.Int
import qualified Data.Word            as Word

import           Network.QUIC.Error   (QUICResult (..))
import qualified Network.QUIC.Error   as Error
import qualified Network.QUIC.Frame as F
import           Network.QUIC.Frame   (Frame (..), FrameType (..),
                                       word82FrameType)
import           Network.QUIC.Header  (CommonHeader (..), Flags (..),
                                       Header (..))
import           Network.QUIC.Types   (Nonce, Settings (..))

decodeHeader :: Settings -> ByteString -> QUICResult (Header, ByteString)
decodeHeader s b = undefined

decodeCommonHeader :: Settings -> ByteString -> QUICResult (CommonHeader, ByteString)
decodeCommonHeader s bs = undefined

decodeFrame :: Settings -> ByteString -> QUICResult (Frame, ByteString)
decodeFrame s bytes  = case (word82FrameType b) of
                        frametype@(STREAM{})          -> decodeFrameStream            s   frametype   bs
                        frametype@(ACK{})             -> decodeFrameAck               s   frametype   bs
                        STOP_WAITING                  -> decodeFrameStopWaiting       s   bs
                        WINDOW_UPDATE                 -> decodeFrameWindowUpdate      s   bs
                        BLOCKED                       -> decodeFrameBlocked           s   bs  
                        RST_STREAM                    -> decodeFrameRstStream         s   bs
                        PADDING                       -> decodeFramePadding           s   bs
                        CONNECTION_CLOSE              -> decodeFrameConnectionClosed  s   bs
                        GOAWAY                        -> decodeFrameGoaway            s   bs
                        PING                          -> decodeFramePing              s   bs
                        _                             -> Left Error.InvalidPacketError
  where
    (b,bs) = (BSL.head bytes, BSL.tail bytes)

    decodeFrameStream :: Settings -> FrameType -> ByteString -> QUICResult (Frame, ByteString)
    decodeFrameStream s f bs = case BG.runGetOrFail (get f) bs of
                                                                 Right (bs, _, frame) -> Right (frame, bs)
                                                                 Left (bs, _, _) -> Left Error.InvalidFrameData
      where
        get :: FrameType -> BG.Get Frame
        get frame = do
          h <- header
          d' <- d
          return $ h{ streamStreamData = d', streamFrameType = frame}
        header :: BG.Get Frame
        header = undefined
        d :: BG.Get ByteString
        d = undefined

    decodeFrameAck :: Settings -> FrameType -> ByteString -> QUICResult (Frame, ByteString)
    decodeFrameAck s frame bs = case BG.runGetOrFail get bs of
                          Right (bs, _, frame)  -> Right (frame, bs)
                          Left (_ ,_, _)        -> Left Error.InvalidAckData
      where
        get :: BG.Get Frame
        get =  undefined
        header = undefined
        ackBlock :: BG.Get F.AckBlock
        ackBlock = undefined
        timeStamp :: BG.Get F.AckTimeStamp
        timeStamp = undefined

                          

    decodeFrameStopWaiting :: Settings -> ByteString -> QUICResult (Frame, ByteString)
    decodeFrameStopWaiting s bs =  case BG.runGetOrFail get bs of
                                   Right (bs, _, frame) -> Right (frame, bs)
                                   Left _               -> Left Error.InvalidStopWaitingData
      where
        get :: BG.Get Frame 
        get = undefined

    decodeFrameWindowUpdate :: Settings -> ByteString -> QUICResult (Frame, ByteString)
    decodeFrameWindowUpdate s bs = case BG.runGetOrFail get bs of
                                   Right (bs, _, frame) -> Right (frame, bs)
                                   Left _               -> Left Error.InvalidWindowUpdateData
      where
        get :: BG.Get Frame 
        get = undefined

    decodeFrameBlocked :: Settings -> ByteString -> QUICResult (Frame, ByteString)
    decodeFrameBlocked s bs = case BG.runGetOrFail get bs of
                                   Right (bs, _, frame) -> Right (frame, bs)
                                   Left _               -> Left Error.InvalidFrameData
      where
        get :: BG.Get Frame
        get = undefined

    decodeFrameCongestionFeedBack :: Settings -> ByteString -> QUICResult (Frame, ByteString)
    decodeFrameCongestionFeedBack s bs = case BG.runGetOrFail get bs of
                                        Right (bs, _, frame) -> Right (frame, bs)
                                        Left _ -> Left Error.InvalidConnectionCloseData
      where 
        get = undefined

    decodeFramePadding :: Settings -> ByteString -> QUICResult (Frame, ByteString)
    decodeFramePadding s bs =  Right (Padding, BSL.empty)

    decodeFrameRstStream :: Settings -> ByteString -> QUICResult (Frame, ByteString)
    decodeFrameRstStream s bs =  case BG.runGetOrFail get bs of
                                      Right (bs, _, frame) -> Right (frame, bs)
                                      Left _    -> Left Error.InvalidRstStreamData
      where
        get :: BG.Get Frame
        get = undefined

    decodeFramePing :: Settings -> ByteString -> QUICResult (Frame, ByteString)
    decodeFramePing s bs = Right (Ping, BSL.empty)

    decodeFrameConnectionClosed :: Settings -> ByteString -> QUICResult (Frame, ByteString)
    decodeFrameConnectionClosed s bs =  case BG.runGetOrFail get bs of
                                             Right (bs, _, frame) -> Right (frame, bs)
                                             Left _ -> Left Error.InvalidConnectionCloseData
      where
        get :: BG.Get Frame
        get = undefined

    decodeFrameGoaway :: Settings -> ByteString -> QUICResult (Frame, ByteString)
    decodeFrameGoaway s bs =  case BG.runGetOrFail get bs of
                              Right (bs, _, frame) -> Right (frame, bs)
                              Left _ -> Left Error.InvalidGoAwayData
      where 
        get :: BG.Get Frame
        get = undefined

