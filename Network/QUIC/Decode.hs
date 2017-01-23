module Network.QUIC.Decode
  (
    decodeHeader
  , decodeFrame
  ) where
import qualified Data.Binary.Get as BG (getLazyByteString, getRemainingLazyByteString,runGetOrFail, Get)
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

import qualified Network.QUIC.Internal as BG 

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
    decodeFrameStream s f@(STREAM{}) bs = case BG.runGetOrFail (get f) bs of
                                                                 Right (bs, _, frame) -> Right (frame, bs)
                                                                 Left _ -> Left Error.InvalidFrameData
      where
        get :: FrameType -> BG.Get Frame
        get frame@(STREAM fin dlen off stream) = do
          (s,o) <- header off stream
          d' <- dataBody dlen
          return $ Stream frame s o d'

        header :: Int -> Int -> BG.Get (Int, Int)
        header offset streamId = do 
          s <- streamId' streamId
          o <- offset' offset
          return $ (s,o)
              where
                streamId' :: Int -> BG.Get Int
                streamId' s = case s of
                    8 ->  BG.getInt8
                    16 -> BG.getInt16
                    32 -> BG.getInt32
                    64 -> BG.getInt64

                offset' :: Int -> BG.Get Int
                offset' o = case o of
                    0 ->  return 0
                    8 ->  BG.getInt8
                    16 -> BG.getInt16
                    24 -> BG.getInt24
                    32 -> BG.getInt32
                    48 -> BG.getInt48
                    56 -> BG.getInt56
                    64 -> BG.getInt64


        dataBody :: Bool -> BG.Get BSL.ByteString
        dataBody False  = BG.getRemainingLazyByteString
        dataBody True = fromIntegral <$> BG.getInt16 >>= BG.getLazyByteString 

    decodeFrameStream _ _ _ = Left Error.InvalidFrameData

    decodeFrameAck :: Settings -> FrameType -> ByteString -> QUICResult (Frame, ByteString)
    decodeFrameAck s f@(ACK{}) bs = case BG.runGetOrFail (get f) bs of
                          Right (bs, _, frame)  -> Right (frame, bs)
                          Left _        -> Left Error.InvalidAckData
      where
        get :: FrameType -> BG.Get Frame
        get frame@(ACK f a len)=  do
          acked' <- acked a
          delay <- BG.getInt16
          block <- ackBlock f len
          s     <- BG.getInt8
          dlargest <- lack
          t <- timeSinceLargestAcked 
          stamp <- timeStamp s

          return $ Ack frame acked' delay block 0 dlargest t (Just stamp)

        acked :: Int -> BG.Get Int
        acked n = undefined

        ackBlock :: Bool -> Int -> BG.Get [F.AckBlock]
        ackBlock b n 
          | b == False = g n
          | otherwise =  do
                l <- BG.getInt8
                head' <- g n
                rest' <- f l
                return  $ head' ++ rest'
                
            where
              g :: Int -> BG.Get [F.AckBlock]
              g len = do 
                n <- BG.getIntN len
                return $ [F.AckBlock Nothing n]

              f :: Int -> BG.Get [F.AckBlock]
              f 0 =  return []
              f n =  do
                e0 <- f' n
                e1 <- f (n -1)
                return (e0:e1)

              f' :: Int -> BG.Get F.AckBlock
              f' len = F.AckBlock <$> (Just <$> BG.getInt8) <*> (BG.getIntN len)

        lack :: BG.Get (Maybe Int)
        lack = undefined

        timeSinceLargestAcked  :: BG.Get (Maybe Int)
        timeSinceLargestAcked = undefined

        timeStamp :: Int ->  BG.Get [F.AckTimeStamp]
        timeStamp 0 = return []
        timeStamp n = undefined
    decodeFrameAck _ _ _ = Left Error.InvalidAckData

                          

    decodeFrameStopWaiting :: Settings -> ByteString -> QUICResult (Frame, ByteString)
    decodeFrameStopWaiting s bs =  case BG.runGetOrFail (get $ packetNumberSize s) bs of
                                   Right (bs, _, frame) -> Right (frame, bs)
                                   Left _               -> Left Error.InvalidStopWaitingData
      where
        get :: Int -> BG.Get Frame 
        get n = StopWaiting <$> (BG.getIntN n)

    decodeFrameWindowUpdate :: Settings -> ByteString -> QUICResult (Frame, ByteString)
    decodeFrameWindowUpdate s bs = case BG.runGetOrFail get bs of
                                   Right (bs, _, frame) -> Right (frame, bs)
                                   Left _               -> Left Error.InvalidWindowUpdateData
      where
        get :: BG.Get Frame 
        get  = WindowUpdate <$> BG.getInt32 <*> BG.getInt64

    decodeFrameBlocked :: Settings -> ByteString -> QUICResult (Frame, ByteString)
    decodeFrameBlocked s bs = case BG.runGetOrFail get bs of
                                   Right (bs, _, frame) -> Right (frame, bs)
                                   Left _               -> Left Error.InvalidFrameData
      where
        get :: BG.Get Frame
        get = Blocked <$> BG.getInt32

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
        get = RstStream <$> BG.getInt32 <*> BG.getInt64 <*> (Error.int2err <$> BG.getInt32)

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

