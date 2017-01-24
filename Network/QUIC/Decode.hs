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
import qualified Network.QUIC.Header  as H
import           Network.QUIC.Header  (Flags (..),
                                       Header (..))
import           Network.QUIC.Types   (Nonce, Settings (..))

import qualified Network.QUIC.Internal as BG 

decodeHeader :: Settings -> ByteString -> QUICResult (Header, ByteString)
decodeHeader s bs = case BG.runGetOrFail get bs of
                         Right (bs, _, h) -> Right (h, bs)
                         Left _ -> undefined
  where
    get :: BG.Get Header
    get = Header <$> flag <*> conn <*> ver <*> (fromIntegral <$> BG.getIntN 32) <*> num
      where
        flag :: BG.Get Flags
        flag = H.word82flags <$> fromIntegral <$> BG.getInt8
        conn :: BG.Get (Maybe Int64)
        conn = undefined
        ver :: BG.Get (Maybe Int32)
        ver = undefined
        num :: BG.Get Integer
        num = undefined



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
          Stream frame <$> (sid stream) <*> (BG.getIntN off) <*> (body dlen)
        sid ::  Int -> BG.Get Int
        sid 0 =  return 0
        sid n = BG.getIntN n

        body :: Bool -> BG.Get BSL.ByteString
        body False  = BG.getRemainingLazyByteString
        body True = fromIntegral <$> BG.getInt16 >>= BG.getLazyByteString 

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
          Ack frame acked' delay block <$> lack <*> timeSinceLargestAcked <*> (timeStamp s)

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
        timeStamp n = do
          i <- f 
          e0 <- timeStamp (n -1)
          return (i:e0)
          where
            f = undefined
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
        get = ConnectionClose <$> err <*> reason
          where
            err :: BG.Get Error.ErrorCodes
            err = Error.int2err <$> BG.getInt32
            reason :: BG.Get ByteString
            reason = fromIntegral <$> BG.getInt16 >>= BG.getLazyByteString


    decodeFrameGoaway :: Settings -> ByteString -> QUICResult (Frame, ByteString)
    decodeFrameGoaway s bs =  case BG.runGetOrFail get bs of
                              Right (bs, _, frame) -> Right (frame, bs)
                              Left _ -> Left Error.InvalidGoAwayData
      where 
        get :: BG.Get Frame
        get = Goaway <$> err <*> BG.getInt32 <*> reason
          where
            err :: BG.Get Error.ErrorCodes
            err = Error.int2err <$> BG.getInt32
            reason :: BG.Get ByteString
            reason = fromIntegral <$> BG.getInt16 >>= BG.getLazyByteString

