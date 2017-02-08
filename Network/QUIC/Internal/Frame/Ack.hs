module Network.QUIC.Internal.Frame.Ack where
import Data.Binary.Get
import Data.Binary.Put
import Data.Word
import Network.QUIC.Error
import Network.QUIC.Internal.Frame.Ack.Block
import Network.QUIC.Internal.Frame.Ack.TimeStamp


data AckFrame = AckFrame { ackHasAckBlock :: Bool
                         , ackLargestAckedSize :: Int
                         , ackBlockLengthSize :: Int
                         , ackLargestAcked         :: Int
                         , ackDelay             :: Int
                         , ackBlock             :: [AckBlock]
                         , ackDeltaLargestAcked :: Maybe Int
                         , ackTimeLargestAcked     :: Maybe Int
                         , ackTimeStamps        :: [AckTimeStamp]
                         } deriving Show

decodeFrameAck ::  ByteString -> QUICResult (Frame, ByteString)
decodeFrameAck bs = case BG.runGetOrFail (get f) bs of
                      Right (bs, _, frame)  -> Right (frame, bs)
                      Left _        -> Left Error.InvalidAckData
  where
    get  -> BG.Get Frame
    get =  do
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
          g len = BG.getIntN len >>= ( \n -> return [F.AckBlock Nothing n])

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
