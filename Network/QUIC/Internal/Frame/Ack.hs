module Network.QUIC.Internal.Frame.Ack where
import Data.Binary.Get
import Data.Binary.Put
import Data.Word
import Data.ByteString.Lazy
import Network.QUIC.Error
import Network.QUIC.Internal
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

decodeAckFrame ::  ByteString -> QUICResult (AckFrame, ByteString)
decodeAckFrame bs = case runGetOrFail (get f) bs of
                      Right (bs, _, frame)  -> Right (frame, bs)
                      Left _        -> Left InvalidAckData
  where
    get  :: Get AckFrame
    get =  do
      acked' <- acked a
      delay <- getInt16
      block <- ackBlock f len
      s     <- getInt8
      Ack frame acked' delay block <$> lack <*> timeSinceLargestAcked <*> (timeStamp s)

    acked :: Int -> Get Int
    acked n = undefined

    ackBlock :: Bool -> Int -> Get [AckBlock]
    ackBlock b n 
      | b == False = g n
      | otherwise =  do
            l <- getInt8
            head' <- g n
            rest' <- f l
            return  $ head' ++ rest'
            
        where
          g :: Int -> Get [AckBlock]
          g len = getIntN len >>= ( \n -> return [AckBlock Nothing n])

          f :: Int -> Get [AckBlock]
          f 0 =  return []
          f n =  do
            e0 <- f' n
            e1 <- f (n -1)
            return (e0:e1)

          f' :: Int -> Get AckBlock
          f' len = AckBlock <$> (Just <$> getInt8) <*> (getIntN len)

    lack :: Get (Maybe Int)
    lack = undefined

    timeSinceLargestAcked  :: Get (Maybe Int)
    timeSinceLargestAcked = undefined

    timeStamp :: Int ->  Get [AckTimeStamp]
    timeStamp 0 = return []
    timeStamp n = do
      i <- f 
      e0 <- timeStamp (n -1)
      return (i:e0)
      where
        f = undefined
decodeFrameAck _ _ _ = Left InvalidAckData
