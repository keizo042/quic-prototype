module Network.QUIC.Internal.Frame.Ack.Block where

data AckBlock = AckBlock { ackBlockgap :: Maybe Int
                         , ackBlockLength      :: Int
                         } deriving Show

