module Network.QUIC.Internal.Frame.Ack.Block 
  (
    AckBlock(..)
    , getAckBlock
    , putAckBlock
  )where
import qualified Network.QUIC.Error as E

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

import Data.Binary.Get
import Data.Binary.Put


data AckBlock = AckBlock { ackBlockGap :: Maybe Int
                         , ackBlockLength      :: Int
                         } deriving Show

getAckBlock :: Get AckBlock
getAckBlock = undefined

putAckBlock :: AckBlock -> Put
putAckBlock (AckBlock gap len)= undefined
