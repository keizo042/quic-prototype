{-# LANGUAGE CPP #-}
module Network.QUIC.Header
  (
      Header(..)
  ) where
import           Data.Int
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.Bits
import Data.Word 

import Network.QUIC.Types
import Network.QUIC.Internal.Version

data Header = RegularHeader { regularHeaderConnectionID         :: Maybe ConnectionID
            ,regularHeaderQuicVersion          :: Maybe Version
            ,regularHeaderDiversificationNonce :: Integer
            ,regularHeaderPacketNumber         :: PacketNumber
            } 

