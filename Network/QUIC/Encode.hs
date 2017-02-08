{-# LANGUAGE OverloadedStrings #-}
module Network.QUIC.Encode where

import Data.ByteString      (ByteString (..))
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BSL
import           Network.QUIC.Error   (QUICResult (..))
import qualified Network.QUIC.Error   as Error
import Network.QUIC.Frame   (Frame(..), FrameType(..))
import qualified Network.QUIC.Frame   as Frame
import Network.QUIC.Header  (Header (..))
import Network.QUIC.Types   (Settings(..))

encodeHeader :: Settings ->  Header -> ByteString
encodeHeader = undefined

