module Network.QUIC.Decode
  (
    decodeHeader
  ) where

import qualified Network.QUIC.Frame as F
import           Network.QUIC.Frame   (Frame (..), FrameType (..),
                                       word82FrameType)
import qualified Network.QUIC.Header  as H
import           Network.QUIC.Header  (Flags (..),
                                       Header (..))
import           Network.QUIC.Types   (Nonce, Settings (..))
import qualified Network.QUIC.Stream as S
import qualified Network.QUIC.Internal as BG 

