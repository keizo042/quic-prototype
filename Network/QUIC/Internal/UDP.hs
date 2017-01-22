{-# LANGUAGE OverloadedStrings #-}
module Network.QUIC.Internal.UDP 
  ()
  where
import qualified Network.Socket as S
import qualified Network.Socket.ByteString as SB
import qualified Data.ByteString.Lazy.Char8 as BSW


listen :: S.Socket -> IO ()
listen sock = undefined
