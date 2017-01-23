{-# LANGUAGE GADTs #-}
module Network.QUIC.Stream
  (
  )
  where

data EndPoint = Client | Server
data Peer = Remote | Local
data State where
           | Open :: State
           | Idle :: State
           | Reserved :: State
           | HalfClosed  :: Peer -> State
           | Closed :: State
