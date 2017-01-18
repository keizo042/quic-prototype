module Network.QUIC.Stream
  (
  )
  where

data EndPoint = Client | Server
data State = Idle 
           | Open
           | Reserved
           | HalfClosed
           | Closed
