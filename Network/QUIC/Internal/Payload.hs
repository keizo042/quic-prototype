module Network.QUIC.Internal.Payload 
  (
    Payload(..)
  )where
import Network.QUIC.Internal.Frame

data Payload = Stream StreamFrame
             | Ack AckFrame
             | Padding PaddingFrame
             | Ping PingFrame
             | WindowUpdate WindowUpdateFrame
             | Blocked BlockedFrame
             | GoAway GoAwayFrame
             | RstStream RstStreamFrame
             | StopWaiting StopWaitingFrame
             | ConnectionClosed ConnectionClosedFrame
              deriving Show


