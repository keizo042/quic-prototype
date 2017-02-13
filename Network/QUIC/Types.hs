module Network.QUIC.Types (
  ByteSize
  , ByteSizeInfo
  , ConnectionID
  , StreamID
  , PacketNumber
  )where


type PacketNumber = Integer

type ConnectionID = Integer

type StreamID = Int

type ByteSize = Int

data ByteSizeInfo = NoInfo
                  deriving Show
