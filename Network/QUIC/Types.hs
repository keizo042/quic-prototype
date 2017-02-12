module Network.QUIC.Types (
  ByteSize
  , ByteSizeInfo
  , ConnectionID
  , PacketNumber
  )where


type PacketNumber = Integer

type ConnectionID = Integer

type ByteSize = Int

data ByteSizeInfo = NoInfo
                  deriving Show
