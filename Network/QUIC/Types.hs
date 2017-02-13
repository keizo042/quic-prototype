module Network.QUIC.Types (
  ByteSize
  , ByteSizeInfo
  , ConnectionID
  , StreamID
  , countStreamIDbyteSize
  , PacketNumber
  )where


type PacketNumber = Integer

type ConnectionID = Integer

type StreamID = Int

countStreamIDbyteSize :: StreamID -> ByteSize
countStreamIDbyteSize i = undefined

type ByteSize = Int

data ByteSizeInfo = NoInfo
                  deriving Show
