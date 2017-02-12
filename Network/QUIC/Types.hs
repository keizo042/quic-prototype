module Network.QUIC.Types (
  ByteSize
  )where



type ByteSize = Int

type ConnectionID = Int

data ByteSizeInfo = NoInfo
                  deriving Show
