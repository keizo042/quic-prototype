{-# LANGUAGE GADTs #-}
module Network.QUIC.Stream
  (
      EndPoint(..)
    , isClient
    , isServer
    , Peer(..)
  )
  where


data EndPoint = Client | Server
              deriving Show

data Peer = Remote | Local
          deriving Show

data State where
            Open :: State
            Idle :: State
            Reserved :: State
            HalfClosed  :: Peer -> State
            Closed :: State

isClient :: EndPoint -> Bool
isClient Client = True
isClient _ = False

isServer :: EndPoint -> Bool
isServer Server = True
isServer _ = False
