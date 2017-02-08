module Network.QUIC.Internal.Flags where

flag :: BG.Get Flags
flag = H.word82flags <$> fromIntegral <$> BG.getInt8
conn :: Bool -> BG.Get (Maybe Int64)
conn True = BG.getInt64 >>=  return . Just . fromIntegral 
conn False = return Nothing
ver :: Bool -> BG.Get (Maybe Int32)
ver True = BG.getInt32 >>= return . Just . fromIntegral
ver False =  return Nothing
num :: BG.Get Integer
num = undefined
