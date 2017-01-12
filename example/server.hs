module Main where
import           Network.QUIC

import           System.Posix.Directory
import           System.Posix.Exit
import           System.Posix.Files
import           System.Posix.IO
import           System.Posix.Process


main :: IO ()
main = do
  pid <- forkProcess child
  exitImmediately ExitSucess

child :: IO ()
child = do
     changeWorkingDirectory "/"
     setFileCreationMask 0
     mapM_ closeFd [stdInput, stdOutput, stdError]
     nullFd <- openFd "/dev/null" ReadWrite Nothing  defaultFileFlags
     mapM_ (dupTo nullFd) [stdInput, stdOutput, stdError]
     closeFd nullFd
     createSession     
     pid' <- forkProcess server

     exitImmediately ExitSuccess

server :: IO ()
server = undefined
