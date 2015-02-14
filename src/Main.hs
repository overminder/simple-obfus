{-# LANGUAGE ViewPatterns #-}

import System.Environment

import Control.Monad
import Control.Applicative
import Control.Exception
import Network.Socket
import qualified Data.ByteString as B
import Control.Concurrent.Async
import Pipes

import Util
import Data.Obfus.ByteString

main = do
  [read -> localPort, remoteHostName, read -> remotePort] <- getArgs
  key <- B.getContents

  remoteAddr <- addrFor remoteHostName remotePort
  serverSock <- makeServerTcpSock localPort

  forever $ do
    (peer, addr) <- accept serverSock
    putStrLn $ "[serverLoop] Accepted " ++ show addr
    async (pipePeerTo peer remoteAddr key `finally`
           closeWithTag "serverLoop" peer)

 where
  closeWithTag tag s = do
    putStrLn $ "[closeWithTag] @" ++ tag
    close s

  pipePeerTo peerSock remoteAddr key = bracket
    (do s <- makeTcpSock
        connect s remoteAddr
        putStrLn $ "[pipePeerTo] Connected to " ++ show remoteAddr
        return s)
    (closeWithTag "pipePeerTo")
    (\ remoteSock -> runPipeLoop peerSock remoteSock key)

  runPipeLoop p1 p2 key = do
    t1 <- async $ runEffect $
      fromSocket p1 4096 >-> rc4Stream key >-> toSocket p2
    t2 <- async $ runEffect $
      fromSocket p2 4096 >-> rc4Stream key >-> toSocket p1
    res <- waitEitherCatch t1 t2
    putStrLn $ "[runPipeLoop] done with " ++ show res


    
