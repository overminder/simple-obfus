{-# LANGUAGE ViewPatterns #-}

import System.Environment

import Control.Monad
import Control.Applicative
import Control.Exception
import Network.Socket
import Network.BSD
import qualified Data.ByteString as B
import Control.Concurrent.Async
import Pipes
import System.Timeout

import Util
import Data.Obfus.ByteString

main = do
  [action, read -> localPort, remoteHostName, read -> remotePort] <- getArgs
  key <- B.getContents

  remoteAddr <- addrFor remoteHostName remotePort
  serverSock <- makeServerTcpSock localPort

  case action of
    "L" -> runLocal serverSock remoteAddr key
    "R" -> runRemote serverSock remoteAddr key

runRemote serverSock remoteAddr key = forever $ do
  (wPeer, wAddr) <- accept serverSock
  mbRPair <- timeout 1000000 $ accept serverSock
  case mbRPair of
    Just (rPeer, rAddr) -> do
      putStrLn $ "[serverLoop] Accepted " ++ show rAddr ++ " and " ++ show wAddr
      void $ async (pipePeer wPeer rPeer remoteAddr key `finally`
        closeL "finally" [rPeer, wPeer])
    Nothing ->
      closeL "accept-second-timeout" [wPeer]

 where
  pipePeer wPeer rPeer remoteAddr key = bracket
    (do s <- makeTcpSock
        connect s remoteAddr
        putStrLn $ "[pipePeerTo] Connected to " ++ show remoteAddr
        return s)
    (closeL "pipePeerTo" . (:[]))
    (\ remoteSock -> runPipeLoop wPeer rPeer remoteSock key)

runPipeLoop wPeer rPeer connPeer key = do
  t1 <- async $ runEffect $
    fromSocket rPeer 4096 >-> rc4Stream key >-> toSocket connPeer
  t2 <- async $ runEffect $
    fromSocket connPeer 4096 >-> rc4Stream key >-> toSocket wPeer
  res <- waitEitherCatch t1 t2
  putStrLn $ "[runPipeLoop] done with " ++ show res


runLocal serverSock remoteAddr key = forever $ do
  (peer, addr) <- accept serverSock
  putStrLn $ "[runLocal] Accepted " ++ show addr
  async (pipePeer peer remoteAddr key `finally` closeL "finally" [peer])
 where
  pipePeer peer remoteAddr key = bracket
    (do [rPeer, wPeer] <- replicateM 2 $ makeClientTcpSock remoteAddr
        putStrLn $ "[pipePeerTo] Connected to " ++ show remoteAddr
        return [rPeer, wPeer])
    (closeL "pipePeerTo")
    (\ [rPeer, wPeer] -> runPipeLoop wPeer rPeer peer key)

closeL tag xs = do
  putStrLn $ "[closeL] @" ++ tag
  mapM_ close xs
