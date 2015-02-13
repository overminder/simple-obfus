import System.Environment

import Control.Monad
import Control.Applicative
import Control.Exception
import Network.Socket
import Network.BSD
import qualified Network.Socket.ByteString as B
import qualified Data.ByteString as B
import Data.Obfus.ByteString
import Pipes
import Control.Concurrent.Async

fromSocket :: Socket -> Int -> Producer B.ByteString IO ()
fromSocket s siz = do
  bs <- lift $ B.recv s siz
  if B.null bs
    then return ()
    else yield bs >> fromSocket s siz

toSocket :: Socket -> Consumer B.ByteString IO ()
toSocket s = forever $ do
  bs <- await
  lift $ B.sendAll s bs

makeTcpSock = do
  s <- socket AF_INET Stream defaultProtocol
  setSocketOption s ReuseAddr 1
  return s

main = do
  [localPort, remoteHostName, remotePort] <- getArgs
  key <- B.getContents

  remoteHost:_ <- hostAddresses <$> getHostByName remoteHostName
  let
    remoteAddr = SockAddrInet (fromIntegral (read remotePort)) remoteHost

  serverSock <- makeTcpSock
  bindSocket serverSock (SockAddrInet (fromIntegral (read localPort)) 0)
  listen serverSock 5
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


    
