module Util where

import Control.Monad
import Control.Applicative
import Network.BSD
import Network.Socket
import qualified Network.Socket.ByteString as B
import qualified Data.ByteString as B
import Pipes

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

makeServerTcpSock localPort = do
  serverSock <- makeTcpSock
  bindSocket serverSock (SockAddrInet (fromIntegral localPort) 0)
  listen serverSock 5
  return serverSock

makeClientTcpSock addr = do
  sock <- makeTcpSock
  connect sock addr
  return sock

addrFor :: String -> Int -> IO SockAddr
addrFor hostName port = do
  host:_ <- hostAddresses <$> getHostByName hostName
  return $ SockAddrInet (fromIntegral port) host

