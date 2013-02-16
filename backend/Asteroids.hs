{-# LANGUAGE OverloadedStrings #-}
import Control.Monad
import Control.Monad.Trans
import Control.Concurrent
import Control.Concurrent.STM

import Network (listenOn, PortID(PortNumber))
import Network.Socket (accept, withSocketsDo, Socket)
import Network.WebSockets

import Data.Aeson
import qualified Data.ByteString.Lazy as BS

data ServerEvent = ServerEvent
data ClientEvent = ClientConnected String (TChan ServerEvent)
data WireMessageFromClient = Hello { _nick :: String }

instance FromJSON WireMessageFromClient where
  parseJSON (Object v) = undefined
  parseJSON _ = mzero

newClient :: Int -> TChan ClientEvent -> Request -> WebSockets Hybi10 ()
newClient id chan rq = do
  liftIO $ putStrLn "Ktos przylazl"
  acceptRequest rq
  msg <- (receiveData :: WebSockets Hybi10 BS.ByteString)
  liftIO $ BS.putStrLn msg
  case decode msg :: Maybe WireMessageFromClient of
    Nothing -> liftIO . putStrLn $ "malformed message"
    Just (Hello nick) -> handleHelloMessage nick
  return ()
  where handleHelloMessage nick = do
          clientChan <- liftIO . atomically $ newTChan
          networkChan <- liftIO . atomically $ newTChan
          sink <- getSink
          liftIO $ putStrLn (nick ++ " connected")
          liftIO . atomically $ writeTChan chan (ClientConnected nick clientChan)
          liftIO $ forkIO $ clientLoop clientChan chan networkChan sink nick
          clientNetworkLoop networkChan

clientNetworkLoop networkChan = do
  msg <- (receiveData :: WebSockets Hybi10 BS.ByteString)
  case decode msg :: Maybe WireMessageFromClient of
    Nothing -> liftIO . putStrLn $ "malformed message"
    Just wireMessage -> liftIO . atomically $ writeTChan networkChan wireMessage
  clientNetworkLoop networkChan

clientLoop = undefined

acceptLoop :: [Int] -> Socket -> TChan ClientEvent -> IO ()
acceptLoop (id:ids) serverSock chan = do
  (clientSock, sockAddr) <- accept serverSock
  forkIO $ runWithSocket clientSock (newClient id chan)
  acceptLoop ids serverSock chan

main :: IO ()
main = withSocketsDo $ do
  putStrLn "Hello, world!"
  chan <- atomically newTChan
  servSock <- listenOn $ PortNumber 9160
  acceptLoop [0..] servSock chan
