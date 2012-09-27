import Control.Monad.Trans
import Control.Concurrent
import Control.Concurrent.STM

import Network.Socket (accept, withSocketsDo, Socket)
import Network.WebSockets

data ServerEvent
data ClientEvent = ClientConnected String ServerEvent
data WireMessageFromClient = Hello { _nick :: String }

newClient :: TChan ClientEvent -> Request -> WebSockets Hybi10 ()
newClient id chan rq = do
  liftIO $ putStrLn "Ktos przylazl"
  acceptRequest rq
  msg <- (receiveData :: WebSockets Hybi10 String)
  case wireMessageFromString msg of
    Ok (Hello nick) -> handleHelloMessage nick
    Error str -> liftIO $ putStrLn ("JSON error: " ++ str)
    _ -> liftIO $ putStrLn ("Unexpected message")
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
  msg <- (receiveData :: WebSockets Hybi10 String)
  case wireMessageFromString msg of
    Ok wireMessage -> liftIO . atomically $ writeTChan networkChan wireMessage
    Error str -> liftIO . putStrLn $ "JSON error: " ++ str
  clientNetworkLoop networkChan

acceptLoop :: Socket -> TChan ClientMessage -> IO ()
acceptLoop (id:ids) serverSock chan = do
  (clientSock, sockAddr) <- accept serverSock
  forkIO $ runWithSocket clientSock (newClient id chan)
  acceptLoop ids serverSock chan

main :: IO ()
main = withSocketsDo $ do
  putStrLn "Hello, world!"
  servSock <- listenOn $ PortNumber 9160
  forkIO $ acceptLoop [0..] servSock chan
