import Control.Monad (when)
import System.ZMQ
import qualified Data.ByteString.Char8 as B

-- |Receive messages from a ZMQ subscription socket and print the topics.
receiveMsgs :: Socket Sub -> IO ()
receiveMsgs socket = do
  topic <- receive socket []
  B.putStrLn topic
  is_multi <- moreToReceive socket
  when is_multi $ do
    _ <- receive socket []
    return ()
  receiveMsgs socket

-- |Subscribe to a ZMQ endpoint and print the topic of each recevied message.
-- The endpoint is a ZMQ endpoint specifier such as "tcp://127.0.0.1:1234".
listen :: String -> IO ()
listen endpoint = withContext 1 useContext
  where
    useContext c = withSocket c Sub useSocket
    useSocket s = do
      connect s endpoint
      subscribe s ""
      receiveMsgs s
