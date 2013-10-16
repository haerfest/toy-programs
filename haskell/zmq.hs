import Data.Time
import System.Locale
import System.ZMQ

import qualified Data.ByteString.Char8 as B


type Topic     = B.ByteString
type Message   = B.ByteString
type ProcessFn = Topic -> Maybe Message -> IO ()


-- |Return a string containing the local time.
getLocalTime :: IO String
getLocalTime = do
  tz <- getCurrentTimeZone
  now_utc <- getCurrentTime
  let now_local = utcToLocalTime tz now_utc
  return $ formatTime defaultTimeLocale "%X.%q" now_local


-- |Print the current time and the topic of a received message.
processMsg :: Topic -> Maybe Message -> IO ()
processMsg topic message = do
  now <- getLocalTime
  putStr (now ++ ": ")
  B.putStrLn topic


-- |Receive messages from a ZMQ subscription socket and print the topics.
receiveMsgs :: Socket Sub -> ProcessFn -> IO ()
receiveMsgs socket processFn = do
  topic <- receive socket []
  is_multi <- moreToReceive socket
  msg <- if is_multi then do
           msg <- receive socket []
           return $ Just msg
         else
           return Nothing
  processFn topic msg
  receiveMsgs socket processFn


-- |Subscribe to a ZMQ endpoint and print the topic of each received message.
-- The endpoint is a ZMQ endpoint specifier such as "tcp://127.0.0.1:1234".
listen :: String -> IO ()
listen endpoint = withContext 1 useContext
  where
    useContext c = withSocket c Sub useSocket
    useSocket s = do
      connect s endpoint
      subscribe s ""
      receiveMsgs s processMsg
