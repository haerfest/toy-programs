import System.Time
import System.Locale
import System.ZMQ
import Text.Printf
import qualified Data.ByteString.Char8 as B


type Topic   = B.ByteString
type Message = B.ByteString


-- |Used to subscribe to all topics sent by a publisher.
allTopics :: String
allTopics = ""


-- |Return a string containing the local time.
getLocalTime :: IO String
getLocalTime = do
  cal <- getClockTime >>= toCalendarTime
  let hour = ctHour cal
      min  = ctMin cal
      sec  = ctSec cal
      msec = round $ fromIntegral (ctPicosec cal) / 1e9 :: Int
  return $ printf "%02u:%02u:%02u.%03u" hour min sec msec


-- |Print the current time and the topic of a received message.
processMsg :: Topic -> Maybe Message -> IO ()
processMsg topic message = do
  getLocalTime >>= return . (++ ": ") >>= putStr
  B.putStrLn topic


-- |Receive messages from a ZMQ subscription socket and print the topics.
receiveMsgs :: Socket Sub -> (Topic -> Maybe Message -> IO ()) -> IO ()
receiveMsgs socket processFn = do
  topic <- receive socket []
  is_multi <- moreToReceive socket
  msg <- if is_multi then do
           receive socket [] >>= return . Just
         else
           return Nothing
  processFn topic msg
  receiveMsgs socket processFn


-- |Subscribe to a ZMQ endpoint and print the topic of each received message.
-- The endpoint is a ZMQ endpoint specifier such as "tcp://127.0.0.1:1234".
listen :: String -> IO ()
listen endpoint = withContext 1 useContext
  where
    useContext context = withSocket context Sub useSocket
    useSocket socket = do
      connect socket endpoint
      subscribe socket allTopics
      receiveMsgs socket processMsg
