import Control.Monad
import System.Time
import System.ZMQ
import Text.Printf
import qualified Data.ByteString.Char8 as B


type Topic   = B.ByteString
type Message = B.ByteString


-- |Used to subscribe to all topics sent by a publisher.
allTopics :: String
allTopics = ""


-- |Convert a calendar time to a time string with a millisecond resolution.
formatLocalTime :: CalendarTime -> String
formatLocalTime c = do
  let hour = ctHour c
      min  = ctMin c
      sec  = ctSec c
      msec = round $ fromIntegral (ctPicosec c) / 1e9 :: Int
  printf "%02u:%02u:%02u.%03u" hour min sec msec


-- |Print the current time, followed by the topic of a received message.
printTopic :: Topic -> Maybe Message -> IO ()
printTopic t _ = do
  c <- toCalendarTime =<< getClockTime
  putStr $ formatLocalTime c ++ ": "
  B.putStrLn t


-- |Receive messages from a ZMQ subscription socket and print the topics.
receiveMsgs :: Socket Sub -> (Topic -> Maybe Message -> IO ()) -> IO ()
receiveMsgs s f = do
  topic <- receive s []
  is_multi <- moreToReceive s
  if is_multi then
    f topic =<< liftM Just (receive s [])
  else
    f topic Nothing
  receiveMsgs s f


-- |Subscribe to a ZMQ endpoint and print the topic of each received message.
-- The endpoint is a ZMQ endpoint specifier such as "tcp://127.0.0.1:1234".
listen :: String -> IO ()
listen endpoint = withContext 1 f
  where
    f :: Context -> IO ()
    f c = withSocket c Sub g

    g :: Socket Sub -> IO ()
    g s = do
      connect s endpoint
      subscribe s allTopics
      receiveMsgs s printTopic
