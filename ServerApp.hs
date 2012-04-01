import TinyControl.Server
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack, unpack)
import Control.Concurrent(myThreadId, forkIO)

serverThread :: ServerHandle -> Friend -> Data -> IO ()
serverThread handle friend msg = do
    myId <- myThreadId
    putStrLn (show myId ++ ": Received: " ++ (unpack msg))
    handle <- send handle friend (pack "hello")
    (handle, friend, msg) <- recv handle
    putStrLn (show myId ++ ": Received: " ++ (unpack msg))
    handle <- send handle friend (pack "goodbye")
    close handle

serve :: ServerHandle -> IO ()
serve h = do
    (handle, friend, msg) <- srecv h
    threadId <- forkIO (serverThread handle friend msg)
    serve h

main :: IO ()
main = do
    h <- open "1514"
    serve h
