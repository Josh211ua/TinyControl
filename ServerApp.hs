import TinyControl.Server
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack, unpack)
import Control.Concurrent(myThreadId, forkIO, threadDelay)

main :: IO ()
main = do
    msg <- readFile "testdata"
    serveData "1514" (\d -> do {return (pack msg)})
