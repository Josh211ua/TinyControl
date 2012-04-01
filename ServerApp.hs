import TinyControl.Server
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack, unpack)

main =
  do
    h <- open "1514"
    putStrLn ("opened handle:" ++ show h)
    (handle, friend, msg) <- srecv h
    putStrLn ("Received: " ++ (unpack msg))
    handle <- send handle friend (pack "hello")
    (handle, friend, msg) <- recv handle
    putStrLn ("Received: " ++ (unpack msg))
    handle <- send handle friend (pack "goodbye")
    close h
