import TinyControl.Client
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)

main =
  do
    (h,f) <- open "localhost" "1514"
    putStrLn "sending \"hello, world\""
    handle <- send h f (pack "hello, world")
    putStrLn "waiting for a message from server"
    (handle, friend, msg) <- recv h
    putStrLn ("received this message: " ++ show msg)
    close h
