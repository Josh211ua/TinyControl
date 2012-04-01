import TinyControl.Client
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)

main =
  do
    (h,f) <- open "localhost" "1514"
    print "sending \"hello, world\"\n"
    handle <- send h f (pack "hello, world")
    print "waiting for a message from server\n"
    (handle, friend, msg) <- recv h
    print ("received this message: " ++ show msg ++ "\n")
    close h
