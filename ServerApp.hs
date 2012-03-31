import TinyControl.Server
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)

main =
  do
    h <- open "1514"
    (handle, friend, msg) <- srecv h
    handle <- send handle friend [(pack "hello, world")]
    print (show h)
    close h
