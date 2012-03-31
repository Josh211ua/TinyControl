import TinyControl.Server
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)

main =
  do
    h <- open "1514"
    h' <- send h [(pack "hello, world")]
    print (show h)
    close h