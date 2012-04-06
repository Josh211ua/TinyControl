import TinyControl.Client
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack, unpack)

main =
  do
      d <- wantData "localhost" "1514" (pack "give me the data")
      putStrLn (unpack d)
