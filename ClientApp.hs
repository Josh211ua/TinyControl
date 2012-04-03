import TinyControl.Client
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack, unpack)

main =
  do
      d <- wantData "localhost" "1514" (pack "give me the data")
      putStrLn (unpack d)
--    (h,f) <- open "localhost" "1514"
--    putStrLn ("sending \"hi\" to " ++ show f)
--    handle <- send h f (pack "hi")
--    putStrLn "waiting for a message from server"
--    (handle', friend, msg) <- recv h
--    putStrLn ("received this message: " ++ show msg)
--    threadDelay (10 * 1000 * 1000)
--    putStrLn ("sending \"bye\" to " ++ show f)
--    handle'' <- send handle' friend (pack "bye")
--    putStrLn "waiting for a message from server"
--    (h', friend', msg) <- recv handle''
--    putStrLn ("received this message: " ++ show msg)
--    close h
