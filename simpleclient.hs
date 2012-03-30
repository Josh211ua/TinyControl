import Data.Bits
import Network.Socket
import Network.BSD
import Data.List

data Handle = 
    Handle {hSocket :: Socket,
            hProgram :: String,
            hAddress :: SockAddr}

open :: HostName             -- ^ Remote hostname, or localhost
     -> String               -- ^ Port number or name; 514 is default
     -> String               -- ^ Name to log under
     -> IO Handle      -- ^ Handle to use for logging
open hostname port progname =
    do -- Look up the hostname and port.  Either raises an exception
       -- or returns a nonempty list.  First element in that list
       -- is supposed to be the best option.
       addrinfos <- getAddrInfo Nothing (Just hostname) (Just port)
       let serveraddr = head addrinfos

       -- Establish a socket for communication
       sock <- socket (addrFamily serveraddr) Datagram defaultProtocol

       -- Save off the socket, program name, and server address in a handle
       return $ Handle sock progname (addrAddress serveraddr)

sendmsg :: Handle -> String -> IO ()
sendmsg handle msg = do
    sendstr msg
    count <- receivecount
    putStrLn count
    where -- Send until everything is done
          sendstr :: String -> IO ()
          sendstr [] = return ()
          sendstr omsg = do sent <- sendTo (hSocket handle) omsg
                                    (hAddress handle)
                            sendstr (genericDrop sent omsg)
          receivecount :: IO String
          receivecount = do
              (msg, num, addr) <- recvFrom (hSocket handle) 1024
              return msg
          
close :: Handle -> IO ()
close handle = sClose (hSocket handle)

main =
    do h <- open "localhost" "1514" "test"
       sendmsg h "Hello World"
       close h
