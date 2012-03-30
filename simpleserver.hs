import Data.Bits
import Network.Socket
import Network.BSD
import Data.List
import Control.Monad.State

type HandlerFunc = SockAddr -> String -> IO ()

-- serveLogState :: String
--               -> HandlerFunc
--               -> CountingStateMonad Int
-- serveLogState port handlerfunc =
--     do serveLog port handlerfunc
--        count <- get
--        return count

serveLog :: String              -- ^ Port number or name; 514 is default
         -> HandlerFunc         -- ^ Function to handle incoming messages
         -> IO ()
serveLog port handlerfunc = withSocketsDo $
    do -- Look up the port.  Either raises an exception or returns
       -- a nonempty list.  
       addrinfos <- getAddrInfo 
                    (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
                    Nothing (Just port)
       let serveraddr = head addrinfos

       -- Create a socket
       sock <- socket (addrFamily serveraddr) Datagram defaultProtocol

       -- Bind it to the address we're listening to
       bindSocket sock (addrAddress serveraddr)

       -- Loop forever processing incoming data.  Ctrl-C to abort.
       procMessages sock
    where procMessages sock =
              do -- Receive one UDP packet, maximum length 1024 bytes,
                 -- and save its content into msg and its source
                 -- IP and port into addr
                 (msg, _, addr) <- recvFrom sock 1024
                 -- Handle it
                 handlerfunc addr msg
                 -- And process more messages
                 procMessages sock

-- A simple handler that prints incoming packets
plainHandler :: HandlerFunc
plainHandler addr msg = 
    putStrLn $ "From " ++ show addr ++ ": " ++ msg

type CountingStateMonad = State Int
increment :: CountingStateMonad Int
increment = do
    count <- get
    put $ count + 1
    return count

incrementAndGet :: CountingStateMonad Int
incrementAndGet = do
    count <- increment
    count <- increment
    return count

-- type HandlerFunc = SockAddr -> String -> IO ()
countingHandler :: HandlerFunc
countingHandler addr msg = do
    putStrLn $ show $ evalState incrementAndGet 0

main = serveLog "1514" countingHandler
-- main = print (evalState (serveLogState "1514" countingHandler) 0)
