import Data.Bits
import Network.Socket
import Network.BSD
import Data.List
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.RWS.Lazy
import Data.Set (Set)
import qualified Data.Set as Set
import Debug.Trace

type HandlerFunc = Socket -> SockAddr -> String -> CountingStateMonad ()

-- serveLogState :: String
--               -> HandlerFunc
--               -> CountingStateMonad Int
-- serveLogState port handlerfunc =
--     do serveLog port handlerfunc
--        count <- get
--        return count

serveLog :: String              -- ^ Port number or name; 514 is default
         -> HandlerFunc         -- ^ Function to handle incoming messages
         -> CountingStateMonad ()
serveLog port handlerfunc =
    do
      h <- helper --a <- tma
      let ioh = return h--let ma = return a
        -- ma should have type (m a) because return is just a typeclass function; this is forced by inference from the next line
      let ioh' = withSocketsDo ioh--let ma' = f ma
        -- because f :: (m a -> m a), we have that the input (ma) is of type (m a); this ensures that return does what we want
      lift ioh'--lift ma' -- since ma' is of type (m a), the lifted version should be (t m a)

    where
      helper :: CountingStateMonad () -- t m a
      helper = do -- Look up the port.  Either raises an exception or returns
         -- a nonempty list.
         addrinfos <- lift $ getAddrInfo
                      (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
                      Nothing (Just port)
         let serveraddr = head addrinfos

         -- Create a socket
         sock <- lift $ socket (addrFamily serveraddr) Datagram defaultProtocol

         -- Bind it to the address we're listening to
         lift $ bindSocket sock (addrAddress serveraddr)

         -- Loop forever processing incoming data.  Ctrl-C to abort.
         procMessages sock
        where procMessages sock =
                do -- Receive one UDP packet, maximum length 1024 bytes,
                   -- and save its content into msg and its source
                   -- IP and port into addr
                   (msg, _, addr) <- lift (recvFrom sock 1024)
                   -- Handle it
                   (trace (show msg)) handlerfunc sock addr msg
                   -- And process more messages
                   procMessages sock

-- A simple handler that prints incoming packets
plainHandler :: HandlerFunc
plainHandler sock addr msg = do
    tell $ Set.singleton $ "From " ++ show addr ++ ": " ++ msg

type CountingStateMonad = RWST Int (Set String) Int IO
increment :: CountingStateMonad ()
increment = do
    count <- get
    put $ count + 1

-- type HandlerFunc = SockAddr -> String -> IO ()
countingHandler :: HandlerFunc
countingHandler sock addr msg = do
    num <- get
    tell $ Set.singleton ((show num) ++ msg)
    result <- lift $ sendTo sock ("msg" ++ show num) addr
    increment


main =
  let thing = serveLog "1514" countingHandler in
  do
    result <- runRWST thing 0 0
    print $ show $ result
-- main = print (evalState (serveLogState "1514" countingHandler) 0)
