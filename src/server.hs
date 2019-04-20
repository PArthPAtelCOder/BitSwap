import System.IO
import qualified Data.Map as M
import Data.Bits
import Network.Socket
import Data.List
import Control.Concurrent
import Control.Concurrent.MVar
import Data.Time.Clock as T
import BitSwapSpec as BSS
import BitSwapProtocol as BSP
import IPFSSpec as IPFS
import qualified Data.ByteString as BS

-- type MsgHandlerFn = Maybe (ID NodeID) -> MVar BSNodeState -> String -> IO ()

serverPort = "1314"

runServer port = 
	do 	addrinfos <- getAddrInfo (Just (defaultHints {addrFlags = [AI_PASSIVE]})) Nothing (Just port)
		let serveraddr = head addrinfos
		masterSock <- socket (addrFamily serveraddr) Stream defaultProtocol
		bind masterSock (addrAddress serveraddr)
		listen masterSock 5
		-- Create a lock to use for synchronizing access to the handler
		lock <- newMVar ()
		sharedState <- newMVar emptyBSNodeState
		-- sharedState <- putMVar sharedState (emptyBSNodeState) 
		-- Loop forever waiting for connections. Ctrl-C to abort.
		procRequests lock sharedState masterSock

	where
		-- | Process incoming connection requests
		procRequests :: MVar () -> MVar BSNodeState -> Socket -> IO ()
		procRequests lock sharedState mastersock=
			do 	(childSock, clientaddr) <- accept mastersock
				Main.log $ "Client Connected : " ++ show clientaddr
				forkIO $ procMessages lock sharedState childSock clientaddr
				procRequests lock sharedState mastersock

		-- | Process incoming messages		
		procMessages :: MVar () -> MVar BSNodeState -> Socket -> SockAddr -> IO ()
		procMessages lock sharedState childSock clientaddr =
		
			do 	connhdl <- socketToHandle childSock ReadMode
				hSetBuffering connhdl LineBuffering
				messages <- hGetContents connhdl
				mapM_ (handle lock sharedState Nothing clientaddr) (lines messages)
				hClose connhdl
				Main.log $ "syslogtcpserver.hs: client disconnected" ++ show clientaddr
		
		-- -- Lock the handler before passing data to it.
		-- handle :: MVar () -> HandlerFunc
		-- -- This type is the same as
		handle :: MVar () -> MVar BSNodeState -> Maybe (ID NodeID) -> SockAddr -> String -> IO ()
		handle lock sharedState pID clientaddr msg =
			withMVar lock (\a -> msgHandler sharedState clientaddr msg pID >> return a)
	
-- A simple handler that prints incoming packets
msgHandler :: 	MVar BSNodeState -- Global BSNode state
			->  SockAddr  		-- Client address
			-> String 			  -- Msg received
			-> Maybe (ID NodeID) -- Msg's owner PeerID
			-> IO (ID NodeID)			

-- Handshake Handlling
msgHandler state clientaddr msg Nothing = 
	do 	putStrLn $ "Processing " ++ show clientaddr
		-- Fetch NodeState
		oldstate <- readMVar state
		let OpenMsg receivedLeger = (read msg) :: Msg
		-- Get Modified NodeState
		newstate <- handleReceivedLedger oldstate receivedLeger
	 	-- Put newstate back
	 	swapMVar state newstate
		let pId = peerID_l receivedLeger
		-- Return pId
		return pId
	where
		handleReceivedLedger oldstate receivedLeger = 
			do  
				let pId = nodeID_l receivedLeger
				let nId = nodeID_b oldstate
				let archivedLedger = fetchLedgerFromArchive oldstate pId

				finalLedger <- 	if archivedLedger == (Just receivedLeger) then (return receivedLeger)
								else newLedger nId pId
				let msgQ = 	if archivedLedger /= (Just receivedLeger) then 
								[ ResetLedgerMsg finalLedger]	
							else []
				let peerInfo = Peer pId finalLedger (timeStamp finalLedger) [] [] msgQ
				let newstate = oldstate { activePeers = M.insert pId peerInfo (activePeers oldstate) }									 			
				putStrLn $ show newstate
				return newstate

msgHandler state clientaddr msg (Just pId) = 
	do  putStrLn $ "Processing " ++ show clientaddr
	    Main.log msg
	    return BS.empty


log :: String -> IO ()
log msg = 
	do 	time <- T.getCurrentTime
		putStrLn $ show time ++ " : " ++ msg


main = 	runServer serverPort