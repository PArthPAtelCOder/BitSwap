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

---------------------------------------- BitSwap Server -----------------------------------------------

serverPort = "1314"

-- |  Run BitSwap server at given port
runServer port = 

    do  -- Get available interface
        addrinfos <- getAddrInfo (Just (defaultHints {addrFlags = [AI_PASSIVE]})) Nothing (Just port)
        -- Using first return address
        let serveraddr = head addrinfos
        -- Get TCP Mastersocket
        -- Welcoming Socket, upon connection it creates child connections
        masterSock <- socket (addrFamily serveraddr) Stream defaultProtocol
        -- Binf master socket at asked port
        bind masterSock (addrAddress serveraddr)
        -- Listen for incoming connections, atMax 10 waiting connections
        listen masterSock 10
        -- Create a lock to use for synchronizing access to the handler
        -- Used just like a Mutex
        lock <- newMVar ()
        -- Stores BitSwap's central State
        -- Shared between all chilc sockets
        sharedState <- newMVar emptyBSNodeState
        -- Loop forever waiting for connections.
        procRequests lock sharedState masterSock

    where
        -- | Handles incoming connection requests
        procRequests :: MVar () -> MVar BSNodeState -> Socket -> IO ()
        procRequests lock sharedState mastersock=
            do  (childSock, clientaddr) <- accept mastersock
                Main.log $ "Client Connected : " ++ show clientaddr
                -- Each child socket runs on different thread
                forkIO $ procMessages lock sharedState childSock clientaddr
                procRequests lock sharedState mastersock

        -- | Process incoming messages from child socket   
        procMessages :: MVar () -> MVar BSNodeState -> Socket -> SockAddr -> IO ()
        procMessages lock sharedState childSock clientaddr =
        
            do  -- It's convenient to use socket as Handles
                connhdl <- socketToHandle childSock ReadMode
                -- Set lineBuffering
                hSetBuffering connhdl LineBuffering
                -- Lazy Reading of Socket
                messages <- hGetContents connhdl
                -- Messages are seperated by lineCharacter
                -- On each message handle function is called
                -- Which internally uses msgHandler function
                -- TODO: Split using ustom msgDelimiter
                mapM_ (handle lock sharedState (Just BS.empty) clientaddr) (lines messages)
                hClose connhdl
                Main.log $ "client disconnected : " ++ show clientaddr
        
        -- | used to process message from different sockets concurrently
        --  withMVar functions set's old value of lock back if msgHandler throws an exception
        handle :: MVar () -> MVar BSNodeState -> Maybe (ID NodeID) -> SockAddr -> String -> IO ()
        handle lock sharedState pID clientaddr msg =
            withMVar lock (\a -> msgHandler sharedState clientaddr msg pID >> return a)
  

----------------------------------- MSG HANDLING --------------------------------------------------

-- MsgHandler function
-- Handles OpenMsg (handShake), WantListMsg, BlockMesg, ResetLedgerMsg
msgHandler ::   MVar BSNodeState    -- Global BSNode state
            ->  SockAddr            -- Client address, for logging purpose
            -> String               -- Received Msg
            -> Maybe (ID NodeID)    -- Msg's owner PeerID
            -> IO (ID NodeID)       -- Planned to use in managing future msgs (after handShake)

-- | handles HandShake
-- During HandShake PeerID is not known to childSocket so
-- pID : Nothing
msgHandler state clientaddr msg Nothing = 
    do  Main.log $ "Handshake with " ++ show clientaddr
        -- Reads BSNode State
        -- Not using takeMVar, so that if exception occurs
            -- it doesn't cause problems in synchronisation
            -- withMVar provides atomic handling of msg
        oldstate <- readMVar state
        -- Parse received msg as a OpenMsg
        let OpenMsg receivedLeger = (read msg) :: Msg
        -- Get Modified NodeState
        newstate <- handleReceivedLedger oldstate receivedLeger
        -- refresh state atomically
        swapMVar state newstate
        -- Return pID received from Ledger
        let pId = peerID_l receivedLeger
        return pId

    where
        handleReceivedLedger oldstate receivedLeger = 
            do  -- PeerID
                let pId = nodeID_l receivedLeger
                -- NodeID
                let nId = nodeID_b oldstate
                -- Gets Ledger from archived ledgers, if exist
                    -- Maybe Ledger
                let archivedLedger = fetchLedgerFromArchive oldstate pId
                -- If archived Ledger matches with received Ledger, then use it
                --  else reset Ledger
                finalLedger <-  if archivedLedger == (Just receivedLeger) then (return receivedLeger)
                                else newLedger nId pId
                --  add ResetLedgerMsg in msgQueue if Ledgers doesn't match
                let msgQ =  if archivedLedger /= (Just receivedLeger) then 
                                [ ResetLedgerMsg finalLedger]   
                            else []
                -- prepare Peer DS 
                let peerInfo = Peer pId finalLedger (timeStamp finalLedger) [] [] msgQ
                let newstate = oldstate { activePeers = M.insert pId peerInfo (activePeers oldstate) }                                              
                putStrLn $ show newstate
                
                -- Return new BSState
                return newstate

-- Handles WantListMsg, BlockMsg, ResetLedgerMg
-- After Handshake, childScoket knows peerID
-- so it is passed to function
msgHandler state clientaddr msg (Just pID) =

    do  -- Log
        putStrLn logmsg
        -- Read Global BSState
        oldstate <- readMVar state
        -- Based on type of Msg handle accordingly
        newstate <- case m of
                    ------------------- WantListMsg Handling -------------------------
                    WantListMsg wl -> processReceivedWantList oldstate wl pID
                    
                    ------------------- BlockMsg Handling -------------------------
                    BlockMsg cid_ obj
                        -- CID matches IPFSObj and we want this obj than handle it
                        | integrity && Data.List.elem cid_ (nodeWantList oldstate)  ->
                                handleReceivedBlock oldstate cid_ obj

                        -- Else discard it
                        -- Data might be currupted or some other reason
                        | otherwise -> return oldstate

                            where integrity = verifyCID obj cid_

                    ------------------- ResetLedger Handling -------------------------
                    ResetLedgerMsg l -> return oldstate

                    otherwise -> return oldstate

        -- Update Global BSState
        swapMVar state newstate
        return pID

    where
        logmsg = "\n\nIn other fn Processing " ++ show clientaddr
        -- Parsed Msg
        m = (read msg) :: Msg

        -- According to the Entry received from peer (pID)
            --  updates BSState
        processEntry pID oldstate entry =
            let -- Collect old values
                activePeers_ = activePeers oldstate        
                Just oldPeerInfo = M.lookup pID activePeers_
                oldWantList = peerWantList oldPeerInfo
                oldSendList = sendList oldPeerInfo
                oldMsgQueue = msgQueue oldPeerInfo
                nodeHaveList = haveList oldstate
                -- CID corresponding to the Entry
                cid_ = BSS.cid entry
                presense = M.lookup cid_ nodeHaveList

                newMsgQueue
                    | not (cancel entry) && presense /= Nothing = let Just obj = presense
                                                                in  (BlockMsg cid_ obj):oldMsgQueue
                    | otherwise = oldMsgQueue 

                -- Updates Peer's Local SendList
                newSendList 
                        -- If Obj is no more needed, remove it
                    | cancel entry = (Data.List.delete cid_ oldSendList)
                        -- If CID is already present in SendList
                    | Data.List.elem cid_ oldSendList = oldSendList
                        -- If CID is not present in sendList & Node has corresponding
                        -- Data then Entry to sendList
                    | presense /= Nothing = cid_:oldSendList
                    | otherwise = oldSendList

                -- Update Peer's Local WantList                    
                newWantList
                        -- If obj is no more needed, remove it
                    | cancel entry = (Data.List.delete cid_ oldWantList)
                        -- Obj is needed & already present in wantList
                    | Data.List.elem cid_ oldWantList = oldWantList
                        -- Obj is needed & not present in wantList then add it
                    | otherwise = cid_:oldWantList
                
                newPeerInfo = oldPeerInfo { peerWantList = newWantList, sendList = newSendList }
            in      --  Return update BSState
                oldstate { activePeers = M.insert pID newPeerInfo activePeers_ }

            -- Process WantList (List of Entries)
        processReceivedWantList oldstate wl pID
            -- Peer is activePeer
         | M.member pID activePeers_ = 
             do -- Update last_seen of Peer
                time <- T.getCurrentTime
                let Just oldPeerInfo = M.lookup pID activePeers_
                    tmpPeerInfo = oldPeerInfo { lastSeen = time }
                    tmpState = oldstate { activePeers = M.insert pID tmpPeerInfo activePeers_ }
                    -- Updated BSState
                        --  Each entry is processed one by one, left to right
                        --  Final state is returned
                    newState = foldl (processEntry pID) tmpState wl

                return newState
            
            -- Peer is not avtivePeer
         | otherwise = return oldstate
            
            where   activePeers_ = activePeers oldstate

        -- Make necessary changes in BSState when Block is received
        handleReceivedBlock oldstate cid_ obj = 
            do  -- Update Peer's Last Seen
                time <- T.getCurrentTime
                let activePeers_ = activePeers oldstate
                    Just oldPeerInfo = M.lookup pID activePeers_
                    newPeerInfo = oldPeerInfo { lastSeen = time }
                    newActivePeers = M.insert pID newPeerInfo activePeers_
                    -- Fetch current Block Storage
                    nodeHaveList = haveList oldstate
                    -- Insert received Block
                    newHaveList = M.insert cid_ obj nodeHaveList
                    -- Whenever a newBlockReceive we have to tell peer to not send it anymore
                        -- If we receive new request we have to tell peer for that request
                        -- So instead of sending whole WantList
                        -- WantList corresponding to recent changes is accumulated
                        -- sent to all active peers
                    WantListMsg oldWantListEntries = currentWantListMsg oldstate
                    -- This block is no more Need, so add Entry with cancel = True
                    newEntry = Entry cid_ Normal True
                    newWantListMsg = WantListMsg $ newEntry:oldWantListEntries        
                
                -- Return updated BSState
                return oldstate {   
                        haveList = newHaveList, 
                        currentWantListMsg = newWantListMsg,
                        activePeers = newActivePeers }

-- To Print Logs
log :: String -> IO ()
log msg = 
    do  time <- T.getCurrentTime
        putStrLn $ show time ++ " : " ++ msg

--------------------------------------------- MAIN --------------------------------------------
main =  runServer serverPort
-----------------------------------------------------------------------------------------------