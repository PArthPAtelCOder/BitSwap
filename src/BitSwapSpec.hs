module BitSwapSpec where

import Data.Time.Clock as T
import qualified Data.Map as M
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as DBC
import IPFSSpec as IPFS
import IPFSSpec (Key, ID, NodeID)
-- import IPFSSpec 


-- Questions
-- 1) How to handle different version of CIDs as a Key

-- TODO: implement VarInt

-- difficulty
-- alpha
-- k

--------- CONSTANTS ------------------------------------------

-- Blocked peer is ignored for ignore_cooldown (in sec) period
-- Default = 10s
ignore_cooldown = 10

-- connection that is idle for silence_wait (in sec) is closed
-- Defalult = 30s
silence_wait = 30

-- delimiter to decide Msg's Boundary
msgDelimiter = "\r\n\r\n"
--------------------------------------------------------------
			-- Testing Data --
--------------------------------------------------------------
objs =
  [ IPFSObj (DBC.pack "kQ7qwL8kGhVLB1WVFXxR") []
  , IPFSObj (DBC.pack "YCHKLFNf6Q2PTtzBUkdu") []
  , IPFSObj (DBC.pack "frSc4P5V73TlbWx9VhTu") []
  , IPFSObj (DBC.pack "rmxR8fJvs8eWiPnesOMt") []
  ]

storage = Prelude.foldl f M.empty objs
	where
		f x y = insert (getCIDv0 y) y x
--------------------------------------------------------------

data Priority = Low
            |   Normal
            |   High
            
            deriving (Enum, Ord, Eq, Show, Read)

data Entry = Entry {
                    cid :: IPFS.CID ,	-- CID of content
                    priority :: Priority ,	-- Priority for Data Request, Higher Priority
                    cancel :: Bool			-- True: Content is no more needed
                    } deriving (Enum, Eq, Ord, Show, Read)

type WantList = [ IPFS.CID ]
type MsgQueue = [ Msg ]

-- User interface to add request to wantList
-- TODO     

-- Whenever a new modification function is called for wantList, wantListMsg is changed accordingly.



-- State of Node running BitSwap Protocol
data BSNodeState = BSNodeState
			{
				ledgers :: M.Map (IPFS.ID NodeID) (Ledger),
				activePeers :: M.Map (IPFS.ID NodeID) Peer,
				nodeWantList :: WantList,
				haveList :: M.Map IPFS.CID IPFS.IPFSObj,
				blockList :: [ IPFS.ID NodeID ],
				currentWantListMsg :: Msg
			}

emptyBSNodeState = BSNodeState M.empty M.empty [] storage [] (WantListMsg [])


isPeerBlocked :: 	ID NodeID -- Peer ID
			->	BSNodeState   -- State of Node
			->	Bool

isPeerBlocked peerID state = elem peerID $ blockList state


isContentAvailable :: 	IPFS.CID
					->	BSNodeState
					->	Bool

isContentAvailable cid state =
	let hl = haveList state
	in	M.member cid hl



-- Msg Formats
data Msg = 	WantListMsg [ Entry ]
		|	OpenMsg 	Ledger
		|	BlockMsg 	CID 	IPFS.IPFSObj
			
			deriving (Show, Read, Eq)

serializeMsg :: Msg -> BS.ByteString
serializeMsg msg = DBC.pack $ (show msg) ++ delimiter

deserializeMsg :: BS.ByteString -> Msg
deserializeMsg binaryMsg = Read $ DBC.unpack binaryMsg


-- Peer Information
data Peer = Peer
		{
			peerID_p :: ID NodeID,			

			ledger :: Ledger,				-- Latest Ledger of Peer
											-- May not be same as in Archive

			lastSeen :: T.UTCTime,			-- timestamp of last succesful message communication
											-- Used for connection timeout

			peerWantList :: WantList,
			sendList :: [ IPFS.CID ], 	-- CIDs of Blocks that we have
			
			msgQueue :: MsgQueue 			-- Messages to be sent, TODO: chan?
		}


-- Used to track Peer Credit
data Ledger = Ledger
			{
				nodeID :: ID NodeID,
				peerID_l :: ID NodeID,
				bytesRcvd :: Integer,
				bytesSent :: Integer,
				timeStamp :: UTCTime -- timestamp of last succesful block message communication
			}

-- TODO: add logic for timeStamp comparision
-- checking for equality is not good
-- It should be difference < threshold
instance Eq Ledger where
	(==) x y = 	if 		(nodeID x) == (peerID y) 
					&& 	(nodeID y) == (peerID x)
					&&  (bytesSent x) == (bytesRcvd y)
					&&  (bytesSent y) == (bytesRcvd x)	then True
				else
					False


-- Resets Ledger
resetLedger :: 		Ledger -- Old Ledger
				-> 	Ledger -- New Ledger

resetLedger ledger = ledger {
								bytesRcvd = 0,
								bytesSent = 0,
								timeStamp = T.getCurrentTime
							}

-- Create en empty ledger
newLedger ::	ID NodeID  -- Node ID
			-> 	ID NodeID  -- Peer ID
			->	Ledger     -- New Ledger

newLedger peerID_ nodeID_ = Ledger ( nodeID_, peerID_, 0, 0, T.getCurrentTime )

-- Returns as it is if exists else returns fresh ledger
fetchLedgerFromArchive :: BSNodeState -> ID NodeID -> Ledger

fetchLedgerFromArchive state pId = 
	let ledgers_ = ledgers state
		nId = nodeID state
		pLedger = M.lookup pId ledgers_
	in
		case pLedger of Nothing -> newLedger pId nId
			            Just l -> l