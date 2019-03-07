module BitSwapSpec where

import Data.Time.Clock as T
import qualified Data.HashTable as H
import qualified Data.ByteString as BS
import IPFSSpec


-- Questions
-- 1) How to handle different version of CIDs as a Key

-- TODO: implement VarInt


	-- difficulty
	-- alpha
	-- k

--------- CONSTANTS --------

-- Blocked peer is ignored for ignore_cooldown (in sec) period
-- Default = 10s
ignore_cooldown = 10


-- connection that is idle for silence_wait (in sec) is closed
-- Defalult = 30s
silence_wait = 30 

----------------------------


-- State of Node running BitSwap Protocol
data BSNodeState = BSNodeState
			{
				ledgers :: H.HashTable (ID NodeID) (Ledger),
				activePeers :: H.HashTable (ID NodeID) Peer,
				nodeWantList :: WantList,
				haveList :: H.HashTable (ID NodeID) IPFSSpec.IPFSObj,
				blockList :: [ ID NodeID ]
			}

isPeerBlocked :: 	ID NodeID -- Peer ID
			->	BSNodeState   -- State of Node
			->	Bool

isPeerBlocked peerID state = elem peerID $ blockList state

data WantList 	=	WantList 
				{
					lastUpdated		::	T.UTCTime ,
					cids 			:: 	[ CID ]
				} deriving (Show, Read)

-- Msg Formats
data Msg = 	WantListMsg	WantList
		|	OpenMsg 	Ledger
		|	BlockMsg 	CID 	IPFSSpec.IPFSObj
			deriving (Show, Read, Eq)

-- Peer Information
data Peer = Peer
		{
			peerID :: ID NodeID,
			ledger :: Ledger,
			-- timestamp of last succesful message communication
			-- Used for connection timeout
			lastSeen :: T.UTCTime,
			peerWantList :: WantList,
			sendList :: [ CID ], -- CIDs of Block that we have
			msgQueue :: [ Msg ], -- Messages to be sent, TODO: chan?
		}


-- Used to track Peer Credit
data Ledger = Ledger
			{
				nodeID :: ID NodeID,
				peerID :: ID NodeID,
				bytesRcvd :: Integer,
				bytesSent :: Integer,
				timeStamp :: UTCTime -- timestamp of last succesful block message communication
			}

-- TODO: add logic for timeStamp comparision
-- checking for equality is not good
-- It should be difference < threshold
instance Eq Ledger where
	(==) x y = 	if 	(nodeID x) == (peerID y) &&
					(nodeID y) == (peerID x) &&
					(bytesSent x) == (bytesRcvd y) &&
					(bytesSent y) == (bytesRcvd x) &&	then True
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

newLedger peerID_ nodeID_ = Ledger ( nodeID_, peerID_, 0, 0, T.getCurrentTime )import Data.Time.Clock as CLK
import qualified Data.HashTable as H
import qualified Data.ByteString as BS


-- CONSTANTS to be added
	-- ignore_cooldown
	-- difficulty
	-- alpha
	-- k
	-- silence_wait = 30 default

-- Confirm ways to handle constants
-- Using type class
-- directly as a variable


-- Questions
-- 1) How to handle different version of CIDs as a Key

-- TODO: implement VarInt



-- State of Node running BitSwap Protocol
data BitSwap = BitSwap
			{
				ledgers :: H.HashTable (ID NodeID) (Ledger),
				activePeers :: H.HashTable (ID NodeID) Peer,
				wantList :: [ CID ],
				haveList :: H.HashTable (ID NodeID) IPFSObj
				-- TODO: blockList
			}

-- Information related to peer
data Peer = Peer
		{
			peerID :: ID NodeID,
			ledger :: Ledger,
			-- timestamp of last succesful message communication
			-- Used for connection timeout
			lastSeen :: IO UTCTime,
			wantList :: [ CID ]
		}


data Ledger = Ledger
			{
				nodeID :: ID NodeID,
				peerID :: ID NodeID,
				bytesRcvd :: Integer,
				bytesSent :: Integer,
				timeStamp :: IO UTCTime -- timestamp of last succesful block message communication
			}
-- TODO: add logic for timeStamp comparision
-- checking for equality is not good
-- It should be difference < threshold
instance Eq Ledger where
	(==) x y = 	if 	(nodeID x) == (peerID y) &&
					(nodeID y) == (peerID x) &&
					(bytesSent x) == (bytesRcvd y) &&
					(bytesSent y) == (bytesRcvd x) &&	then True
				else
					False


-- Resets Ledger
resetLedger :: 		Ledger -- Old Ledger
				-> 	Ledger -- New Ledger

resetLedger ledger = ledger {
								bytesRcvd = 0,
								bytesSent = 0,
								timeStamp = CLK.getCurrentTime
							}

-- Create en empty ledger
newLedger ::	ID NodeID  -- Node ID
			-> 	ID NodeID  -- Peer ID
			->	Ledger     -- New Ledger

newLedger peerID_ nodeID_ = Ledger ( nodeID_, peerID_, 0, 0, CLK.getCurrentTime )
