import Data.Time.Clock as CLK
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
