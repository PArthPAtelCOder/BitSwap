import Data.Time.Clock as CLK

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