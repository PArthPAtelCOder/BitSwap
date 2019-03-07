module BitSwapProtocol where

import BitSwapSpec as BSSpec
import IPFSSpec

---- Debt ratio & Prob calculation is the heart of protocol
---- Implementation specific

findDebtRatio :: 	BSSpec.Ledger 	-- Ledger of Peer
				->	Double			-- peer's Debt ratio

findDebtRatio ledger =	
	let rcvd = BSSpec.bytesRcvd ledger
		sent = BSSpec.bytesSent ledger
	in
		sent/(1+rcvd)

findProbOfSending ::	BSSpec.Ledger 	-- Ledger of Peer
					->	Double			-- Probability

findProbOfSending ledger =
	let debtRatio = findDebtRatio ledger
	in
		1 - ( 1/(1+exp(6 - 3*debtRatio) ))

------------------------------------------------------------