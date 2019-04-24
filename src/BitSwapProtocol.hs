module BitSwapProtocol where

import BitSwapSpec as BSSpec
import BitSwapSpec (  )
import IPFSSpec

---- Debt ratio & Prob calculation is the heart of protocol
---- Implementation specific

findDebtRatio ::    BSSpec.Ledger   -- Ledger of Peer
                ->  Double          -- peer's Debt ratio

findDebtRatio ledger =  
    let 
        rcvd = BSSpec.bytesRcvd ledger
        rcvd_dbl = (fromIntegral rcvd) :: Double
        sent = BSSpec.bytesSent ledger
        sent_dbl = (fromIntegral sent) :: Double
    in
        sent_dbl/(1+rcvd_dbl)

findProbOfSending ::    BSSpec.Ledger   -- Ledger of Peer
                    ->  Double          -- Probability

findProbOfSending ledger =
    let debtRatio = findDebtRatio ledger
    in
        1 - ( 1/(1+exp(6 - 3*debtRatio) ))


------------------------------------------------------------
