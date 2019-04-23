import Data.Bits
import Network.Socket
import Data.List
import System.IO
import IPFSSpec as IPFS
import qualified Data.ByteString.Char8 as DBC
import qualified Data.ByteString as BS
import BitSwapSpec

------------------------------------------ TESTING CONFIG ---------------------------------------

-- BitSwap Server HostName
hostname = "localhost"
-- BitSwap Server Port
port = "1314"

-- IPFSObj For Testing
objs =
  [ IPFSObj (DBC.pack "kQ7qwL8kGhVLB1WVFXxR") []
  , IPFSObj (DBC.pack "YCHKLFNf6Q2PTtzBUkdu") []
  , IPFSObj (DBC.pack "frSc4P5V73TlbWx9VhTu") []
  , IPFSObj (DBC.pack "rmxR8fJvs8eWiPnesOMt") []
  ]

-- WantListMsg, that declares removal
myWantListMsg_sub = WantListMsg ( map (\x -> Entry (getCIDv0 x) High True ) (Main.objs ))
-- WantListMsg, that declares addition
myWantListMsg_add = WantListMsg ( map (\x -> Entry (getCIDv0 x) High False ) (Main.objs ))

-- BlockMsgs
myBlockMsgs = map (\x -> BlockMsg (getCIDv0 x) x) (Main.objs)

------------------------------------------ BitSwap Client -------------------------------------

-- 
send hostname port msg =
	do
		addrinfos <- getAddrInfo Nothing (Just hostname) (Just port)
		let serveraddr = Data.List.head addrinfos
		sock <- socket (addrFamily serveraddr) Stream defaultProtocol
		setSocketOption sock KeepAlive 1
		connect sock (addrAddress serveraddr)
		h <- socketToHandle sock WriteMode
		hSetBuffering h (LineBuffering)
		System.IO.hPutStrLn h msg
		hFlush h
		hClose h

--  Delimiter is added at appropriate locations
series [] = ""
series msgs = foldr (\x y -> (x ++ "\n" ++ y) ) "" (map show msgs)


----------------------------------------------- MAIN ------------------------------------------

main = 
    do  -- Ledger
        l <- newLedger (BS.empty) (BS.empty)
        -- OpenMsg
        let myOpenMsg = OpenMsg l
        -- Series of Msgs that will be sent
        let msgs = series ([ myOpenMsg, myWantListMsg_add] ++ myBlockMsgs) 
        -- Print Joined Messages Locally
        putStrLn msgs
        -- Send to remote BitSwap Server
        send hostname port msgs
        return ()

----------------------------------------------------------------------------------------------