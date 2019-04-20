import Data.Bits
import Network.Socket
import Data.List
import System.IO
import IPFSSpec as IPFS
import qualified Data.ByteString.Char8 as DBC
import qualified Data.ByteString as BS
import BitSwapSpec

hostname = "localhost"
port = "1314"
obj = IPFSObj (DBC.pack "kQ7qwL8kGhVLB1WVFXxR") []

myCid = getCIDv0 obj

send hostname port msg =
	do
		addrinfos <- getAddrInfo Nothing (Just hostname) (Just port)
		let serveraddr = Data.List.head addrinfos
		sock <- socket (addrFamily serveraddr) Stream defaultProtocol
		setSocketOption sock KeepAlive 1
		connect sock (addrAddress serveraddr)
		h <- socketToHandle sock WriteMode
		hSetBuffering h (BlockBuffering Nothing)
		System.IO.hPutStrLn h msg
		-- Make sure that we send data immediately
		hFlush h
		hClose h


main = 
	do
		l <- newLedger BS.empty BS.empty
		send hostname port $ show (OpenMsg l) ++ "\n"