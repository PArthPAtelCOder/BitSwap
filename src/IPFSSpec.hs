import qualified Data.ByteString as BS
import MultiFormats as MF --Link with old project

data CID = 	CIDv0 String -- always starts with Qm
		|	CIDv1 String

data IPFSLink = IPFSLink
				{
					name :: String,
					size :: Integer,
					cid :: CID
				}

data IPFSObj = 	IPFSObj
				{
					content :: BS.ByteString,
					links :: [ IPFSLink ]
				}


-- Returns CIDv0 for content
-- CIDv0 is base58 encoded sha256 hash of data
-- cidv0 ::= <multihash-content-address>
getCIDv0 ::		BS.ByteString -- content
			-> 	CID -- calcualted CID, always starts with Qm

getCIDv0 content =	CIDv0 cid
			where
				cid = MF.multiBase "base58" . MF.multiHash "SHA256" content -- TODO: replace with own implementation
