import qualified Data.ByteString as BS

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