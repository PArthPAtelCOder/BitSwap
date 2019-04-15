module IPFSSpec where


import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as DBC
import Crypto.Hash.SHA256 (hash)
import Data.Word (Word8)
-- import MultiFormats as MF --Link with old project
import Data.ByteString.Base58 (encodeBase58, decodeBase58, bitcoinAlphabet)


data CID 	= 	CIDv0 BS.ByteString -- always starts with Qm
									-- CIDv0 is base58 encoded sha256 hash of data
									-- cidv0 ::= <multihash-content-address>
			|	CIDv1 BS.ByteString

		deriving ( Show, Read, Eq, Ord )


data IPFSLink = IPFSLink 
				{
					name :: String  ,
					size :: Integer ,
					cid :: CID
				} 

				deriving ( Show, Read, Eq )


data IPFSObj = 	IPFSObj
				{
					content :: BS.ByteString,
					links :: [ IPFSLink ]
				}

				deriving ( Show, Read, Eq )

-- A phantom type to represent ID
type ID a = BS.ByteString

data NodeID

data Key

-- | Temp MultiHash implmentation for Sha-256 encoding

hashSHA256_MH 	:: 	BS.ByteString 	-- Content
				->	BS.ByteString 	-- SHA-256 digest with prfix 0x12

hashSHA256_MH content 	=	BS.append prefix digest
	
	where
		prefix 		= BS.pack [18::Word8, 32::Word8] -- 18:Sha-256, 32:length of digest
		digest 		= hashSHA256 content
		hashSHA256 	= hash


-- | Returns CIDv0 for IPFSObj
getCIDv0 ::		IPFSObj
			-> 	CID 		--	calcualted CID, always starts with Qm

getCIDv0 obj =	CIDv0 cid

	where
		content = DBC.pack $ show obj	
		cid = encodeBase58 bitcoinAlphabet $ hashSHA256_MH content

-- | Checks integrity of received IPFSObj
verifyCID	::	IPFSObj		-- Received IPFSObj
			->	CID 		-- Received CID
			->	Bool		-- Result

verifyCID obj cid_@(CIDv0 _ )	=	cid_ == getCIDv0 obj
verifyCID obj cid_@(CIDv1 _ )	=   False -- TODO




















-- -- Returns CIDv1 for data
-- -- <cidv1> ::= <multibase-prefix><cid-version><multicodec-content-type><multihash-content-address>
-- getCIDv1 ::		IPFSObj
-- 			->	HashFunction  	-- Hash function
-- 			->	CodecType	  	-- Codec representing content serialization
-- 			->	EncodingScheme 	-- Encoding to be used for CID
-- 			-> 	CID 			-- calcualted CIDv1

-- getCIDv1 content hf ct es 	= 	CIDv1 cid
-- 							where
-- 								-- TODO: replace with own implementation
-- 								cidWithoutEncoding = multiCodec ct . multiHash hf content
-- 								cid = multiBase es (byte 1 + cidWithoutEncoding)
