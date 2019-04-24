module IPFSSpec where

import Data.Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as DBC
import Crypto.Hash.SHA256 (hash)
import Data.Word (Word8)
-- import MultiFormats as MF --Link with old project
import Data.ByteString.Base58 (encodeBase58, decodeBase58, bitcoinAlphabet)


------------ Constants ---------------------------
-- Min no of preceding 0 bits required in hash
-- of NodeID
-- Prevent malicious user from keep changing NodeIDs
difficulty = 4
--------------------------------------------------


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


-- TODO: Search Function for it
genKeyPair :: IO (BS.ByteString, BS.ByteString)
genKeyPair = return (BS.empty, BS.empty)

-- Returns pubKey,priKey, NodeID which has POW with 'difficulty'
getNodeIDConfig :: IO (BS.ByteString, BS.ByteString,ID NodeID)

getNodeIDConfig =
	do 	
		(pubKey, priKey) <- genKeyPair
		let nodeID_ = hash pubKey
		let nZeros = findNoOfPrecedingZeroBits $ BS.unpack (hash nodeID_)
		-- If nZeros are less than difficulty, then another PKI pair
		if nZeros < difficulty then
			getNodeIDConfig
		-- Else return current KeyConfig
		else
			return (pubKey, priKey, nodeID_)

------------------------
-- generateNodeID :: BS.ByteString

--	| Finds no of preceding zero bits in [Word8]
-- For ByteString, use unpack
findNoOfPrecedingZeroBits :: [Word8] -> Int

findNoOfPrecedingZeroBits [] = 0
findNoOfPrecedingZeroBits (x:xs)	
	| n == 8 = 8 + (findNoOfPrecedingZeroBits  xs)
	| otherwise = n
		
	where n = countPrecedingZeros x

countPrecedingZeros :: Word8 -> Int
countPrecedingZeros word
	| testBit word 7 = 0
	| testBit word 6 = 1
	| testBit word 5 = 2
	| testBit word 4 = 3
	| testBit word 3 = 4
	| testBit word 2 = 5
	| testBit word 1 = 6
	| testBit word 0 = 7
	| otherwise 	 = 8




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
