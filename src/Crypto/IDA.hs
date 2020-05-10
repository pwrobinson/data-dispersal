-----------------------------------------------------------------------------
-- |
-- Module      :  Crypto.IDA
-- Copyright   :  Peter Robinson 2014
-- License     :  LGPL
--
-- Stability   :  experimental
-- Portability :  portable
--
-- This module provides an (m,n)-information dispersal scheme that provides
-- data redundancy while preserving secrecy.
-- In other words, this module combines the best of 2 worlds: secret sharing
-- algorithms with low-overhead information dispersal.
--
-- Function 'encode' splits a given bytestring into @n@ fragments with the
-- following properties:
--
-- 1. Any @m@ of the @n@ fragments are sufficient for reconstructing the original
-- bytestring via 'decode', and
-- 2. the knowledge of up to @m-1@ fragments does /not/ leak any information
-- about the original bytestring.
--
-- In more detail, suppose that we have some bytestring @b@ that we want to
-- (securely) disperse and parameter @m@, @n@.
-- Running 'encode' @m n b@ does the following:
--
-- * Generate a randomly chosen key of 32 bytes, called @key@.
-- * Encrypt the bytestring @b@ using @key@ via AES.
-- * Generate @n@ shares using the perfect secret sharing algorithm implemented
-- in module "Crypto.SecretSharing"; see package <http://hackage.haskell.org/package/secret-sharing>secret-sharing
-- * Generate @n@ fragments of the encrypted data using the information
-- dispersal algorithm in "Data.IDA".
-- * Finally, we pair up these shares and fragments as
-- a list of 'EncryptedFragment's.
--
-- The size of each encrypted fragment is @O(|b|\/m + |key|)@.
-- For sufficiently large bytestrings, the @O(|b|\/m)@ factor dominates and thus
-- the scheme is space-optimal.
--
-- The secret sharing algorithm guarantess that the knowledge of up to @m-1@ of
-- the fragments does not leak any information about the encryption key (and
-- hence the encrypted data).
--
-----------------------------------------------------------------------------
{-# LANGUAGE DeriveDataTypeable, ScopedTypeVariables, DeriveGeneric #-}
module Crypto.IDA( EncryptedFragment(keyShare,aesIV,fragment)
                 , encode
                 , encodeWithIV
                 , decode
                 )
where
import Data.IDA.Internal( Fragment(theContent))
import qualified Data.IDA.Internal as IDA

import Crypto.SecretSharing( Share )
import qualified Crypto.SecretSharing as PSS

import Data.ByteString.Lazy( ByteString )
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import System.Entropy( getEntropy )
import Codec.Crypto.AES
import Control.Exception
import Data.Typeable
import Data.Binary( Binary )
import GHC.Generics

data EncryptedFragment = EncryptedFragment
  { keyShare  :: Share       -- ^ the list of (bytewise) shares of the AES key
  , aesIV      :: B.ByteString  -- ^ the initialization vector of the AES encryption
  , fragment   :: Fragment      -- ^ the encrypted fragment of the original data
  }
  deriving(Typeable,Eq,Generic)


instance Show EncryptedFragment where
  show f = show (keyShare f,theContent $ fragment f)

instance Binary EncryptedFragment


aesKeyLength, aesIVLength :: Int
aesKeyLength = 32
aesIVLength  = 16

-- | Space efficient and secrecy-preserving (m,n)-information dispersal:
-- Generates @n@ fragments out
-- of a given bytestring @b@. Each fragment has size @length b \/ m + O(1)@.
-- At least m fragments are required for reconstruction.
-- Preserves secrecy: The knowledge of less than m
-- fragments provides /no/ information about the original data whatsoever.
encode :: Int              -- ^ m: number of fragments required for reconstruction
       -> Int                    -- ^ n: total number of fragments (@n ≥ m@)
       -> ByteString             -- ^ the information that we want to disperse
       -> IO [EncryptedFragment] -- ^ a list of n encrypted fragments.
encode m n msg = encode' m n Nothing msg


-- | Same as 'encode' but uses an initialization vector for the AES encryption.
encodeWithIV :: Int        -- ^ m: number of fragments required for reconstruction
             -> Int        -- ^ n: total number of fragments (@n ≥ m@)
             -> ByteString -- ^ the initialization vector for the AES encryption
             -> ByteString -- ^ the information that we want to disperse
       -> IO [EncryptedFragment] -- ^ a list of n encrypted fragments.
encodeWithIV m n iv msg = encode' m n (Just iv) msg


encode' :: Int              -- ^ m: number of fragments required for reconstruction
        -> Int              -- ^ n: total number of fragments (@n ≥ m@)
        -> Maybe ByteString -- ^ the initialization vector for the AES encryption.
                            --   If none is given, we create a random one.
        -> ByteString       -- ^ the information that we want to disperse
        -> IO [EncryptedFragment] -- ^ a list of n encrypted fragments.
encode' m numFragments mIV msg = do
  key <- getEntropy aesKeyLength
  iv  <- maybe (getEntropy aesIVLength) (return . BL.toStrict) mIV
  keyShareList <- PSS.encode m numFragments (BL.fromStrict key)
  let headers = zip keyShareList (replicate numFragments $ BL.fromStrict iv)
  let fs = IDA.encode m numFragments $  BL.toStrict $ crypt CTR key iv Encrypt msg
  return [ EncryptedFragment ks (BL.toStrict iv') f
         | ((ks,iv'),f) <- zip headers fs
         ]

-- | Reconstruct the original data from (at least) @m@ fragments.
-- Throws an 'AssertionFailed' exception if an insufficient number fragments are
-- given or if a decoding error occurs.
decode :: [EncryptedFragment]
       -> ByteString
decode [] = BL.pack []
decode pss@(p:_)
  | length pss < IDA.reconstructionThreshold (fragment p) = throw $ AssertionFailed
      "decode: not enough fragments for reconstruction."
  | otherwise =
  let m    = IDA.reconstructionThreshold $ fragment p in
  let efs  = take m pss in
  let iv   = aesIV p in
  let fs   = map fragment efs in
  let ss   =  map keyShare efs in
  let emsg = IDA.decode fs in
  let key  = PSS.decode ss in
  crypt CTR (BL.toStrict key) iv Decrypt $ BL.fromStrict emsg
