-----------------------------------------------------------------------------
-- |
-- Module      :  Data.IDA.Internal
-- Copyright   :  Peter Robinson 2014
-- License     :  LGPL
-- 
-- Maintainer  :  Peter Robinson <peter.robinson@monoid.at>
-- Stability   :  experimental
-- Portability :  portable
--
-----------------------------------------------------------------------------
{-# LANGUAGE DeriveDataTypeable, ScopedTypeVariables, DeriveGeneric #-}
module Data.IDA.Internal 
where
import Data.ByteString( ByteString )
import qualified Data.ByteString as B
import Data.Vector(Vector,(!))
import qualified Data.Vector as V
import Data.Array( array )
import qualified Data.Array as A
import Control.Exception
import Data.Typeable
import Data.Binary( Binary )
import GHC.Generics
import qualified Data.Matrix as M

import Data.IDA.FiniteField
import qualified Data.FiniteField.PrimeField as PF


   

-- | A fragment of the original data.
data Fragment = Fragment 
  { fragmentId  :: !Int          -- ^ index of this fragment 
  , trailLength :: !Int             -- ^ number of symbols added to the original message
  , reconstructionThreshold :: !Int -- ^ number of fragments required for reconstruction
  , theContent :: ![FField]         -- ^ the encoded content of the fragment
  , msgLength  :: !Int              -- ^ length of the original message
  }
  deriving(Typeable,Eq,Generic)

instance Show Fragment where
  show f = show (fragmentId f,theContent f)

instance Binary Fragment


-- | Takes a message (a bytestring) and yields 'n' fragments such that any 'm' of
-- them are sufficient for reconstructing the original message.
encode :: Int -- ^ m: we need ≥ 'm' fragments to reconstruct the original message
       -> Int -- ^ n: total number of fragments  into which we are going to 
              --   split the message; 'n' ≥ 'm'
       -> ByteString -- ^ the original message
       -> [Fragment] -- ^ 'n' fragments
encode m numFragments msg 
  | numFragments >= 1021 && numFragments <1 = 
      throw $ AssertionFailed "encode: invalid number of fragments."
  | otherwise =
  let (intseq,trailLen) = toIntVec m msg 
      len = V.length intseq 
      blocks = V.fromList $ groupInto m intseq 
      vm = vmatrix numFragments m 
      c i k = dotProduct (M.getRow i vm) (blocks ! (k-1)) -- product M.! (i,k) 
      content i = [ c i j 
                  | j <- [ 1 .. ceiling $ fromIntegral len / fromIntegral m ]
                  ] in
  [ Fragment { fragmentId = i 
          , trailLength = trailLen 
          , reconstructionThreshold = m 
          , theContent = content i  
          , msgLength = len 
          } 
  | i <- [1 .. numFragments]
  ]
   
-- | Takes a list of at least m fragments (where 'm' is the reconstruction 
-- threshold used for 'encode') and tries to reconstruct the original message.
-- Throws an 'AssertionFailed' exception if there are less than m fragments
-- or if the fragments belong to a different message.
decode :: [Fragment] 
       -> ByteString
decode [] = throw $ AssertionFailed 
      "decode: need at least m fragments for reconstruction."
decode pss@(p:_) 
  | length pss < reconstructionThreshold p = throw $ AssertionFailed 
      "decode: need at least m fragments for reconstruction."
  | otherwise =
  let m = reconstructionThreshold p 
      idxs = map fragmentId (take m pss) 
      n = maximum idxs 
      fragments :: [Vector FField]
      fragments = map (V.fromList . theContent) (take m pss)
      idxVec = V.fromList idxs 
      vecA = M.matrix m m $ \(i,j) -> 
                 vmatrix n m M.! (idxVec!(i-1),j) 
      matrixBInv = inverse vecA 
      colVecR :: Vector FMatrix 
      colVecR = V.fromList 
                  [  M.transpose $ M.fromLists [ map (! (k-1)) fragments ] 
                  | k <- [1..V.length (head fragments)] 
                  ]  
      idxList = [(j,k) | k <- [1..V.length (head fragments)], j <- [1..m]] 
      matrixBInvTimesColVecr = array ((1,1),(V.length (head fragments),m)) 
        [ ((k,j),head $ 
          array (1,m) (zip [1..] $ M.toLists $ matrixBInv 
                                               *
                                               (colVecR ! (k-1))) A.! j) 
        | k <- [1..V.length (head fragments)], j <- [1..m]] 
      mCont :: [FField]
      mCont = map (\(j,k) -> matrixBInvTimesColVecr A.! (k,j)) idxList in
  fromIntVec (msgLength p - trailLength p) $ V.fromList mCont


-- | Takes an integer m and a bytestring and converts the bytestring into a
-- 'Vector Word8', appending 0s at the end such that the length is dividable by
-- m.
toIntVec :: Int -> ByteString -> (Vector FField,Int)
toIntVec m bStr = 
  let len = B.length bStr in
  let trailLen = if (len `mod` m) == 0 then 0
                                       else ((len `div` m)+1)*m - len in 
  let bStrApp = bStr `B.append` B.pack (replicate trailLen 0) in
  (V.fromList $ map fromIntegral $ B.unpack bStrApp,trailLen)


-- | Converts a bytestring to a 'Vector Word8', removing the trailing 0s.
fromIntVec :: Int -> Vector FField -> ByteString
fromIntVec originalLength intVec = 
  B.pack $ map (fromInteger . PF.toInteger . number) $ V.toList $ V.slice 0 originalLength intVec


-- | Splits a vector into lists of the given size. O(vector-length \/ size).
groupInto :: Int -> Vector a -> [Vector a]
groupInto size as =
  let (fs,ss) = V.splitAt size as in
  if V.null ss 
    then [fs]
    else  fs : groupInto size ss

