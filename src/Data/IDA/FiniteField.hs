{-# LANGUAGE DataKinds, DeriveDataTypeable, DeriveGeneric, GeneralizedNewtypeDeriving, TemplateHaskell, Haskell2010, TypeFamilies, FlexibleContexts, Trustworthy, FlexibleInstances, TypeSynonymInstances, OverlappingInstances, UndecidableInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.IDA.FiniteField
-- Copyright   :  Peter Robinson 2014
-- License     :  LGPL
--
-- Maintainer  :  Peter Robinson <peter.robinson@monoid.at>
-- Stability   :  experimental
-- Portability :  portable
--
-- Linear algebra computations in a finite prime field.
--
-----------------------------------------------------------------------------

module Data.IDA.FiniteField
where

import Control.Exception
import Data.Typeable
import GHC.Generics
import qualified Data.FiniteField.PrimeField as PF
import Data.FiniteField.Base

import qualified Data.Vector as V
import Data.Vector(Vector)
import Data.Matrix
import Data.Monoid
import Data.Binary



-- | Our finite prime field. All computations are performed in this field.
newtype FField = FField { number :: $(PF.primeField $ fromIntegral 1021) }
  deriving(Read,Ord,Eq,Num,Fractional,Generic,Typeable,FiniteField)

instance Show FField where
  show = show . PF.toInteger . number

instance Semigroup FField where
  (<>) = (+)

instance Monoid FField where
  mempty = 0
  mappend  = (+)

instance Enum FField where
  toEnum =  FField . fromIntegral
  fromEnum = fromEnum . PF.toInteger . number

instance Binary FField where
  get = do
    n <- get :: Get Integer
    return $ FField { number = fromInteger n }
  put f = put (PF.toInteger $ number f)


-- | The size of the finite field
prime :: Int
prime = fromInteger $ order (0 :: FField)


-- | A matrix over the finite field.
type FMatrix = Matrix FField


dotProduct :: Num a => Vector a -> Vector a -> a
dotProduct v1 v2 = V.sum $ V.zipWith (*) v1 v2


-- | Solves a linear equality system @A x = b@ given by a lower triangular matrix via
-- forward substitution.
forwardSub :: Fractional a => Matrix a -> Vector a -> Vector a
forwardSub =
  forwardSub' (V.empty)
  where
    forwardSub' xV lower bV
      | nrows lower == 0 = xV
      | otherwise =
        let curRow = getRow 1 lower
            offset = V.length xV
            lm   =  getRow 1 lower V.! offset
            curB = V.head bV
            negSum = curRow `dotProduct` xV
            curX = (curB - negSum) / lm
        in
        forwardSub' (V.snoc xV curX)
                    (submatrix 2 (nrows lower) 1 (ncols lower) lower)
                    (V.tail bV)



-- | Solves a linear equality system @A x = b@ given by an upper triangular matrix via
-- backward substitution.
backwardSub :: Fractional a => Matrix a -> Vector a -> Vector a
backwardSub upper bV =
  backwardSub' upper bV V.empty (nrows upper)
  where
    backwardSub' upper bV xV i
      | nrows upper == 0 = xV
      | i<=0             = xV
      | otherwise =
        let curRow = snd $ V.splitAt i $ getRow i upper
            lm   =  (getRow i upper) V.! (i-1)
            curB  = bV V.! (i-1)
            negSum = xV `dotProduct` curRow
            curX = (curB - negSum) / lm
        in
        backwardSub' upper bV (curX `V.cons` xV) (i-1)

-- | Compute the inverse of  matrix. Throws 'AssertionFailed' if the matrix is
-- not invertible.
inverse :: (Ord a,Fractional a) => Matrix a -> Matrix a
inverse mat =
  let mInv = luDecomp mat
  in
  case mInv of
    Nothing -> throw $ AssertionFailed "inverse: matrix is not invertible!"
    Just (upper,lower,pmatrix,_) ->
      let m = nrows mat
          bVecs = [ getCol i (identity m) | i <- [1..m] ]
          columnsOfInverse = flip map bVecs $ \bV ->
            let yV = forwardSub lower (getCol 1 $ pmatrix * colVector bV)
                xV = backwardSub upper yV
            in colVector xV
      in
      foldr (<|>) (fromLists [[]]) columnsOfInverse


-- | Construct a Vandermonde matrix. The i-th list element is the i-th seed of
-- the geometric progression of the i-th row.
vandermonde :: Int -> [FField] -> FMatrix
vandermonde m is = matrix (length is) m $ \(i,j) -> (fromIntegral i)^(fromIntegral $ j-1)

-- | Create an nxm Vandermonde matrix. /O(n m)/.
vmatrix :: Int -> Int -> FMatrix
vmatrix numFragments m = vandermonde m [1..fromIntegral numFragments]
