{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, GeneralizedNewtypeDeriving, TemplateHaskell, Haskell2010, TypeFamilies, FlexibleContexts, Trustworthy, FlexibleInstances, TypeSynonymInstances, OverlappingInstances, UndecidableInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.IDA.FiniteField.Matrix
-- Copyright   :  Peter Robinson 2014
-- License     :  LGPL
-- 
-- Maintainer  :  Peter Robinson <peter.robinson@monoid.at>
-- Stability   :  stable
-- Portability :  portable
-- 
-----------------------------------------------------------------------------

module Data.IDA.FiniteField.Matrix -- ( FField, prime )
where

import Control.Exception
import Data.Typeable
import GHC.Generics
import qualified Data.FiniteField.PrimeField as PF
import Data.FiniteField.Base
import Data.IDA.Prime

import qualified Data.List as L

import Data.Typeable

import qualified Data.Vector as V
import Data.Vector(Vector)
--import Data.IDA.FiniteField

import Data.Matrix

dotProduct :: Num a => Vector a -> Vector a -> a
dotProduct v1 v2 = V.sum $ V.zipWith (*) v1 v2

forwardSub :: Fractional a => Matrix a -> Vector a -> Vector a
forwardSub lower bV =
  forwardSub' lower bV (V.empty)
  where
    forwardSub' lower bV xV 
      | nrows lower == 0 = xV
      | otherwise = 
        let curRow = V.init $ getRow 1 lower 
            offset = V.length xV
            lm   =  (getRow 1 lower) V.! offset
            curB = V.head bV  
            negSum = curRow `dotProduct` xV
            curX = (curB - negSum) / lm
        in
        forwardSub' (submatrix 2 (nrows lower) 1 (ncols lower) lower) 
                   (V.tail bV) 
                   (V.snoc xV curX)


backwardSub :: Fractional a => Matrix a -> Vector a -> Vector a
backwardSub upper bV =
  backwardSub' upper bV V.empty (nrows upper)
  where
    backwardSub' upper bV xV i 
      | nrows upper == 0 = xV
      | i<=0             = xV
      | otherwise = 
        let curRow = V.tail $ getRow i upper 
            lm   =  (getRow i upper) V.! (i-1)
            (curB ) = bV V.! (i-1)
            negSum = xV `dotProduct` curRow
            curX = (curB - negSum) / lm
        in
        backwardSub' upper bV (curX `V.cons` xV) (i-1)
          
 
inverse :: (Ord a,Fractional a) => Matrix a -> Matrix a
inverse mat = 
  let (upper,lower,pmatrix,det) = luDecompUnsafe mat
      m = nrows mat
      bVecs = [ getCol i (identity m) | i <- [1..m] ]
      columnsOfInverse = flip map bVecs $ \bV ->
        let yV = forwardSub lower (getCol 1 $ pmatrix * colVector bV)
            xV = backwardSub upper yV
        in colVector xV
  in 
  foldr (<|>) (fromLists [[]]) columnsOfInverse
  
