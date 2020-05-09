-----------------------------------------------------------------------------
-- |
-- Module      :  Data.IDA
-- Copyright   :  Peter Robinson 2014
-- License     :  LGPL
-- 
-- Maintainer  :  Peter Robinson <peter.robinson@monoid.at>
-- Stability   :  experimental
-- Portability :  portable
-- 
-- This module provides an implementation of a space efficient (m,n)-threshold information 
-- dispersal algorithm (IDA) as described in 
-- "Efficient Dispersal of Information for Security, Load Balancing, and Fault
-- Tolerance", by Michael O. Rabin, JACM 1989.
--
-- Given a ByteString bstr of length D
-- >>> f = encode m n bstr
-- encodes bstr as a list of n 'Fragment's, each containing a ByteString
-- of length approximately @D/m@. For reconstructing the original ByteString out 
-- of a sublist fssub of fs, 
-- consisting of at least m fragments, use
-- >>> decode fssub
--
-----------------------------------------------------------------------------
module Data.IDA(encode,decode,Fragment(fragmentId))
where
import Data.IDA.Internal



 
