module Main
where
import Data.Monoid
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck
import Data.Maybe
import GHC.Conc

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.IDA as IDA
import qualified Crypto.IDA as SecretIDA

instance Arbitrary BS.ByteString where
    arbitrary   = fmap BS.pack arbitrary
instance Arbitrary BL.ByteString where
    arbitrary   = fmap BL.pack arbitrary

main :: IO ()
main = do 
  defaultMainWithOpts
       [ testProperty "IDAencodingDecoding" propIDAEncodingDecoding
       , testProperty "SecretIDAencodingDecoding" propSecretIDAEncodingDecoding
       ] mempty  

        

-- | Input: bytestring b, list of booleans.
-- First, encode b, then randomly select sufficiently many fragments according to
-- the boolean list, finally decode these fragments and check if the original
-- bytestring is matched.
propIDAEncodingDecoding bstr blist = 
  let frags = IDA.encode 10 40 bstr in
  let chosen = fst $ unzip $ filter snd $ zip frags blist in
  if length chosen < 10 
    then True
    else bstr == IDA.decode chosen 


-- | Input: bytestring b, list of booleans.
-- First, encode b, then randomly select sufficiently many fragments according to
-- the boolean list, finally decode these fragments and check if the original
-- bytestring is matched.
propSecretIDAEncodingDecoding bstr blist = ioProperty $ do
  frags <- SecretIDA.encode 5 30 Nothing bstr
  let chosen = fst $ unzip $ filter snd $ zip frags blist 
  if length chosen < 5 
    then return True
    else 
      return $ bstr == SecretIDA.decode chosen

