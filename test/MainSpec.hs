{-# LANGUAGE OverloadedStrings #-}
module MainSpec where

import SendBack
import Data.Aeson.Lens
import Control.Lens
import Network.Wreq
import Test.Hspec
import Data.Text (unpack)
import Data.Text.Read (double)
import qualified Network.Wreq.Session as S
import Data.Aeson.Encode.Pretty
import Control.Exception as E
import Network.HTTP.Client (HttpException, HttpExceptionContent(..))
import Data.Aeson (decode)
import Data.Either (rights)
import Data.List (sum)
import qualified Data.ByteString.Lazy as B
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Map as Map
import Data.Text (Text)

fromAddress="13NyfikkgNZTubpzRGbBD1azrBav2Z9Qo3"

main :: IO ()
main = hspec spec

spec = do
  describe "sendback's result" $ do
    it "contains exactly one address from every tx sent to fromAddress save for the exceptions in the code comments" $ do
      sess <- S.newSession
      r <- getReptd sess ("https://blockexplorer.com/api/addr/"++fromAddress)
      let txs = r ^.. responseBody . key "transactions" . values . _String
      length txs `shouldBe` 110 -- this was the state when this program was
                                -- written. use beyond this point on your own
                                -- risk.
      r2 <-  mapM (getTx sess) (map unpack txs)
      let r3 = filter (not . inputFilter) r2 -- throw out those tx that have
                                                 -- fromAddress
                                                 -- as an input
      let r4 = filter (not . txidFilter (polo++bcash)) r3 -- throw out exchange addresses
      
      result <- sendback fromAddress
      let res = decode $ B.fromStrict $ encodeUtf8 result :: Maybe (Map.Map Text Double)
      

      let equal = do
            let addresses = map ( ^.. responseBody . key "vin" . values . key "addr" . _String) r4
            vals <- mapM (\r -> do
                             l <- mapM double (valueList r)
                             return $ sum (map fst l)) r4
            let adsVals = zip addresses $ map (/2.0) vals
            return $ do
              resMap <- res
              return $ (Map.size resMap) == (length adsVals) &&
                all (\(a,v) -> any (\t -> Just v==(Map.lookup t resMap)) a) adsVals
      equal `shouldBe` Right (Just True)
        
inputFilter r = any is13 (r ^.. responseBody . key "vin" . values) 
is13 r = unpack (r ^. key "addr" . _String) == fromAddress

getTx sess x = getReptd sess ("https://blockexplorer.com/api/tx/"++x)
          
txidFilter l r = any (\x -> unpack (r ^. responseBody . key "txid" . _String) == x) l

valueList r = map (\r ->  r ^. key "value" . _String)
  (filter (\r -> r ^.. key "scriptPubKey" . key "addresses" . values . _String == ["13NyfikkgNZTubpzRGbBD1azrBav2Z9Qo3"]) (r ^.. responseBody .  key "vout" . values))
      


             
