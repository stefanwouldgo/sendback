{-# LANGUAGE OverloadedStrings #-}
module SendBack where

import Lib

import Data.Aeson.Lens
import Control.Lens
import Network.Wreq
import Test.Hspec
import Data.Text (Text, unpack)
import Data.Text.Read (rational)
import qualified Network.Wreq.Session as S
import Data.Aeson.Encode.Pretty
import Control.Exception as E
import Network.HTTP.Client (HttpException, HttpExceptionContent(..))
import Data.Aeson (toJSON)
import Data.Either (rights)
import Data.List (sum)
import qualified Data.ByteString.Lazy as B
import qualified Data.Text as T
import qualified Data.Map as Map

sendback :: String -> IO Text
sendback fromAddress = do 
  sess <- S.newSession
  ru <- getReptd sess $ "https://api.bitcoinprivacy.net/utxos/"++fromAddress++"/0/1000"
  rs <- getReptd sess $ "https://api.bitcoinprivacy.net/movements/"++fromAddress++"/0/1000"
  let tx = map unpack $ (txs ru) ++ (txs rs)
  let val = (vals ru) ++ (vals rs)
  let txval = filter (\(x,_)-> not . elem x $ polo++bcash) $ zip tx val -- throw out stuff
  adval <- mapM (\(tx,v) -> do { r <- getReptd sess $ "https://api.bitcoinprivacy.net/inputs/"++tx++"/0/1" ; return (r ^. responseBody . values . key "address" . _String , calc v) } ) txval
  let strings = map (\(a,v) -> T.concat ["\"",a,"\":",(T.pack $ show v)]) adval
  return $ T.concat ["{", T.intercalate "," strings, "}"]

txs r = r ^.. responseBody . values . key "tx" . _String
vals r = r ^.. responseBody . values . key "value" . _Number

calc n = n / (2.0 * 10^8)

getReptd sess url = S.get sess url >>= responder
  where responder s 
          | s ^. responseStatus . statusCode == 400 = getReptd sess url
          | otherwise = return s

bcash = ["6f4e90ad56f9625983b76f1646186fe970f9ea99f700575eca49540b7a5bf9cb"]
xapo = ["41689179071e0a98965147bee7f357386e874300cddacbc00c29cb786f1dcd18",
        "430aa2d3087d81f3b02b9375b2117e2c9704a6f8cd855b084a4e997684f13ae6"]

polo = ["de7c41cc1a3b2cf8cd58fd6c42fdcabae587ae8be6325e138acdda2fabd00391",
            "fc92654877f4cf03c6a6790428c5185a9e5b9de96f439163405afde36ce40a6d",            
            "479eee0fb5bf33e729f0393cf99888d6a3ab76bd3b897aac090cad6cf6125ac9",
            "999738a32c1c8bb900fc3d49d0325ef72b2113e1d60568e4da8fc3c92c7fecb5"]

maybes = ["0ab21eb583527deee6c559c5c472524b6335d67376a943c1460bb19b46fad948", --4m
          "63ee7b56ec45803d5755208d24dc31587d7896d1668233cacff9d98ad80190f7", --3.9m
          "8bf7b32431567d935ba34c529ecf7949029452006448f6a35b3b5b6796556a34"] --20m

consolidating = ["97aa596b03ef6d73db8c7c76d35046b1ab5d2b99db58840d9c30179e57b6ca32",
                "da0dee468923a84bb17cc42dadc4b106d1c968cd5ceb965fa92c62a733714907",
                "9d253f380ee4e63d7d2d803ed0d7531936744c2ac0e3b73210adb27561b0a9ff"]

