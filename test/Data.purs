module Test.Data where

import Prelude                           (class Eq, class Show, show, (<>))

{--import Control.Monad.Aff.Console         (logShow, log)--}
import Data.Argonaut.Decode              (class DecodeJson)
import Data.Argonaut.Encode              (class EncodeJson)
import Data.Argonaut.Generic.Argonaut as GA
import Data.Generic                      (class Generic)
import Data.Lens                         (lens)
import GoogleCloud.Datastore             (class HasDatastore, Datastore)
import GoogleCloud.Datastore.Transaction (class HasTransaction, Transaction)


data HereIsData
  = HereIsData
  { bar :: String
  , baz :: Int
  , quux :: Boolean }

derive instance eqHereIsData :: Eq HereIsData

testData :: HereIsData
testData = HereIsData { bar: "hello", baz: 123, quux: true }

testData2 :: HereIsData
testData2 = HereIsData { bar: "hello_number 2", baz: 456, quux: false }

derive instance genericHereIsData :: Generic HereIsData 

instance encodeHereIsData :: EncodeJson HereIsData where
  encodeJson = GA.encodeJson

instance decodeHereIsData :: DecodeJson HereIsData where
  decodeJson = GA.decodeJson

instance showHereIsData :: Show HereIsData where
  show (HereIsData x) = "HereIsData: " <> x.bar <> " " <> show x.baz <> " " <> show x.quux

data Config
  = Config { datastore :: Datastore }

instance configHasDatastore :: HasDatastore Config where
  _datastore = lens (\(Config x) -> (x.datastore)) (\d x -> Config { datastore: x })

data Context
  = Context { datastore :: Datastore
            , transaction :: Transaction
            }

instance contextHasTransaction :: HasTransaction Context where
  _transaction = lens (\(Context x) -> (x.transaction)) (
    \(Context c) x -> Context (c { transaction = x }))

instance contextHasDatastore :: HasDatastore Context where
  _datastore = lens (\(Context x) -> (x.datastore)) (
    \(Context c) x -> Context (c { datastore = x }))
