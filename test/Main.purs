module Test.Main where

import Prelude                     (Unit, pure, show, bind, (<>),
                                    ($), class Show, unit)

import Control.Monad.Aff           (launchAff)
import Control.Monad.Aff.Console   (logShow, log)
import Control.Monad.Eff           (Eff)
import Control.Monad.Eff.Console   (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.State.Trans   (evalStateT)
import Control.Monad.Trans.Class   (lift)
import Data.Argonaut.Decode        (class DecodeJson, gDecodeJson)
import Data.Argonaut.Encode        (class EncodeJson, gEncodeJson)
import Data.Generic                (class Generic)
import Data.Lens                   (lens)
import Data.Maybe                  (Maybe(..))
import Data.NonEmpty               ((:|))
import GoogleCloud.Datastore


data HereIsData
  = HereIsData
  { bar :: String
  , baz :: Int
  , quux :: Boolean }

testData :: HereIsData
testData = HereIsData { bar: "hello", baz: 123, quux: true }

testData2 :: HereIsData
testData2 = HereIsData { bar: "hello_number 2", baz: 456, quux: false }

derive instance genericHereIsData :: Generic HereIsData

instance encodeHereIsData :: EncodeJson HereIsData where
  encodeJson = gEncodeJson

instance decodeHereIsData :: DecodeJson HereIsData where
  decodeJson = gDecodeJson

instance showHereIsData :: Show HereIsData where
  show (HereIsData x) = "HereIsData: " <> x.bar <> " " <> show x.baz <> " " <> show x.quux

data Config
  = Config { datastore :: Datastore }

instance configHasDatastore :: HasDatastore Config where
  _datastore = lens (\(Config x) -> (x.datastore)) (\d x -> Config { datastore: x })

main :: Eff (console :: CONSOLE, datastore :: DATASTORE, err :: EXCEPTION) Unit
main = do
  ds <- configuredDatastore (ProjectId "hello") (Credentials "")
  launchAff $ do
    res <- evalStateT prog (Config { datastore: ds })
    log "Finished evaluating program"
    logShow res
  pure unit
  where prog = do
          key <- keyWithId (Kind "FOO") (Id 1)
          key2 <- keyWithId (Kind "FOO") (Id 2)
          save key Upsert testData2
          save key2 Upsert testData
          getres :: Array HereIsData <- get (key :| []) Nothing
          q <- createQuery Nothing (Kind "FOO") []
          qres :: Array HereIsData <- lift (queryRunUntilComplete q (Just Eventual))
          pure qres

