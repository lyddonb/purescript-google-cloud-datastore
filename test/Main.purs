module Test.Main where

import Prelude                     (Unit, pure, show, bind, (<>),
                                    ($), class Show, unit)

import Control.Monad.Aff           (Aff, launchAff)
import Control.Monad.Aff.Class     (liftAff)
import Control.Monad.Aff.Console   (logShow, log)
import Control.Monad.Eff           (Eff)
import Control.Monad.Eff.Console   (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.State.Trans   (StateT, evalStateT, execStateT)
import Control.Monad.Trans.Class   (lift)
import Data.Argonaut.Decode        (class DecodeJson)
import Data.Argonaut.Encode        (class EncodeJson)
import Data.Argonaut.Generic.Argonaut as GA
import Data.Generic                (class Generic)
import Data.Lens                   (lens)
import Data.Maybe                  (Maybe(..))
import Data.NonEmpty               ((:|))
import GoogleCloud.Datastore (class HasDatastore, Consistency(..), Credentials(..), DATASTORE, Datastore, Id(..), Kind(..), ProjectId(..), SaveMethod( ..), configuredDatastore, get, keyWithId, save)
import GoogleCloud.Datastore.Query (createQuery, limit, queryRunUntilComplete)
import GoogleCloud.Datastore.Transaction (class HasTransaction, Transaction, mkTransaction, transactionCommit, transactionGet, transactionRun, transactionSave)


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

saveDataTransactional
  :: forall s eff. HasDatastore s => HasTransaction s 
  => StateT s (Aff (datastore :: DATASTORE | eff)) (Array HereIsData)
saveDataTransactional = do
  key <- keyWithId (Kind "FOO") (Id 1)
  key2 <- keyWithId (Kind "FOO") (Id 2)
  _ <- transactionSave key Upsert testData2
  _ <- transactionSave key2 Upsert testData
  getres :: Array HereIsData <- transactionGet (key :| []) Nothing
  q <- createQuery Nothing (Kind "FOO") [
    limit 2
  ]
  qres :: Array HereIsData <- lift (queryRunUntilComplete q (Just Eventual))
  pure qres

transact
  :: forall eff. 
  Datastore
  -> Aff (datastore :: DATASTORE | eff) (Array HereIsData)
transact ds = do
  trans <- evalStateT mkTransaction (Config { datastore: ds })
  st <- execStateT transactionRun (Context { datastore: ds, transaction: trans })
  res <- evalStateT saveDataTransactional st
  _ <- execStateT transactionCommit st
  liftAff $ pure res

saveData 
  :: forall s eff. HasDatastore s 
  => StateT s (Aff (datastore :: DATASTORE | eff)) (Array HereIsData)
saveData = do
  key <- keyWithId (Kind "FOO") (Id 1)
  key2 <- keyWithId (Kind "FOO") (Id 2)
  _ <- save key Upsert testData2
  _ <- save key2 Upsert testData
  getres :: Array HereIsData <- get (key :| []) Nothing
  q <- createQuery Nothing (Kind "FOO") [
    limit 2
  ]
  qres :: Array HereIsData <- lift (queryRunUntilComplete q (Just Eventual))
  pure qres

main :: forall eff. Eff (datastore :: DATASTORE, console :: CONSOLE, exception :: EXCEPTION | eff) Unit
main = do
  ds <- configuredDatastore (ProjectId "test") (Credentials "")
  _ <- launchAff $ do
    res <- transact ds
    {--res <- evalStateT saveData (Config { datastore: ds })--}
    _ <- log "Finished evaluating program"
    logShow res
  pure unit


