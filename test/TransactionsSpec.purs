module Test.TransactionsSpec where

import Prelude                           (Unit, bind, ($), pure)

import Control.Monad.Aff                 (Aff)
import Control.Monad.Aff.Class           (liftAff)
import Control.Monad.Eff.Class           (liftEff)
import Control.Monad.Eff.Console         (CONSOLE)
import Control.Monad.Eff.Exception       (EXCEPTION)
import Control.Monad.State.Trans         (StateT, evalStateT, execStateT)
import Control.Monad.Trans.Class         (lift)
import Data.Maybe                        (Maybe(..))
import Data.NonEmpty                     ((:|))
import GoogleCloud.Datastore             (class HasDatastore, Consistency(..), 
                                          Credentials(..), DATASTORE, Datastore, 
                                          Id(..), Kind(..), ProjectId(..), 
                                          SaveMethod( ..), configuredDatastore, 
                                          createQuery, keyWithId)
import GoogleCloud.Datastore.Query       (limit, queryRunUntilComplete)
import GoogleCloud.Datastore.Transaction (class HasTransaction, mkTransaction, 
                                          transactionCommit, transactionGet, 
                                          transactionRun, transactionSave)
import Test.Spec                         (Spec,describe, it)
import Test.Spec.Assertions              (shouldEqual)
import Test.Data

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

spec :: forall eff. Spec (datastore :: DATASTORE, console :: CONSOLE, exception :: EXCEPTION | eff) Unit
spec = 
  describe "Transactional Integration" do
    it "handles save, get, query" do
      ds <- liftEff $ configuredDatastore (ProjectId "test") (Credentials "") 
      res <- transact ds 
      res `shouldEqual` [testData2, testData]
