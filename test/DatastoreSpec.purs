module Test.DatastoreSpec where

import Prelude                           (Unit, bind, ($), pure)

import Control.Monad.Aff                 (Aff)
import Control.Monad.Eff.Class           (liftEff)
import Control.Monad.Eff.Console         (CONSOLE)
import Control.Monad.Eff.Exception       (EXCEPTION)
import Control.Monad.State.Trans         (StateT, evalStateT)
import Control.Monad.Trans.Class         (lift)
import Data.Maybe                        (Maybe(..))
import Data.NonEmpty                     ((:|))
import GoogleCloud.Datastore             (class HasDatastore, Consistency(..), 
                                          Credentials(..), DATASTORE, 
                                          Id(..), Kind(..), ProjectId(..), 
                                          SaveMethod( ..), configuredDatastore, 
                                          get, keyWithId, save)
import GoogleCloud.Datastore.Query       (createQuery, limit, queryRunUntilComplete)
import Test.Spec                         (Spec,describe, it)
import Test.Spec.Assertions              (shouldEqual)
import Test.Data


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

spec :: forall eff. Spec (datastore :: DATASTORE, console :: CONSOLE, exception :: EXCEPTION | eff) Unit
spec = 
  describe "Non-Transactional Intgration" do
    it "handles save, get, query" do
      ds <- liftEff $ configuredDatastore (ProjectId "test") (Credentials "") 
      res <- evalStateT saveData (Config { datastore: ds })
      res `shouldEqual` [testData2, testData]
