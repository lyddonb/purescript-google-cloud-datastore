module Test.DatastoreSpec where

import Prelude                           (Unit, bind, discard, ($), (<$>), (>), 
                                         (==), pure)

import Control.Monad.Aff                 (Aff)
import Control.Monad.Eff.Class           (liftEff)
import Control.Monad.Eff.Console         (CONSOLE)
import Control.Monad.Eff.Exception       (EXCEPTION)
import Control.Monad.State.Trans         (StateT, evalStateT)
import Control.Monad.Trans.Class         (lift)
import Data.Foldable                     (and)
import Data.Maybe                        (Maybe(..))
import GoogleCloud.Datastore             
import GoogleCloud.Datastore.Query       (createQuery, limit, queryRunUntilComplete)
import Test.Spec                         (Spec,describe, it)
import Test.Spec.Assertions              (shouldEqual)
import Test.Data


saveData 
  :: forall s eff. HasDatastore s 
  => StateT s (Aff (datastore :: DATASTORE, console :: CONSOLE | eff)) (Array HereIsData)
saveData = do
  key <- createKey [(IdPath (Kind "FOO") (Id 1)), (IdPath (Kind "BAR") (Id 2))]
  key2 <- keyWithId (Kind "FOO") (Id 2)
  _ <- save key Upsert testData2
  _ <- save key2 Upsert testData
  getres :: Array HereIsData <- get key Nothing
  q <- createQuery Nothing (Kind "FOO") [
    limit 2
  ]
  qres :: Array HereIsData <- lift (queryRunUntilComplete q (Just Eventual))
  pure qres

aids
  :: forall s eff. HasDatastore s 
  => StateT s (Aff (datastore :: DATASTORE, console :: CONSOLE | eff)) (Array Key)
aids = do
  key <- incompleteKey (Kind "FOO")
  res <- allocateIds key 10
  pure res

verifyKeyId :: Key -> Boolean
verifyKeyId (Key { id: x }) = if x > 0 then true else false

createIdKey
  :: forall s eff. HasDatastore s 
  => StateT s (Aff (datastore :: DATASTORE, console :: CONSOLE | eff)) Key
createIdKey = do
  key <- createKey [(IdPath (Kind "CREATE") (Id 2))] 
  pure key

createNameKeyWithParent
  :: forall s eff. HasDatastore s 
  => StateT s (Aff (datastore :: DATASTORE, console :: CONSOLE | eff)) Key
createNameKeyWithParent = do
  key <- createKey [ (NamePath (Kind "CREATE") (Name "asdf"))
                   , (NamePath (Kind "IT") (Name "bar"))
                   ] 
  pure key

spec :: forall eff. Spec (datastore :: DATASTORE, console :: CONSOLE, exception :: EXCEPTION | eff) Unit
spec = 
  describe "Non-Transactional Intgration" do
    describe "Integration test" do
      it "handles save, get, query" do
        ds <- liftEff $ configuredDatastore (ProjectId "test") (Credentials "") 
        res <- evalStateT saveData (Config { datastore: ds })
        res `shouldEqual` [testData2, testData]
    describe "Allocate Ids" do
      it "creates 10 FOO keys with int ids greather than 0" do
        ds <- liftEff $ configuredDatastore (ProjectId "test") (Credentials "") 
        res <- evalStateT aids (Config { datastore: ds })
        (and (verifyKeyId <$> res)) `shouldEqual` true
    describe "Create Keys" do
      it "creates a simple id based key with no parent or namespace" do
        ds <- liftEff $ configuredDatastore (ProjectId "test") (Credentials "")
        res <- evalStateT createIdKey (Config { datastore: ds }) 
        (getId res) `shouldEqual` (Id 2)
        (getKind res) `shouldEqual` (Kind "CREATE")
      it "creates a simple name based key with parent or namespace" do
        ds <- liftEff $ configuredDatastore (ProjectId "test") (Credentials "")
        res <- evalStateT createNameKeyWithParent (Config { datastore: ds }) 
        (getName res) `shouldEqual` (Name "bar")
        (getKind res) `shouldEqual` (Kind "IT")
        let key = getParent res
        (getName key) `shouldEqual` (Name "asdf")
        (getKind key) `shouldEqual` (Kind "CREATE")
