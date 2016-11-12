module GoogleCloud.Datastore
  ( Consistency(..)
  , Credentials(..)
  , CursorToken(..)
  , EndCursor(..)
  , FilterOperator(..)
  , GetOptions(..)
  , Id(..)
  , InfoCallbackData(..)
  , KeyPathConfig(..)
  , Kind(..)
  , MaxApiCalls(..)
  , Name(..)
  , Namespace(..)
  , OrderingType(..)
  , ProjectId(..)
  , SaveMethod(..)
  , class HasDatastore
  , _datastore
  , DATASTORE
  , Datastore
  , Key
  , Query
  , configuredDatastore
  , createQuery
  , end
  , filter
  , get
  , groupBy
  , hasAncestor
  , incompleteKey
  , insert
  , keyWithConfig
  , keyWithId
  , limit
  , mkDatastore
  , offset
  , order
  , parameterizeQuery
  , queryRun
  , queryRunUntilComplete
  , save
  , select
  , start
  , update
  , upsert
  ) where

import Data.Array as A
import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Rec.Class (Step(..), tailRecM)
import Control.Monad.State.Trans (StateT)
import Control.Monad.Trans.Class (lift)
import Control.Promise (toAff, Promise)
import Data.Argonaut.Core (Json, fromArray, fromNumber, fromString, fromObject, fromBoolean)
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Argonaut.Prisms (_Array, _Object, _String)
import Data.Array ((!!))
import Data.Either (Either(..))
import Data.Foldable (class Foldable, foldMap)
import Data.Function.Eff (EffFn2, EffFn3, runEffFn3, runEffFn2)
import Data.Function.Uncurried (Fn2, runFn2, Fn3, runFn3, Fn4, runFn4, Fn1, runFn1)
import Data.Int (toNumber)
import Data.Lens ((^.), use, Getter, view, preview, (^?))
import Data.Lens.Index (ix)
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid.Endo (Endo(..))
import Data.Newtype (class Newtype, unwrap)
import Data.NonEmpty (NonEmpty)
import Data.Semigroup ((<>))
import Data.StrMap (fromFoldable)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Prelude (Unit, bind, (<<<), ($), class Show, show, pure, (>>=), (<$>), unit, (<*>))
import Unsafe.Coerce (unsafeCoerce)


newtype ProjectId   = ProjectId String
newtype Credentials = Credentials String
newtype Kind        = Kind String
newtype Id          = Id Int
newtype Name        = Name String
newtype Namespace   = Namespace String
newtype CursorToken = CursorToken String
newtype MaxApiCalls = MaxApiCalls Int
newtype EndCursor   = EndCursor String

derive instance newtypeEndCursor :: Newtype EndCursor _

data OrderingType
  = Descending
  | Ascending

data KeyPathConfig
  = IdPath Kind Id
  | NamePath Kind Name

data Consistency
  = Strong
  | Eventual

data SaveMethod
  = Insert
  | Update
  | Upsert

instance showSaveMethod :: Show SaveMethod where
  show Insert = "insert"
  show Update = "update"
  show Upsert = "upsert"

data InfoCallbackData
  = MoreResultsAfterLimit EndCursor
  | MoreResultsAfterCursor EndCursor
  | NoMoreResults

instance showInfoCallbackData :: Show InfoCallbackData where
  show (MoreResultsAfterLimit (EndCursor ec)) = "More results after limit, cursor: " <> ec
  show (MoreResultsAfterCursor (EndCursor ec)) = "More results after cursor, cursor: " <> ec
  show NoMoreResults = "Query complete, no more results"

instance showConsistency :: Show Consistency where
  show Strong = "strong"
  show Eventual = "eventual"

data GetOptions
  = GetOptions (Maybe Consistency) (Maybe MaxApiCalls)

data FilterOperator
  = Gt
  | Lt
  | GtEq
  | LtEq
  | Eq

instance showFilterOperator :: Show FilterOperator where
  show op = case op of
    Gt   -> ">"
    Lt   -> "<"
    GtEq -> ">="
    LtEq -> "<="
    Eq   -> "="

-- | Evidence of a datastore object from a value of type s, used as the state.
class HasDatastore s where
  _datastore :: Getter s s Datastore Datastore

-- | The type of a Datastore object
foreign import data Datastore :: *

-- | The type of a Key object
foreign import data Key :: *

-- | The type of a Query object
foreign import data Query :: *

-- | The effect associated with using the Datastore module
foreign import data DATASTORE :: !

foreign import gcloudDatastore
  :: forall eff. Eff (datastore :: DATASTORE | eff) Datastore

foreign import configuredDatastoreImpl
  :: forall eff
   . EffFn2 (datastore :: DATASTORE | eff)
      ProjectId
      Credentials
      Datastore

foreign import keyImpl
  :: Fn2
      Datastore
      Json
      Key

foreign import getImplNoOptions
  :: Fn2
      Datastore
      (Array Key)
      (Promise Json)

foreign import getImplWithOptions
  :: Fn3
      Datastore
      (Array Key)
      Json
      (Promise Json)

foreign import saveImpl
  :: Fn2
      Datastore
      Json
      (Promise Json)

foreign import createQueryImplNoNamespace
  :: forall eff
   . EffFn2 (datastore :: DATASTORE | eff)
      Datastore
      String
      Query

foreign import createQueryImplWithNamespace
  :: forall eff
   . EffFn3 (datastore :: DATASTORE | eff)
      Datastore
      String
      String
      Query

foreign import endImpl
  :: Fn2
      String
      Query
      Query

foreign import startImpl
  :: Fn2
      String
      Query
      Query

foreign import hasAncestorImpl
  :: Fn2
      Key
      Query
      Query

foreign import limitImpl
  :: Fn2
      Int
      Query
      Query

foreign import offsetImpl
  :: Fn2
      Int
      Query
      Query

foreign import filterImpl
  :: Fn4
      String
      String
      Json
      Query
      Query

foreign import groupByImpl
  :: Fn2
      (Array String)
      Query
      Query

foreign import selectImpl
  :: Fn2
      (Array String)
      Query
      Query

foreign import orderImpl
  :: Fn3
      String
      Json
      Query
      Query

foreign import queryRunOptionsImpl
  :: Fn2
      Query
      Json
      (Promise Json)

foreign import queryRunNoOptionsImpl
  :: Fn1
      Query
      (Promise Json)

-- | Create an auto-parameterized instance of the Datastore object when running in the GCloud
mkDatastore
  :: forall eff
   . Eff (datastore :: DATASTORE | eff) Datastore
mkDatastore =
  gcloudDatastore

-- | Create a customized instance of the Datastore object with credentials
configuredDatastore
  :: forall eff
   . ProjectId
  -> Credentials
  -> Eff (datastore :: DATASTORE | eff) Datastore
configuredDatastore p c =
  runEffFn2 configuredDatastoreImpl p c

-- | Create an incomplete key given only a Kind
incompleteKey
  :: forall s eff
   . HasDatastore s
  => Kind
  -> StateT s (Aff (datastore :: DATASTORE | eff)) Key
incompleteKey (Kind k) = do
   ds <- use _datastore
   lift (pure (runFn2 keyImpl ds key))
  where key = fromString k

-- | Create a key with an id path
keyWithId
  :: forall s eff
   . HasDatastore s
  => Kind
  -> Id
  -> StateT s (Aff (datastore :: DATASTORE | eff)) Key
keyWithId (Kind k) (Id i) = do
    ds <- use _datastore
    lift (pure (runFn2 keyImpl ds key))
  where key = fromArray [ fromString k, fromNumber $ toNumber i ]

-- | Create a key with an named path
keyWithName
  :: forall s eff
   . HasDatastore s
  => Kind
  -> Name
  -> StateT s (Eff (datastore :: DATASTORE | eff)) Key
keyWithName (Kind k) (Name n) = do
    ds <- use _datastore
    lift (pure (runFn2 keyImpl ds key))
  where key = fromArray [ fromString k, fromString n ]

-- | Create a key with a namespace and a key path configuration
keyWithConfig
  :: forall s eff
   . HasDatastore s
  => Namespace
  -> KeyPathConfig
  -> StateT s (Eff (datastore :: DATASTORE | eff)) Key
keyWithConfig (Namespace ns) (IdPath (Kind k) (Id i))     = do
    ds <- use _datastore
    lift (pure (runFn2 keyImpl ds key))
  where key  = fromObject (fromFoldable vals)
        vals = [ Tuple "namespace" (fromString ns)
               , Tuple "path" (fromArray [fromString k, fromNumber $ toNumber i]) ]
keyWithConfig (Namespace ns) (NamePath (Kind k) (Name n)) = do
    ds <- use _datastore
    lift (pure (runFn2 keyImpl ds key))
  where key  = fromObject (fromFoldable vals)
        vals = [ Tuple "namespace" (fromString ns)
               , Tuple "path" (fromArray [fromString k, fromString n]) ]

instance encodeOptionsJson :: EncodeJson GetOptions where
  encodeJson (GetOptions (Just Strong) (Just (MaxApiCalls i)))   =
    fromObject (fromFoldable [
                 Tuple "consistency" (fromString "strong")
               , Tuple "maxApiCalls" (fromNumber $ toNumber i) ])
  encodeJson (GetOptions (Just Eventual) (Just (MaxApiCalls i))) =
    fromObject (fromFoldable [
                 Tuple "consistency" (fromString "eventual")
               , Tuple "maxApiCalls" (fromNumber $ toNumber i) ])
  encodeJson (GetOptions Nothing (Just (MaxApiCalls i)))         =
    fromObject (fromFoldable [ Tuple "maxApiCalls" (fromNumber $ toNumber i) ])
  encodeJson (GetOptions (Just Strong) Nothing)                  =
    fromObject (fromFoldable [ Tuple "consistency" (fromString "strong") ])
  encodeJson (GetOptions (Just Eventual) Nothing)                =
    fromObject (fromFoldable [ Tuple "consistency" (fromString "eventual") ])
  encodeJson (GetOptions Nothing Nothing)                        =
    fromObject (fromFoldable [])

-- | Get the keys, with optional configuration parameters
get
  :: forall s eff a f
   . (DecodeJson a, HasDatastore s, Foldable f)
  => NonEmpty f Key
  -> Maybe GetOptions
  -> StateT s (Aff (datastore :: DATASTORE | eff)) (Array a)
get ks Nothing = do
  ds  <- use _datastore
  res <- lift (toAff (runFn2 getImplNoOptions ds (A.fromFoldable ks)))
  lift (parseEntity res)
get ks (Just os) = do
  ds  <- use _datastore
  res <- lift (toAff (runFn3 getImplWithOptions ds (A.fromFoldable ks) (encodeJson os)))
  lift (parseEntity res)

parseEntity
  :: forall a eff
   . DecodeJson a
  => Json
  -> Aff eff (Array a)
parseEntity json =
  case traverse decodeJson (json^._Array >>= view _Array) of
    Right x -> pure x
    Left e  -> throwError (error e)

-- | Save the data for the key
save
  :: forall s eff a
   . (EncodeJson a, HasDatastore s)
  => Key
  -> SaveMethod
  -> a
  -> StateT s (Aff (datastore :: DATASTORE | eff)) Unit
save key sm entityData = do
    ds <- use _datastore
    lift (toAff (runFn2 saveImpl ds obj))
    pure unit
  where obj = (fromObject (fromFoldable [
                            Tuple "key" (unsafeCoerce key)
                          , Tuple "data" (encodeJson entityData)
                          , Tuple "method" (fromString (show sm)) ]))

-- | Insert the data for the key
insert
  :: forall s eff a
   . (EncodeJson a, HasDatastore s)
  => Key
  -> a
  -> StateT s (Aff (datastore :: DATASTORE | eff)) Unit
insert key entityData =
  save key Insert entityData

-- | Update the data for the key
update
  :: forall s eff a
   . (EncodeJson a, HasDatastore s)
  => Key
  -> a
  -> StateT s (Aff (datastore :: DATASTORE | eff)) Unit
update key entityData =
  save key Update entityData

-- | Upsert the data for the key
upsert
  :: forall s eff a
   . (EncodeJson a, HasDatastore s)
  => Key
  -> a
  -> StateT s (Aff (datastore :: DATASTORE | eff)) Unit
upsert key entityData =
  save key Upsert entityData

-- | Create a Query object
createQuery
  :: forall s eff f
   . (HasDatastore s, Foldable f)
  => Maybe Namespace
  -> Kind
  -> f (Query -> Query)
  -> StateT s (Aff (datastore :: DATASTORE | eff)) Query
createQuery Nothing               (Kind k) params = do
  ds <- use _datastore
  lift $ parameterizeQuery params <$> liftEff (runEffFn2 createQueryImplNoNamespace ds k)
createQuery (Just (Namespace ns)) (Kind k) params = do
  ds <- use _datastore
  lift $ parameterizeQuery params <$> liftEff (runEffFn3 createQueryImplWithNamespace ds ns k)

-- | Parameterize the query by folding over the given endomorphisms
parameterizeQuery
  :: forall f
   . Foldable f
  => f (Query -> Query)
  -> Query
  -> Query
parameterizeQuery xs q =
  (unwrap (foldMap Endo xs)) q

-- | Set an ending cursor to a query
end
  :: CursorToken
  -> Query
  -> Query
end (CursorToken ct) q =
  runFn2 endImpl ct q

-- | Set a starting cursor to a query
start
  :: CursorToken
  -> Query
  -> Query
start (CursorToken ct) q =
  runFn2 startImpl ct q

-- | Filter a query by ancestors
hasAncestor
  :: Key
  -> Query
  -> Query
hasAncestor k q =
  runFn2 hasAncestorImpl k q

-- | Limit the number of returned entities
limit
  :: Int
  -> Query
  -> Query
limit count q =
  runFn2 limitImpl count q

-- | Offset count for the query entities
offset
  :: Int
  -> Query
  -> Query
offset os q =
  runFn2 offsetImpl os q

-- | Add a filter to the query
filter
  :: forall a
   . EncodeJson a
  => String
  -> FilterOperator
  -> a
  -> Query
  -> Query
filter prop op val q =
  runFn4 filterImpl prop (show op) (encodeJson val) q

-- | Select only the given properties
select
  :: forall f
   . Foldable f
  => NonEmpty f String
  -> Query
  -> Query
select props q =
  runFn2 selectImpl (A.fromFoldable props) q

-- | Group by properties in a query
groupBy
  :: forall f
   . Foldable f
  => NonEmpty f String
  -> Query
  -> Query
groupBy props q =
  runFn2 groupByImpl (A.fromFoldable props) q

-- | Order only the given property with ascending or descending
order
  :: forall f
   . Foldable f
  => String
  -> OrderingType
  -> Query
  -> Query
order prop Descending q =
  runFn3 orderImpl
         prop
         (fromObject (fromFoldable [
                       Tuple "descending" (fromBoolean true)]))
         q
order prop Ascending q =
  runFn3 orderImpl
         prop
         (fromObject (fromFoldable [
                       Tuple "descending" (fromBoolean false)]))
         q

-- | Run a query through the Promise API, returning the result
-- | entities along with the query cursor information
queryRun
  :: forall eff a
   . DecodeJson a
  => Query
  -> Maybe Consistency
  -> Aff (datastore :: DATASTORE | eff) (Tuple (Array a) InfoCallbackData)
queryRun q Nothing = do
  res <- toAff (runFn1 queryRunNoOptionsImpl q)
  Tuple <$> parseEntity res <*> parseInfoCallback res
queryRun q (Just c) = do
    res <- toAff (runFn2 queryRunOptionsImpl q obj)
    Tuple <$> parseEntity res <*> parseInfoCallback res
  where obj = fromObject (fromFoldable [ Tuple "consistency" (fromString (show c)) ])

-- | Run a query through the Promise API, returning the completed result by appending paginated results
queryRunUntilComplete
  :: forall eff a
   . DecodeJson a
  => Query
  -> Maybe Consistency
  -> Aff (datastore :: DATASTORE | eff) (Array a)
queryRunUntilComplete q c = tailRecM go { mc: c, q: q, icb: Nothing, res: [] }
  where go params = case params.icb of
          Nothing                          -> do
            Tuple ar2 icb <- queryRun params.q params.mc
            pure (Loop ( params { icb = pure icb, res = params.res <> ar2 } ))
          Just (MoreResultsAfterLimit ec)  -> do
            Tuple ar2 icb <- queryRun (start (CursorToken $ unwrap ec) params.q) params.mc
            case ar2 of
              [] -> pure (Done params.res)
              _  -> pure (Loop ( params { icb = pure icb, res = params.res <> ar2 } ))
          Just (MoreResultsAfterCursor ec) -> do
            Tuple ar2 icb <- queryRun (start (CursorToken $ unwrap ec) params.q) params.mc
            case ar2 of
              [] -> pure (Done params.res)
              _  -> pure (Loop ( params { icb = pure icb, res = params.res <> ar2 } ))
          Just NoMoreResults               ->
            pure (Done params.res)

parseInfoCallback
  :: forall eff
   . Json
  -> Aff eff InfoCallbackData
parseInfoCallback json =
     maybe (throwError (error "Unable to parse query info callback")) pure icb
  where icb = do
          cbObj <- (json^._Array) !! 1 >>= preview _Object
          mr <- cbObj ^? ix "moreResults" <<< _String
          case mr of
            "MORE_RESULTS_AFTER_LIMIT"  ->
              MoreResultsAfterLimit <<< EndCursor <$>
                cbObj ^? ix "endCursor" <<< _String
            "MORE_RESULTS_AFTER_CURSOR" ->
              MoreResultsAfterCursor <<< EndCursor <$>
                cbObj ^? ix "endCursor" <<< _String
            _                           ->
              pure NoMoreResults

