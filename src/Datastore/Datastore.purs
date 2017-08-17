module GoogleCloud.Datastore
  ( Consistency(..)
  , Credentials(..)
  , GetOptions(..)
  , Id(..)
  , KeyPathConfig(..)
  , Kind(..)
  , MaxApiCalls(..)
  , Name(..)
  , Namespace(..)
  , ProjectId(..)
  , SaveMethod(..)
  , class HasDatastore
  , _datastore
  , DATASTORE
  , Datastore
  , Key
  , configuredDatastore
  , get
  , incompleteKey
  , insert
  , keyWithConfig
  , keyWithId
  , mkDatastore
  , parseEntity
  , save
  , update
  , upsert
  ) where

import Data.Array as A
import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Error.Class (throwError)
import Control.Monad.State.Trans (StateT)
import Control.Monad.Trans.Class (lift)
import Control.Promise (toAff, Promise)
import Data.Argonaut.Core (Json, fromArray, fromNumber, fromString, fromObject)
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Argonaut.Prisms (_Array)
import Data.Either (Either(..))
import Data.Foldable (class Foldable)
import Control.Monad.Eff.Uncurried (EffFn2, runEffFn2)
import Data.Function.Uncurried (Fn2, runFn2, Fn3, runFn3)
import Data.Int (toNumber)
import Data.Lens ((^.), use, Getter, view)
import Data.Maybe (Maybe(..))
import Data.NonEmpty (NonEmpty)
import Data.StrMap (fromFoldable)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Prelude (Unit, bind, ($), class Show, show, pure, (>>=), unit)
import Unsafe.Coerce (unsafeCoerce)


newtype ProjectId   = ProjectId String
newtype Credentials = Credentials String
newtype Kind        = Kind String
newtype Id          = Id Int
newtype Name        = Name String
newtype Namespace   = Namespace String
newtype MaxApiCalls = MaxApiCalls Int

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

instance showConsistency :: Show Consistency where
  show Strong = "strong"
  show Eventual = "eventual"

data GetOptions
  = GetOptions (Maybe Consistency) (Maybe MaxApiCalls)

-- | Evidence of a datastore object from a value of type s, used as the state.
class HasDatastore s where
  _datastore :: Getter s s Datastore Datastore

-- | The type of a Datastore object
foreign import data Datastore :: Type

-- | The type of a Key object
foreign import data Key :: Type

-- | The effect associated with using the Datastore module
foreign import data DATASTORE :: Effect

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

-- | Create an auto-parameterized instance of the Datastore object when running in the GCloud
mkDatastore
  :: forall eff . Eff (datastore :: DATASTORE | eff) Datastore
mkDatastore =
  gcloudDatastore

-- | Create a customized instance of the Datastore object with credentials
configuredDatastore
  :: forall eff . ProjectId
  -> Credentials
  -> Eff (datastore :: DATASTORE | eff) Datastore
configuredDatastore p c =
  runEffFn2 configuredDatastoreImpl p c

-- | Create an incomplete key given only a Kind
incompleteKey
  :: forall s eff . HasDatastore s
  => Kind
  -> StateT s (Aff (datastore :: DATASTORE | eff)) Key
incompleteKey (Kind k) = do
   ds <- use _datastore
   lift (pure (runFn2 keyImpl ds key))
  where key = fromString k

-- | Create a key with an id path
keyWithId :: forall s eff . HasDatastore s
  => Kind
  -> Id
  -> StateT s (Aff (datastore :: DATASTORE | eff)) Key
keyWithId (Kind k) (Id i) = do
    ds <- use _datastore
    lift (pure (runFn2 keyImpl ds key))
  where key = fromArray [ fromString k, fromNumber $ toNumber i ]

-- | Create a key with an named path
keyWithName :: forall s eff . HasDatastore s
  => Kind
  -> Name
  -> StateT s (Eff (datastore :: DATASTORE | eff)) Key
keyWithName (Kind k) (Name n) = do
    ds <- use _datastore
    lift (pure (runFn2 keyImpl ds key))
  where key = fromArray [ fromString k, fromString n ]

-- | Create a key with a namespace and a key path configuration
keyWithConfig
  :: forall s eff . HasDatastore s
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
  :: forall s eff a f . DecodeJson a => HasDatastore s => Foldable f
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
  :: forall a eff . DecodeJson a
  => Json
  -> Aff eff (Array a)
parseEntity json =
  case traverse decodeJson (json^._Array >>= view _Array) of
    Right x -> pure x
    Left e  -> throwError (error e)

-- | Save the data for the key
save
  :: forall s eff a . EncodeJson a => HasDatastore s
  => Key
  -> SaveMethod
  -> a
  -> StateT s (Aff (datastore :: DATASTORE | eff)) Unit
save key sm entityData = do
    ds <- use _datastore
    _ <- lift (toAff (runFn2 saveImpl ds obj))
    pure unit
  where obj = (fromObject (fromFoldable [
                            Tuple "key" (unsafeCoerce key)
                          , Tuple "data" (encodeJson entityData)
                          , Tuple "method" (fromString (show sm)) ]))

-- | Insert the data for the key
insert
  :: forall s eff a . EncodeJson a => HasDatastore s
  => Key
  -> a
  -> StateT s (Aff (datastore :: DATASTORE | eff)) Unit
insert key entityData =
  save key Insert entityData

-- | Update the data for the key
update
  :: forall s eff a . EncodeJson a => HasDatastore s
  => Key
  -> a
  -> StateT s (Aff (datastore :: DATASTORE | eff)) Unit
update key entityData =
  save key Update entityData

-- | Upsert the data for the key
upsert
  :: forall s eff a . EncodeJson a => HasDatastore s
  => Key
  -> a
  -> StateT s (Aff (datastore :: DATASTORE | eff)) Unit
upsert key entityData =
  save key Upsert entityData
