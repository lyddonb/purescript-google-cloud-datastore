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
  , Key(..)
  , allocateIds
  , configuredDatastore
  , createKey
  , get
  , getId
  , getKind
  , getName
  , getNamespace
  , getParent
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
import Data.Array (snoc)
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Generic (class Generic, gEq, gShow)
import Control.Monad.Eff.Uncurried (EffFn2, runEffFn2)
import Data.Function.Uncurried (Fn1, runFn1, Fn2, runFn2, Fn3, runFn3)
import Data.Int as DI
import Data.Lens ((^.), use, Getter, view)
import Data.Maybe (Maybe(..))
import Data.StrMap (fromFoldable)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Prelude (class Eq, class Show, Unit, bind, ($), show, pure, (>>=), unit)
import Unsafe.Coerce (unsafeCoerce)


newtype ProjectId   = ProjectId String
newtype Credentials = Credentials String
newtype MaxApiCalls = MaxApiCalls Int

newtype Kind        = Kind String
derive instance genericKind :: Generic Kind 
instance showKind :: Show Kind where show = gShow
instance eqKind :: Eq Kind where eq = gEq

newtype Id          = Id Int
derive instance genericId :: Generic Id 
instance showId :: Show Id where show = gShow
instance eqId :: Eq Id where eq = gEq

newtype Name        = Name String
derive instance genericName :: Generic Name 
instance showName :: Show Name where show = gShow
instance eqName :: Eq Name where eq = gEq

newtype Namespace   = Namespace String
derive instance genericNamespace :: Generic Namespace 
instance showNamespace :: Show Namespace where show = gShow
instance eqNamespace :: Eq Namespace where eq = gEq

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

newtype Key = Key 
  { namespace :: String
  , id :: Int
  , name :: String
  , kind :: String
  , parent :: Key
  }

getId :: Key -> Id
getId (Key { id: i }) = Id i

getKind :: Key -> Kind
getKind (Key { kind: k }) = Kind k

getName :: Key -> Name
getName (Key { name: n }) = Name n

getNamespace :: Key -> Namespace
getNamespace (Key { namespace: n }) = Namespace n

getParent :: Key -> Key
getParent (Key { parent: p }) = p

type AllocateResponse = 
  { keys :: Array Key
  , response :: Json
  }

-- | Evidence of a datastore object from a value of type s, used as the state.
class HasDatastore s where
  _datastore :: Getter s s Datastore Datastore

-- | The type of a Datastore object
foreign import data Datastore :: Type

foreign import data AllocateResponseImpl :: Type

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

foreign import allocateIdsImpl
  :: Fn3
      Datastore
      (Array Key)
      Int
      {--(Promise (Array Key))--}
      (Promise AllocateResponseImpl)

foreign import handleAllocateImpl 
  :: Fn1 AllocateResponseImpl AllocateResponse


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

createKey :: forall s eff . HasDatastore s
  => Array KeyPathConfig
  -> StateT s (Aff (datastore :: DATASTORE | eff)) Key
createKey keyPath = do
  ds <- use _datastore
  lift (pure (runFn2 keyImpl ds $ makeKeyPath keyPath))

makeKeyPath :: Array KeyPathConfig -> Json
makeKeyPath keyPath =
   fromArray (foldl makeKeyArray [] keyPath)
  where
    makeKeyArray :: Array Json -> KeyPathConfig -> Array Json
    makeKeyArray prev (IdPath (Kind k) (Id i)) = 
      foldl snoc prev [fromString k, (fromNumber $ DI.toNumber i)]
    makeKeyArray prev (NamePath (Kind k) (Name i)) = 
      foldl snoc prev [fromString k, fromString i]

-- | Create a key with an id path
keyWithId :: forall s eff . HasDatastore s
  => Kind
  -> Id
  -> StateT s (Aff (datastore :: DATASTORE | eff)) Key
keyWithId (Kind k) (Id i) = do
    ds <- use _datastore
    lift (pure (runFn2 keyImpl ds key))
  where key = fromArray [ fromString k, fromNumber $ DI.toNumber i ]

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
               , Tuple "path" (fromArray [fromString k, fromNumber $ DI.toNumber i]) ]
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
               , Tuple "maxApiCalls" (fromNumber $ DI.toNumber i) ])
  encodeJson (GetOptions (Just Eventual) (Just (MaxApiCalls i))) =
    fromObject (fromFoldable [
                 Tuple "consistency" (fromString "eventual")
               , Tuple "maxApiCalls" (fromNumber $ DI.toNumber i) ])
  encodeJson (GetOptions Nothing (Just (MaxApiCalls i)))         =
    fromObject (fromFoldable [ Tuple "maxApiCalls" (fromNumber $ DI.toNumber i) ])
  encodeJson (GetOptions (Just Strong) Nothing)                  =
    fromObject (fromFoldable [ Tuple "consistency" (fromString "strong") ])
  encodeJson (GetOptions (Just Eventual) Nothing)                =
    fromObject (fromFoldable [ Tuple "consistency" (fromString "eventual") ])
  encodeJson (GetOptions Nothing Nothing)                        =
    fromObject (fromFoldable [])

-- | Get the keys, with optional configuration parameters
get
  :: forall s eff a . DecodeJson a => HasDatastore s 
  => Key
  -> Maybe GetOptions
  -> StateT s (Aff (datastore :: DATASTORE | eff)) (Array a)
get ks Nothing = do
  ds  <- use _datastore
  res <- lift (toAff (runFn2 getImplNoOptions ds (unsafeCoerce ks)))
  lift (parseEntity res)
get ks (Just os) = do
  ds  <- use _datastore
  res <- lift (toAff (runFn3 getImplWithOptions ds (unsafeCoerce ks) (encodeJson os)))
  lift (parseEntity res)

parseEntity
  :: forall a eff . DecodeJson a
  => Json
  -> Aff eff (Array a)
parseEntity json =
  case traverse decodeJson (json^._Array >>= view _Array) of
    Right x -> pure x
    Left e  -> throwError (error e)

decodeEntity
  :: forall a eff . DecodeJson a
  => Json
  -> Aff eff (Array a)
decodeEntity json =
  case decodeJson json >>= traverse decodeJson of
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

-- | Allocate "x" number of ids for the kind 
allocateIds
  :: forall s eff . HasDatastore s
  => Key
  -> Int
  -> StateT s (Aff (datastore :: DATASTORE | eff)) (Array Key)
allocateIds ks n = do
  ds  <- use _datastore
  res <- lift (toAff (runFn3 allocateIdsImpl ds (unsafeCoerce ks) n))
  r <- lift $ pure (runFn1 handleAllocateImpl res)
  pure $ r.keys
