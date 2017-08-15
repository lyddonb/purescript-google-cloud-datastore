module GoogleCloud.Datastore.Transaction
  ( class HasTransaction
  , _transaction
  , Transaction
  , mkTransaction
  , transactionCommit
  , transactionGet
  , transactionRun
  , transactionSave
  ) where

import Data.Array as A
import Control.Monad.Aff (Aff)
import Control.Monad.State.Trans (StateT)
import Control.Monad.Trans.Class (lift)
import Control.Promise (toAff, Promise)
import Data.Argonaut.Core (Json, fromString, fromObject)
import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Foldable (class Foldable)
import Data.Function.Uncurried (Fn2, runFn2, Fn3, runFn3, Fn1, runFn1)
import Data.Lens (use, Getter)
import Data.Maybe (Maybe(..))
import Data.NonEmpty (NonEmpty)
import Data.StrMap (fromFoldable)
import Data.Tuple (Tuple(..))
import Prelude (Unit, bind, ($), show, pure, unit)
import Unsafe.Coerce (unsafeCoerce)
import GoogleCloud.Datastore


newtype ProjectId   = ProjectId String
newtype Credentials = Credentials String
newtype Kind        = Kind String
newtype Id          = Id Int
newtype Name        = Name String
newtype Namespace   = Namespace String
newtype CursorToken = CursorToken String
newtype MaxApiCalls = MaxApiCalls Int
newtype EndCursor   = EndCursor String

-- | Evidence of a transaction object from a value of type s, used as the state.
class HasTransaction s where
  _transaction :: Getter s s Transaction Transaction

-- | The type of a Transaction object
foreign import data Transaction :: Type

foreign import transactionImpl
  :: Fn1
      Datastore
      Transaction

foreign import transactionSaveImpl
  :: Fn2
      Transaction
      Json
      Unit

foreign import transactionGetImplNoOptions
  :: Fn2
      Transaction
      (Array Key)
      (Promise Json)

foreign import transactionGetImplWithOptions
  :: Fn3
      Transaction
      (Array Key)
      Json
      (Promise Json)

foreign import transactionRunImpl
  :: Fn1
      Transaction
      (Promise Transaction)

foreign import transactionCommitImpl
  :: Fn1
      Transaction
      (Promise Transaction)

-- | Create a instance of the Transaction object 
mkTransaction
  :: forall s eff . HasDatastore s 
  => StateT s (Aff (datastore :: DATASTORE | eff)) Transaction
mkTransaction = do
  ds <- use _datastore
  lift $ pure $ runFn1 transactionImpl ds


transactionRun
  :: forall s eff . HasTransaction s => HasDatastore s
  => StateT s (Aff (datastore :: DATASTORE | eff)) Transaction
transactionRun = do
  trans <- use _transaction
  lift $ toAff $ runFn1 transactionRunImpl trans


transactionCommit
  :: forall s eff . HasTransaction s => HasDatastore s
  => StateT s (Aff (datastore :: DATASTORE | eff)) Transaction
transactionCommit = do
  trans <- use _transaction
  lift $ toAff $ runFn1 transactionCommitImpl trans


-- | Save the data for the key within a Transaction
transactionSave
  :: forall s eff a . EncodeJson a => HasTransaction s => HasDatastore s
  => Key
  -> SaveMethod
  -> a
  -> StateT s (Aff (datastore :: DATASTORE | eff)) Unit
transactionSave key sm entityData = do
    trans <- use _transaction
    _ <- lift (pure (runFn2 transactionSaveImpl trans obj))
    pure unit
  where obj = (fromObject (fromFoldable [
                            Tuple "key" (unsafeCoerce key)
                          , Tuple "data" (encodeJson entityData)
                          , Tuple "method" (fromString (show sm)) ]))

transactionGet
  :: forall s eff a f . DecodeJson a => HasTransaction s => HasDatastore s => Foldable f
  => NonEmpty f Key
  -> Maybe GetOptions
  -> StateT s (Aff (datastore :: DATASTORE | eff)) (Array a)
transactionGet ks Nothing = do
  trans  <- use _transaction
  res <- lift (toAff (runFn2 transactionGetImplNoOptions trans (A.fromFoldable ks)))
  lift (parseEntity res)
transactionGet ks (Just os) = do
  trans  <- use _transaction
  res <- lift (toAff (runFn3 transactionGetImplWithOptions trans (A.fromFoldable ks) (encodeJson os)))
  lift (parseEntity res)
