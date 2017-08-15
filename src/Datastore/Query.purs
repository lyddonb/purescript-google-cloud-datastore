module GoogleCloud.Datastore.Query
  ( CursorToken(..)
  , EndCursor(..)
  , FilterOperator(..)
  , InfoCallbackData(..)
  , OrderingType
  , Query
  , createQuery
  , end
  , filter
  , groupBy
  , hasAncestor
  , limit
  , offset
  , order
  , parameterizeQuery
  , queryRun
  , queryRunUntilComplete
  , start
  ) where

import Data.Array as A
import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Rec.Class (Step(..), tailRecM)
import Control.Monad.State.Trans (StateT)
import Control.Monad.Trans.Class (lift)
import Control.Promise (toAff, Promise)
import Data.Argonaut.Core (Json, fromString, fromObject, fromBoolean)
import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Argonaut.Prisms (_Array, _Object, _String)
import Data.Array ((!!))
import Data.Foldable (class Foldable, foldMap)
import Control.Monad.Eff.Uncurried (EffFn2, EffFn3, runEffFn3, runEffFn2)
import Data.Function.Uncurried (Fn2, runFn2, Fn3, runFn3, Fn4, runFn4, Fn1, runFn1)
import Data.Lens ((^.), use, preview, (^?))
import Data.Lens.Index (ix)
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid.Endo (Endo(..))
import Data.Newtype (class Newtype, unwrap, ala)
import Data.NonEmpty (NonEmpty)
import Data.Semigroup ((<>))
import Data.StrMap (fromFoldable)
import Data.Tuple (Tuple(..))
import Prelude (bind, (<<<), ($), class Show, show, pure, (>>=), (<$>), (<*>))
import GoogleCloud.Datastore

newtype CursorToken = CursorToken String
newtype EndCursor   = EndCursor String

derive instance newtypeEndCursor :: Newtype EndCursor _

data OrderingType
  = Descending
  | Ascending

data InfoCallbackData
  = MoreResultsAfterLimit EndCursor
  | MoreResultsAfterCursor EndCursor
  | NoMoreResults

instance showInfoCallbackData :: Show InfoCallbackData where
  show (MoreResultsAfterLimit (EndCursor ec)) = "More results after limit, cursor: " <> ec
  show (MoreResultsAfterCursor (EndCursor ec)) = "More results after cursor, cursor: " <> ec
  show NoMoreResults = "Query complete, no more results"

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

-- | The type of a Query object
foreign import data Query :: Type

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

foreign import runOptionsImpl
  :: Fn2
      Query
      Json
      (Promise Json)

foreign import runNoOptionsImpl
  :: Fn1
      Query
      (Promise Json)

-- | Create a Query object
createQuery
  :: forall s eff f . HasDatastore s => Foldable f
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
  :: forall f . Foldable f
  => f (Query -> Query)
  -> Query
  -> Query
parameterizeQuery =
  ala Endo foldMap

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
  :: forall a . EncodeJson a
  => String
  -> FilterOperator
  -> a
  -> Query
  -> Query
filter prop op val q =
  runFn4 filterImpl prop (show op) (encodeJson val) q

-- | Select only the given properties
select
  :: forall f . Foldable f
  => NonEmpty f String
  -> Query
  -> Query
select props q =
  runFn2 selectImpl (A.fromFoldable props) q

-- | Group by properties in a query
groupBy
  :: forall f . Foldable f
  => NonEmpty f String
  -> Query
  -> Query
groupBy props q =
  runFn2 groupByImpl (A.fromFoldable props) q

-- | Order only the given property with ascending or descending
order
  :: forall f . Foldable f
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
  :: forall eff a . DecodeJson a
  => Query
  -> Maybe Consistency
  -> Aff (datastore :: DATASTORE | eff) (Tuple (Array a) InfoCallbackData)
queryRun q Nothing = do
  res <- toAff (runFn1 runNoOptionsImpl q)
  Tuple <$> parseEntity res <*> parseInfoCallback res
queryRun q (Just c) = do
    res <- toAff (runFn2 runOptionsImpl q obj)
    Tuple <$> parseEntity res <*> parseInfoCallback res
  where obj = fromObject (fromFoldable [ Tuple "consistency" (fromString (show c)) ])

-- | Run a query through the Promise API, returning the completed result by appending paginated results
queryRunUntilComplete
  :: forall eff a . DecodeJson a
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
  :: forall eff . Json
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
