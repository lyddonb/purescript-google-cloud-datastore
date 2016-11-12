## Module GoogleCloud.Datastore

#### `ProjectId`

``` purescript
newtype ProjectId
  = ProjectId String
```

#### `Credentials`

``` purescript
newtype Credentials
  = Credentials String
```

#### `Kind`

``` purescript
newtype Kind
  = Kind String
```

#### `Id`

``` purescript
newtype Id
  = Id Int
```

#### `Name`

``` purescript
newtype Name
  = Name String
```

#### `Namespace`

``` purescript
newtype Namespace
  = Namespace String
```

#### `CursorToken`

``` purescript
newtype CursorToken
  = CursorToken String
```

#### `MaxApiCalls`

``` purescript
newtype MaxApiCalls
  = MaxApiCalls Int
```

#### `EndCursor`

``` purescript
newtype EndCursor
  = EndCursor String
```

##### Instances
``` purescript
Newtype EndCursor _
```

#### `OrderingType`

``` purescript
data OrderingType
  = Descending
  | Ascending
```

#### `KeyPathConfig`

``` purescript
data KeyPathConfig
  = IdPath Kind Id
  | NamePath Kind Name
```

#### `Consistency`

``` purescript
data Consistency
  = Strong
  | Eventual
```

##### Instances
``` purescript
Show Consistency
```

#### `SaveMethod`

``` purescript
data SaveMethod
  = Insert
  | Update
  | Upsert
```

##### Instances
``` purescript
Show SaveMethod
```

#### `InfoCallbackData`

``` purescript
data InfoCallbackData
  = MoreResultsAfterLimit EndCursor
  | MoreResultsAfterCursor EndCursor
  | NoMoreResults
```

##### Instances
``` purescript
Show InfoCallbackData
```

#### `GetOptions`

``` purescript
data GetOptions
  = GetOptions (Maybe Consistency) (Maybe MaxApiCalls)
```

##### Instances
``` purescript
EncodeJson GetOptions
```

#### `FilterOperator`

``` purescript
data FilterOperator
  = Gt
  | Lt
  | GtEq
  | LtEq
  | Eq
```

##### Instances
``` purescript
Show FilterOperator
```

#### `HasDatastore`

``` purescript
class HasDatastore s where
  _datastore :: Getter s s Datastore Datastore
```

Evidence of a datastore object from a value of type s, used as the state.

#### `Datastore`

``` purescript
data Datastore :: *
```

The type of a Datastore object

#### `Key`

``` purescript
data Key :: *
```

The type of a Key object

#### `Query`

``` purescript
data Query :: *
```

The type of a Query object

#### `DATASTORE`

``` purescript
data DATASTORE :: !
```

The effect associated with using the Datastore module

#### `mkDatastore`

``` purescript
mkDatastore :: forall eff. Eff (datastore :: DATASTORE | eff) Datastore
```

Create an auto-parameterized instance of the Datastore object when running in the GCloud

#### `configuredDatastore`

``` purescript
configuredDatastore :: forall eff. ProjectId -> Credentials -> Eff (datastore :: DATASTORE | eff) Datastore
```

Create a customized instance of the Datastore object with credentials

#### `incompleteKey`

``` purescript
incompleteKey :: forall s eff. HasDatastore s => Kind -> StateT s (Aff (datastore :: DATASTORE | eff)) Key
```

Create an incomplete key given only a Kind

#### `keyWithId`

``` purescript
keyWithId :: forall s eff. HasDatastore s => Kind -> Id -> StateT s (Aff (datastore :: DATASTORE | eff)) Key
```

Create a key with an id path

#### `keyWithConfig`

``` purescript
keyWithConfig :: forall s eff. HasDatastore s => Namespace -> KeyPathConfig -> StateT s (Eff (datastore :: DATASTORE | eff)) Key
```

Create a key with a namespace and a key path configuration

#### `get`

``` purescript
get :: forall s eff a f. (DecodeJson a, HasDatastore s, Foldable f) => NonEmpty f Key -> Maybe GetOptions -> StateT s (Aff (datastore :: DATASTORE | eff)) (Array a)
```

Get the keys, with optional configuration parameters

#### `save`

``` purescript
save :: forall s eff a. (EncodeJson a, HasDatastore s) => Key -> SaveMethod -> a -> StateT s (Aff (datastore :: DATASTORE | eff)) Unit
```

Save the data for the key

#### `insert`

``` purescript
insert :: forall s eff a. (EncodeJson a, HasDatastore s) => Key -> a -> StateT s (Aff (datastore :: DATASTORE | eff)) Unit
```

Insert the data for the key

#### `update`

``` purescript
update :: forall s eff a. (EncodeJson a, HasDatastore s) => Key -> a -> StateT s (Aff (datastore :: DATASTORE | eff)) Unit
```

Update the data for the key

#### `upsert`

``` purescript
upsert :: forall s eff a. (EncodeJson a, HasDatastore s) => Key -> a -> StateT s (Aff (datastore :: DATASTORE | eff)) Unit
```

Upsert the data for the key

#### `createQuery`

``` purescript
createQuery :: forall s eff f. (HasDatastore s, Foldable f) => Maybe Namespace -> Kind -> f (Query -> Query) -> StateT s (Aff (datastore :: DATASTORE | eff)) Query
```

Create a Query object

#### `parameterizeQuery`

``` purescript
parameterizeQuery :: forall f. Foldable f => f (Query -> Query) -> Query -> Query
```

Parameterize the query by folding over the given endomorphisms

#### `end`

``` purescript
end :: CursorToken -> Query -> Query
```

Set an ending cursor to a query

#### `start`

``` purescript
start :: CursorToken -> Query -> Query
```

Set a starting cursor to a query

#### `hasAncestor`

``` purescript
hasAncestor :: Key -> Query -> Query
```

Filter a query by ancestors

#### `limit`

``` purescript
limit :: Int -> Query -> Query
```

Limit the number of returned entities

#### `offset`

``` purescript
offset :: Int -> Query -> Query
```

Offset count for the query entities

#### `filter`

``` purescript
filter :: forall a. EncodeJson a => String -> FilterOperator -> a -> Query -> Query
```

Add a filter to the query

#### `select`

``` purescript
select :: forall f. Foldable f => NonEmpty f String -> Query -> Query
```

Select only the given properties

#### `groupBy`

``` purescript
groupBy :: forall f. Foldable f => NonEmpty f String -> Query -> Query
```

Group by properties in a query

#### `order`

``` purescript
order :: forall f. Foldable f => String -> OrderingType -> Query -> Query
```

Order only the given property with ascending or descending

#### `queryRun`

``` purescript
queryRun :: forall eff a. DecodeJson a => Query -> Maybe Consistency -> Aff (datastore :: DATASTORE | eff) (Tuple (Array a) InfoCallbackData)
```

Run a query through the Promise API, returning the result
entities along with the query cursor information

#### `queryRunUntilComplete`

``` purescript
queryRunUntilComplete :: forall eff a. DecodeJson a => Query -> Maybe Consistency -> Aff (datastore :: DATASTORE | eff) (Array a)
```

Run a query through the Promise API, returning the completed result by appending paginated results


