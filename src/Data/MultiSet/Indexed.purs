module Data.MultiSet.Indexed where

import Prelude
import Data.Maybe (Maybe (..))
import Data.Tuple (Tuple (..))
import Data.Unfoldable (class Unfoldable)
import Data.Foldable (class Foldable, foldMap, foldr, foldl)
import Data.Array (foldr, sort) as Array
import Data.IntMap (IntMap)
import Data.IntMap (insert, union, empty, lookup, keys, delete, isEmpty, values) as IntMap
import Data.Map (Map)
import Data.Map (insertWith, lookup, empty, delete, insert, toUnfoldable, fromFoldable) as Map
import Data.Generic.Rep (class Generic)
import Data.Argonaut (class EncodeJson, class DecodeJson, encodeJson, decodeJson)
import Data.ArrayBuffer.Class
  ( class DynamicByteLength, class EncodeArrayBuffer, class DecodeArrayBuffer
  , byteLength, putArrayBuffer, readArrayBuffer)
import Test.QuickCheck (class Arbitrary, arbitrary)


type Index = Int

newtype IxMultiSet k a = IxMultiSet
  { mapping    :: Map k (IntMap a)
  , keyMapping :: IntMap k
  , nextIndex  :: Index
  }
derive instance genericIxMultiSet :: (Generic k k', Generic a a') => Generic (IxMultiSet k a) _
instance eqIxMultiSet :: (Eq k, Ord a) => Eq (IxMultiSet k a) where
  eq set1 set2 =
    let xs :: Array _
        xs = map sortValues (toUnfoldable' set1)
        ys :: Array _
        ys = map sortValues (toUnfoldable' set2)
        sortValues (Tuple k vs) = Tuple k (Array.sort vs)
    in  xs == ys
derive newtype instance ordIxMultiSet :: (Ord k, Ord a) => Ord (IxMultiSet k a)
instance showIxMultiSet :: (Show k, Show a, Ord a) => Show (IxMultiSet k a) where
  show set =
    let xs :: Array _
        xs = map sortValues (toUnfoldable' set)
        sortValues (Tuple k vs) = Tuple k (Array.sort vs)
    in  show xs
instance functorIxMultiSet :: Functor (IxMultiSet k) where
  map f (IxMultiSet {mapping,keyMapping,nextIndex}) =
    IxMultiSet
    { mapping: map (map f) mapping
    , keyMapping
    , nextIndex
    }
instance foldableIxMultiSet :: Foldable (IxMultiSet k) where
  foldMap f (IxMultiSet {mapping}) = foldMap (foldMap f) mapping
  foldr f acc (IxMultiSet {mapping}) = foldr go acc mapping
    where
      go valueMapping acc' = foldr f acc' valueMapping
  foldl f acc (IxMultiSet {mapping}) = foldl go acc mapping
    where
      go acc' valueMapping = foldl f acc' valueMapping
instance encodeJsonIxMultiSet :: (EncodeJson k, EncodeJson a) => EncodeJson (IxMultiSet k a) where
  encodeJson set =
    let xs :: Array _
        xs = toUnfoldable' set
    in  encodeJson (map (\(Tuple key values) -> {key,values}) xs)
instance decodeJsonIxMultiSet :: (DecodeJson k, DecodeJson a, Ord k) => DecodeJson (IxMultiSet k a) where
  decodeJson json = do
    (xs :: Array {key :: k, values :: Array a}) <- decodeJson json
    let go {key,values} = Tuple key values
    pure (fromFoldable (map go xs))
instance dynamicByteLengthIxMultiSet :: (DynamicByteLength k, DynamicByteLength a) => DynamicByteLength (IxMultiSet k a) where
  byteLength set =
    let xs :: Array _
        xs = toUnfoldable' set
    in  byteLength xs
instance encodeArrayBufferIxMultiSet :: (EncodeArrayBuffer k, EncodeArrayBuffer a) => EncodeArrayBuffer (IxMultiSet k a) where
  putArrayBuffer b o set =
    let xs :: Array _
        xs = toUnfoldable' set
    in  putArrayBuffer b o xs
instance decodeArrayBufferIxMultiSet ::
  ( DynamicByteLength k, DynamicByteLength a
  , Ord k
  , DecodeArrayBuffer k, DecodeArrayBuffer a) => DecodeArrayBuffer (IxMultiSet k a) where
  readArrayBuffer b o = do
    (mxs :: Maybe (Array _)) <- readArrayBuffer b o
    case mxs of
      Nothing -> pure Nothing
      Just xs -> pure (Just (fromFoldable xs))
instance arbitraryIxMultiSet :: (Arbitrary k, Arbitrary a, Ord k) => Arbitrary (IxMultiSet k a) where
  arbitrary = fromFoldable <$> (arbitrary :: _ (Array _))



empty :: forall k a. IxMultiSet k a
empty = IxMultiSet
  { mapping: Map.empty
  , keyMapping: IntMap.empty
  , nextIndex: 0
  }

insert :: forall k a. Ord k => k -> a -> IxMultiSet k a -> {index :: Index, set :: IxMultiSet k a}
insert key value (IxMultiSet {mapping,keyMapping,nextIndex}) =
  { index: nextIndex
  , set: IxMultiSet
    { mapping:
      Map.insertWith
        (flip IntMap.union)
        key
        (IntMap.insert nextIndex value IntMap.empty)
        mapping
    , keyMapping: IntMap.insert nextIndex key keyMapping
    , nextIndex: nextIndex + 1
    }
  }

lookupAll :: forall k a. Ord k => k -> IxMultiSet k a -> Maybe (IntMap a)
lookupAll key (IxMultiSet {mapping}) = Map.lookup key mapping

lookup :: forall k a. Ord k => Index -> IxMultiSet k a -> Maybe {key :: k, value :: a}
lookup index (IxMultiSet {mapping,keyMapping}) = case IntMap.lookup index keyMapping of
  Nothing -> Nothing
  Just key -> case Map.lookup key mapping of
    Nothing -> Nothing
    Just valueMapping -> case IntMap.lookup index valueMapping of
      Nothing -> Nothing
      Just value -> Just {key,value}

deleteAll :: forall k a. Ord k => k -> IxMultiSet k a -> IxMultiSet k a
deleteAll key orig@(IxMultiSet {mapping,keyMapping,nextIndex}) = case Map.lookup key mapping of
  Nothing -> orig
  Just valueMapping ->
    let indicies = IntMap.keys valueMapping
    in  IxMultiSet
        { mapping: Map.delete key mapping
        , keyMapping: Array.foldr IntMap.delete keyMapping indicies
        , nextIndex
        }

delete :: forall k a. Ord k => Index -> IxMultiSet k a -> IxMultiSet k a
delete index orig@(IxMultiSet {mapping,keyMapping,nextIndex}) = case IntMap.lookup index keyMapping of
  Nothing -> orig
  Just key -> case Map.lookup key mapping of
    Nothing -> orig
    Just valuesMapping ->
      let newValuesMapping = IntMap.delete index valuesMapping
      in  if IntMap.isEmpty newValuesMapping
            then IxMultiSet
                 { mapping: Map.delete key mapping
                 , keyMapping: IntMap.delete index keyMapping -- should be last index for key
                 , nextIndex
                 }
            else IxMultiSet
                 { mapping: Map.insert key newValuesMapping mapping
                 , keyMapping: IntMap.delete index keyMapping -- remove from key index
                 , nextIndex
                 }

toUnfoldable :: forall k a f. Unfoldable f => IxMultiSet k a -> f (Tuple k (IntMap a))
toUnfoldable (IxMultiSet {mapping}) = Map.toUnfoldable mapping

toUnfoldable' :: forall k a f. Unfoldable f => Functor f => IxMultiSet k a -> f (Tuple k (Array a))
toUnfoldable' set = map (\(Tuple key values) -> Tuple key (IntMap.values values)) (toUnfoldable set)

fromFoldable :: forall k a f. Foldable f => Ord k => f (Tuple k (Array a)) -> IxMultiSet k a
fromFoldable xs =
    let go (Tuple key values) acc =
          let go' v acc' =
                let {set} = insert key v acc' -- ignore index
                in  set
          in  Array.foldr go' acc values
    in  foldr go empty xs


mapKeys :: forall k k' a. Ord k' => (k -> k') -> IxMultiSet k a -> IxMultiSet k' a
mapKeys f (IxMultiSet {mapping,keyMapping,nextIndex}) = IxMultiSet
  { mapping:
    let xs :: Array _
        xs = Map.toUnfoldable mapping
    in  Map.fromFoldable (map (\(Tuple k vs) -> Tuple (f k) vs) xs)
  , keyMapping: map f keyMapping
  , nextIndex
  }


eqExact :: forall k a. Eq a => Eq k => IxMultiSet k a -> IxMultiSet k a -> Boolean
eqExact (IxMultiSet x) (IxMultiSet y) =
  x.mapping == y.mapping
  && x.keyMapping == y.keyMapping
  && x.nextIndex == y.nextIndex

showExact :: forall k a. Show k => Show a => IxMultiSet k a -> String
showExact (IxMultiSet {mapping}) = show mapping
