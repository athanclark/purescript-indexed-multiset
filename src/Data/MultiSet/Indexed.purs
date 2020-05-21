module Data.MultiSet.Indexed where

import Prelude
import Data.Maybe (Maybe (..))
import Data.Tuple (Tuple)
import Data.Unfoldable (class Unfoldable)
import Data.Foldable (class Foldable, foldMap, foldr, foldl)
import Data.IntMap (IntMap)
import Data.IntMap (insert, union, empty, lookup, keys, delete, isEmpty) as IntMap
import Data.Map (Map)
import Data.Map (insertWith, lookup, empty, delete, insert, toUnfoldable) as Map
import Data.Generic.Rep (class Generic)


type Index = Int

newtype IxMultiSet k a = IxMultiSet
  { mapping    :: Map k (IntMap a)
  , keyMapping :: IntMap k
  , nextIndex  :: Index
  }
derive instance genericIxMultiSet :: (Generic k k', Generic a a') => Generic (IxMultiSet k a) _
derive newtype instance eqIxMultiSet :: (Eq k, Eq a) => Eq (IxMultiSet k a)
derive newtype instance ordIxMultiSet :: (Ord k, Ord a) => Ord (IxMultiSet k a)
instance showIxMultiSet :: (Show k, Show a) => Show (IxMultiSet k a) where
  show (IxMultiSet {mapping}) = show mapping
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
        , keyMapping: foldr IntMap.delete keyMapping indicies
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
