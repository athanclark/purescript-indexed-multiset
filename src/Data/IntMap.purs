module Data.IntMap where

import Prelude
import Foreign.Object (Object)
import Foreign.Object (insert, delete, lookup, toAscUnfoldable, empty, union, keys, isEmpty) as O
import Data.Maybe (Maybe, fromJust)
import Data.Tuple (Tuple (..))
import Data.Eq (class Eq1)
import Data.FunctorWithIndex (class FunctorWithIndex, mapWithIndex)
import Data.Foldable (class Foldable)
import Data.FoldableWithIndex (class FoldableWithIndex, foldMapWithIndex, foldrWithIndex, foldlWithIndex)
import Data.Traversable (class Traversable)
import Data.TraversableWithIndex (class TraversableWithIndex, traverseWithIndexDefault)
import Data.Unfoldable (class Unfoldable)
import Data.Int.Parse (parseInt, toRadix)
import Data.Generic.Rep (class Generic)
import Partial.Unsafe (unsafePartial)


newtype IntMap a = IntMap (Object a)
derive instance genericIntMap :: Generic a a' => Generic (IntMap a) _
derive newtype instance functorIntMap :: Functor IntMap
instance functorWithIndexIntMap :: FunctorWithIndex Int IntMap where
  mapWithIndex f (IntMap xs) = IntMap (mapWithIndex (f <<< getKey) xs)
    where
      getKey k = unsafePartial (fromJust (parseInt k (toRadix 10)))
derive newtype instance foldableIntMap :: Foldable IntMap
instance foldableWithIndexIntMap :: FoldableWithIndex Int IntMap where
  foldMapWithIndex f (IntMap xs) = foldMapWithIndex (f <<< getKey) xs
    where
      getKey k = unsafePartial (fromJust (parseInt k (toRadix 10)))
  foldrWithIndex f acc (IntMap xs) = foldrWithIndex (f <<< getKey) acc xs
    where
      getKey k = unsafePartial (fromJust (parseInt k (toRadix 10)))
  foldlWithIndex f acc (IntMap xs) = foldlWithIndex (f <<< getKey) acc xs
    where
      getKey k = unsafePartial (fromJust (parseInt k (toRadix 10)))
derive newtype instance traversableIntMap :: Traversable IntMap
instance traversableWithIndexIntMap :: TraversableWithIndex Int IntMap where
  traverseWithIndex = traverseWithIndexDefault
derive newtype instance eqIntMap :: Eq a => Eq (IntMap a)
derive newtype instance eq1IntMap :: Eq1 IntMap
derive newtype instance ordIntMap :: Ord a => Ord (IntMap a)
derive newtype instance showIntMap :: Show a => Show (IntMap a)
derive newtype instance semigroupIntMap :: Semigroup a => Semigroup (IntMap a)
derive newtype instance monoidIntMap :: Semigroup a => Monoid (IntMap a)

insert :: forall a. Int -> a -> IntMap a -> IntMap a
insert k x (IntMap xs) = IntMap (O.insert (show k) x xs)

delete :: forall a. Int -> IntMap a -> IntMap a
delete k (IntMap xs) = IntMap (O.delete (show k) xs)

lookup :: forall a. Int -> IntMap a -> Maybe a
lookup k (IntMap xs) = O.lookup (show k) xs

empty :: forall a. IntMap a
empty = IntMap O.empty

isEmpty :: forall a. IntMap a -> Boolean
isEmpty (IntMap xs) = O.isEmpty xs

union :: forall a. IntMap a -> IntMap a -> IntMap a
union (IntMap x) (IntMap y) = IntMap (O.union x y)

keys :: forall a. IntMap a -> Array Int
keys (IntMap xs) = getKey <$> O.keys xs
  where
    getKey k = unsafePartial (fromJust (parseInt k (toRadix 10)))

toUnfoldable :: forall f a. Unfoldable f => Functor f => IntMap a -> f (Tuple Int a)
toUnfoldable (IntMap xs) = map (\(Tuple k x) -> Tuple (getKey k) x) (O.toAscUnfoldable xs)
  where
    getKey k = unsafePartial (fromJust (parseInt k (toRadix 10)))
