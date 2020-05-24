module Test.Main where

import Data.IntMap (IntMap)
import Data.IntMap (empty, insert, lookup, delete, keys) as IntMap
import Data.MultiSet.Indexed (IxMultiSet, Index)
import Data.MultiSet.Indexed (insert, empty, lookup, delete, toUnfoldable) as IxMultiSet

import Prelude
import Data.Maybe (Maybe (..))
import Data.Tuple (Tuple (..), fst)
import Data.Either (Either (..))
import Data.Array (length, sort, nub, snoc, sortWith) as Array
import Data.Foldable (foldr)
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import Effect.Class.Console (log)
import Test.QuickCheck (quickCheckGen, arbitrary, Result (..))
import Test.QuickCheck.Gen (Gen, arrayOf)
import Data.Argonaut (encodeJson, decodeJson)
import Data.ArrayBuffer.Class (encodeArrayBuffer, decodeArrayBuffer)
import Data.ArrayBuffer.Class.Types (Int32BE (..))


main :: Effect Unit
main = do
  log "IntMap"
  log " - insert exists"
  quickCheckGen insertExistsIntMap
  log " - delete doesn't exist"
  quickCheckGen deleteDoesntExistIntMap
  log " - sorted and deduped keys have same length"
  quickCheckGen uniqueKeysIntMap
  log ""
  log "IxMultiSet"
  log " - insert exists"
  quickCheckGen insertExistsIxMultiSet
  log " - delete doesn't exist"
  quickCheckGen deleteDoesntExistIxMultiSet
  log " - has ascending keys when unfolded"
  quickCheckGen toArrayAscendingKeysIxMultiSet
  log " - json iso"
  quickCheckGen jsonIsoIxMultiSet
  log " - arraybuffer iso"
  quickCheckGen abIsoIxMultiSet


insertExistsIntMap :: Gen Result
insertExistsIntMap = do
  xs <- genIntMap
  k <- arbitrary
  a <- arbitrary
  pure $ case IntMap.lookup k (IntMap.insert k a xs) of
    Nothing -> Failed "Key doesn't exist"
    Just a'
      | a == a' -> Success
      | otherwise -> Failed $ "Values don't match - inserted: " <> show a <> ", found: " <> show a'

deleteDoesntExistIntMap :: Gen Result
deleteDoesntExistIntMap = do
  xs <- genIntMap
  k <- arbitrary
  a <- arbitrary
  pure $ case IntMap.lookup k (IntMap.delete k (IntMap.insert k a xs)) of
    Nothing -> Success
    Just a' -> Failed $ "Found value after deletion - inserted: " <> show a <> ", found: " <> show a'


uniqueKeysIntMap :: Gen Result
uniqueKeysIntMap = do
  xs <- genIntMap
  let ks = IntMap.keys xs
      ks' = Array.nub (Array.sort ks)
  pure $ if Array.length ks' == Array.length ks
          then Success
          else Failed $ "Key lengths differ after sorting and deduping - original: " <> show ks <> ", sorted: " <> show ks'


genIntMap :: Gen (IntMap Int)
genIntMap = do
  xs <- arrayOf (Tuple <$> arbitrary <*> arbitrary)
  pure (foldr (\(Tuple k x) -> IntMap.insert k x) IntMap.empty xs)


insertExistsIxMultiSet :: Gen Result
insertExistsIxMultiSet = do
  {indicies,set} <- genIxMultiSet
  k <- arbitrary
  a <- arbitrary
  let {index,set: set'} = IxMultiSet.insert k a set
  pure $ case IxMultiSet.lookup index set' of
    Nothing -> Failed "No index in map"
    Just {key,value}
      | k == key && value == a -> Success
      | otherwise -> Failed $ "Value or key doesn't match - original: " <> show {key: k, value: a} <> ", found: " <> show {key,value}


deleteDoesntExistIxMultiSet :: Gen Result
deleteDoesntExistIxMultiSet = do
  {indicies,set} <- genIxMultiSet
  k <- arbitrary
  a <- arbitrary
  let {index, set: set'} = IxMultiSet.insert k a set
      set'' = IxMultiSet.delete index set'
  pure $ case IxMultiSet.lookup index set'' of
    Nothing -> Success
    Just {key,value} -> Failed $ "Found value when shouldn't exist - original: " <> show {key: k, value: a} <> ", found: " <> show {key,value}


toArrayAscendingKeysIxMultiSet :: Gen Result
toArrayAscendingKeysIxMultiSet = do
  {indicies,set} <- genIxMultiSet
  let xs :: Array (Tuple Int (IntMap Int))
      xs = IxMultiSet.toUnfoldable set
      xs' = Array.sortWith fst xs
  pure $ if xs == xs'
          then Success
          else Failed $ "Unfolded sets don't match after sorting on keys - original: " <> show xs <> ", sorted: " <> show xs'


jsonIsoIxMultiSet :: Gen Result
jsonIsoIxMultiSet = do
  {indicies,set} <- genIxMultiSet
  pure $ case decodeJson (encodeJson set) of
    Left e -> Failed $ "Json decoding failed: " <> e
    Right set'
      | set' == set -> Success
      | otherwise -> Failed $ "Sets not equal - original: " <> show set <> ", parsed: " <> show set'

abIsoIxMultiSet :: Gen Result
abIsoIxMultiSet = do
  {indicies,set} <- genIxMultiSet'
  let ab = unsafePerformEffect (encodeArrayBuffer set)
      mSet' = unsafePerformEffect (decodeArrayBuffer ab)
  pure $ case mSet' of
    Nothing -> Failed "ArrayBuffer decoding failed"
    Just set'
      | set' == set -> Success
      | otherwise -> Failed $ "Sets not equal - original: " <> show set <> ", parsed: " <> show set'


genIxMultiSet :: Gen {set :: IxMultiSet Int Int, indicies :: Array Index}
genIxMultiSet = do
  xs <- arrayOf (Tuple <$> arbitrary <*> arbitrary)
  let go (Tuple k x) {indicies,set: set'} =
        let {index,set} = IxMultiSet.insert k x set'
        in  { indicies: Array.snoc indicies index
            , set
            }
  pure $ foldr go {set: IxMultiSet.empty, indicies: []} xs

genIxMultiSet' :: Gen {set :: IxMultiSet Int32BE Int32BE, indicies :: Array Index}
genIxMultiSet' = do
  xs <- arrayOf (Tuple <$> (Int32BE <$> arbitrary) <*> (Int32BE <$> arbitrary))
  let go (Tuple k x) {indicies,set: set'} =
        let {index,set} = IxMultiSet.insert k x set'
        in  { indicies: Array.snoc indicies index
            , set
            }
  pure $ foldr go {set: IxMultiSet.empty, indicies: []} xs
