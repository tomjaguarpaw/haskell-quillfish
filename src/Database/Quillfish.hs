{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Database.Quillfish where

import Data.Dynamic (Dynamic, fromDynamic, toDyn)
import qualified Data.Dynamic as Dynamic
import Data.Typeable (Typeable)
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Monoid
import Data.Proxy
import qualified Data.Foldable
import Debug.Trace
import qualified Data.List.NonEmpty as NEL
import qualified Control.Monad.Trans.State as St

type Failure = String
type Q = Either Failure

type ColId = Int

-- `Rows` is quillfish's "base table" or "base relation" type.
--
-- A `Rows` contains a list of the row values, a means of extracting
-- each of the column values, and one or more indexes for looking up
-- rows based on keys.
--
-- The rows have some type `a` and columns can be projected out by
-- providing a `ColId` reference into the row.  The type of this
-- projection operation is "too large" in the sense that we can easily
-- ask for a `ColId` that doesn't actually exist in the row.  However,
-- a higher level API will ensure that we only ever ask for valid
-- columns.
--
-- Indexes are provided in a list containing pairs matching the
-- `ColId`s pertaining to the index keys to the functions which take
-- the key values and yield the row or rows matching that key.  For
-- example, for `Rows (k, v)` backed by a `Data.Map k v` there will be
-- two indexes.  The first is the normal boring one which just yields
-- all rows.  Its index entry is of the form
--
--     ([], f)
--
-- where `f` expects to receive an empty list as an argument and
-- returns all the rows.  Again the type of the indices is "too big".
-- If `f` receives a non-empty argument it will fail.  The second
-- index does a much more efficient lookup based on the value of `k`.
-- The index entry is of the form
--
--    ([0], g)
--
-- where `[0]` indicates that `g` expects to be passed a singleton
-- list containing the value of `ColId` `0`, i.e. the key `k`.
--
data Rows a = Rows { elements   :: [a]
                   , col        :: ColId -> Q (a -> Dynamic)
                   , indices    :: [([ColId], [Dynamic] -> Q [a])] }

castErr :: Typeable b => String -> Dynamic -> Q b
castErr err a = case Dynamic.fromDynamic a of
  Nothing -> Left ("Cast error: " <> err)
  Just b  -> return b

rowsOfMap :: (Ord k, Typeable k, Typeable v, Eq v) => Map k v -> Rows (k, v)
rowsOfMap m = Rows { elements = Map.toList m
                   , col    = \i -> if i == 0 then
                                      return (toDyn . fst)
                                    else if i == 1 then
                                      return (toDyn . snd)
                                    else
                                      Left ("Got " <> show i
                                            <> " as index into a map")
                   , indices = [ ([], \case [] -> return (Map.toList m)
                                            _  -> Left "Was expecting an empty argument")
                               , ([0], \case [kDyn] -> do
                                               k <- castErr "Map index key" kDyn
                                               (return
                                                . fmap (\v -> (k, v))
                                                . Data.Foldable.toList
                                                . Map.lookup k) m)
                               ]
                   }

data BaseTableColumn a = BaseTableColumn Tag

data Column a where
  BaseTableAttrExpr :: BaseTableColumn a -> Column a
  Eq1 :: BaseTableColumn a -> Column a -> Column Bool
  Eq2 :: Column a -> BaseTableColumn a -> Column Bool
  Eq12 :: BaseTableColumn a -> BaseTableColumn a -> Column Bool
  Fmap :: (a -> b) -> Column a -> Column b
  Pure :: a -> Column a
  Ap :: Column (a -> b) -> Column a -> Column b

-- The challenge is to write an optimized evaluator for `PrimQuery`
-- that allows indexes to be used wherever possible.
data PrimQuery = Unit
               | forall a. BaseTable (Rows a)
               | Product [NEL.NonEmpty PrimQuery]
               | Restrict [Column Bool]

type BaseTableId = Int

type Tag = (BaseTableId, ColId)

type Query a = St.State (Tag, PrimQuery) a



-- Old stuff.  Probably useless

firstMatch :: (a -> Bool) -> [a] -> Maybe (a, [a])
firstMatch _ []     = Nothing
firstMatch p (a:as) = if p a
                      then Just (a, as)
                      else fmap (\(b, bs) -> (b, a:bs)) (firstMatch p as)

scanMap :: (Eq k, Eq v, Typeable k, Typeable v) =>
           [(ColId, Dynamic)] -> [(k, v)] -> Q [(k, v)]
scanMap irs l = fmap ($ l) (fmap concatEndo (mapM scan irs))

concatEndo :: [a -> a] -> a -> a
concatEndo = foldr (.) id

scan :: (Eq k, Eq v, Typeable k, Typeable v) =>
        (ColId, Dynamic) -> Q ([(k, v)] -> [(k, v)])
scan (i, r) = if i == 0 then do
                    r' <- castErr "scan key 1" r
                    return (filter ((== r') . fst))
              else if i == 1 then do
                    r' <- castErr "scan key 2" r
                    return (filter ((== r') . snd))
              else Left "Got a wrong key index"

{-
join :: [a] -> Rows b -> [(a -> Dynamic, ColId)] -> Q [(a, b)]
join as rs fs = (fmap concat
                 . sequence
                 . map (\a -> (fmap.fmap) (\b -> (a, b)) (indices rs (map (\(f, i) -> (i, f a)) fs))))
                as

test = join as (rowsOfMap (Map.fromList (zip as as))) [(toDyn, 0)]
  where as = [1..1000000] :: [Int]
-}
 
