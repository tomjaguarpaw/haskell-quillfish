{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

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

type ColId = Int

data Rows a = Rows { elements   :: [a]
                   , col        :: ColId -> Q (a -> Dynamic)
                   , indices    :: [([ColId], [Dynamic] -> Q [a])] }

type Q = Either Failure

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
 
data BaseTableColumn a = BaseTableColumn Tag

data Column a where
  BaseTableAttrExpr :: BaseTableColumn a -> Column a
  Eq1 :: BaseTableColumn a -> Column a -> Column Bool
  Eq2 :: Column a -> BaseTableColumn a -> Column Bool
  Eq12 :: BaseTableColumn a -> BaseTableColumn a -> Column Bool
  Fmap :: (a -> b) -> Column a -> Column b
  Pure :: a -> Column a
  Ap :: Column (a -> b) -> Column a -> Column b

data PrimQuery = Unit
               | forall a. BaseTable (Rows a)
               | Product [NEL.NonEmpty PrimQuery]
               | Restrict [Column Bool]

type BaseTableId = Int

type Tag = (BaseTableId, ColId)

type Query a = St.State (Tag, PrimQuery) a
