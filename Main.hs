{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}

import Data.Typeable
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Monoid
import Data.Proxy
import qualified Data.Foldable
import Debug.Trace
import qualified Data.List.NonEmpty as NEL
import qualified Control.Monad.Trans.State as St

data Raw = forall a. Typeable a => Raw a

type Failure = String

type ColId = Int

data Rows a = Rows { elements   :: [a]
                   , col        :: ColId -> Q (a -> Raw)
                   , indices    :: [(ColId, Raw)] -> Q [a] }

type Q = Either Failure

castErr :: (Typeable a, Typeable b) => String -> a -> Q b
castErr err a = case cast a of
  Nothing -> Left ("Cast error: " <> err)
  Just b  -> return b

rowsOfMap :: (Ord k, Typeable k, Typeable v, Eq v) => Map k v -> Rows (k, v)
rowsOfMap m = Rows { elements = Map.toList m
                   , col    = \i -> if i == 0 then return (Raw . fst)
                                    else if i == 1 then return (Raw . snd)
                                         else Left ("Got " <> show i
                                                    <> " as index into a map")
                   , indices = \irs -> case firstMatch ((== 0) . fst) irs of
                       Nothing -> scanMap irs (Map.toList m)
                       Just ((_, Raw r), as) -> do
                         k <- castErr "index key" r
                         (scanMap as
                          . fmap (\v -> (k, v))
                          . Data.Foldable.toList
                          . Map.lookup k) m
                   }

firstMatch :: (a -> Bool) -> [a] -> Maybe (a, [a])
firstMatch _ []     = Nothing
firstMatch p (a:as) = if p a
                      then Just (a, as)
                      else fmap (\(b, bs) -> (b, a:bs)) (firstMatch p as)

scanMap :: (Eq k, Eq v, Typeable k, Typeable v) =>
           [(ColId, Raw)] -> [(k, v)] -> Q [(k, v)]
scanMap irs l = fmap ($ l) (fmap concatEndo (mapM scan irs))

concatEndo :: [a -> a] -> a -> a
concatEndo = foldr (.) id

scan :: (Eq k, Eq v, Typeable k, Typeable v) =>
        (ColId, Raw) -> Q ([(k, v)] -> [(k, v)])
scan (i, Raw r) = if i == 0
              then do
                    r' <- castErr "scan key 1" r
                    return (filter ((== r') . fst))
              else if i == 1
                   then do
                     r' <- castErr "scan key 2" r
                     return (filter ((== r') . snd))
              else Left "Got a wrong key index"


join :: [a] -> Rows b -> [(a -> Raw, ColId)] -> Q [(a, b)]
join as rs fs = (fmap concat
                 . sequence
                 . map (\a -> (fmap.fmap) (\b -> (a, b)) (indices rs (map (\(f, i) -> (i, f a)) fs))))
                as

test = join as (rowsOfMap (Map.fromList (zip as as))) [(Raw, 0)]
  where as = [1..1000000] :: [Int]
 
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
