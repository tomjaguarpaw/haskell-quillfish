{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Data.Typeable
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Monoid
import Data.Proxy
import qualified Data.Foldable
import Debug.Trace

data Raw = forall a. Typeable a => Raw a

type Failure = String

data Rows a = Rows { elements   :: [a]
                   , col        :: Int -> Q (a -> Raw)
                   , indices    :: [(Int, Raw)] -> Q [a] }

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
           [(Int, Raw)] -> [(k, v)] -> Q [(k, v)]
scanMap irs l = fmap ($ l) (fmap concatEndo (mapM scan irs))

concatEndo :: [a -> a] -> a -> a
concatEndo = foldr (.) id

scan :: (Eq k, Eq v, Typeable k, Typeable v) =>
        (Int, Raw) -> Q ([(k, v)] -> [(k, v)])
scan (i, Raw r) = if i == 0
              then do
                    r' <- castErr "scan key 1" r
                    return (filter ((== r') . fst))
              else if i == 1
                   then do
                     r' <- castErr "scan key 2" r
                     return (filter ((== r') . snd))
              else Left "Got a wrong key index"


join :: [a] -> Rows b -> [(a -> Raw, Int)] -> Q [(a, b)]
join as rs fs = (fmap concat
                 . sequence
                 . map (\a -> (fmap.fmap) (\b -> (a, b)) (indices rs (map (\(f, i) -> (i, f a)) fs))))
                as

test = join as (rowsOfMap (Map.fromList (zip as as))) [(Raw, 0)]
  where as = [1..1000000] :: [Int]
