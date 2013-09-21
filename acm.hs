{-# LANGUAGE DeriveGeneric #-}
module Main where

import Control.Monad
import Data.Maybe (fromMaybe)
import GHC.Generics (Generic)
import System.IO
import Data.List
import Data.Maybe
import Data.Hashable (Hashable)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map


data State = Root | State Int
           deriving (Eq, Generic)
instance Hashable State

type Goto char = HashMap State (HashMap char State)
type Failure = HashMap State State
type Output char = HashMap State [[char]]

data ACM char = ACM (Goto char) Failure (Output char)

acm :: (Eq char, Hashable char) => [[char]] -> ACM char
acm ps = ACM gotoMap failureMap outputMap
  where
    gotoMap = buildGoto ps
    failureMap = buildFailure gotoMap
    outputMap = buildOutput ps gotoMap failureMap

buildOutput :: (Eq char, Hashable char) => [[char]] -> Goto char -> Failure -> Output char
buildOutput ps gotoMap failureMap = foldl' build o0 $ tail $ toBFList gotoMap
  where
    build o s = foldl' (\a (_, s') -> Map.insertWith (++) s' (lookupDefault [] (failure s') a) a) o ts
      where
        ts = Map.toList $ lookupDefault Map.empty s gotoMap
    failure = fromMaybe (error "failure: ") . flip Map.lookup failureMap
    o0 = Map.fromList $ zip patStates (map (:[]) ps)
    patStates = fromJust $ mapM (finalState gotoMap Root) ps

finalState :: (Eq char, Hashable char) => Goto char -> State -> [char] -> Maybe State
finalState m = foldM (\s x -> Map.lookup s m >>= Map.lookup x)

buildGoto :: (Eq char, Hashable char) => [[char]] -> Goto char
buildGoto = foldl' (flip Main.insert) Map.empty

insert :: (Eq char, Hashable char) => [char] -> Goto char -> Goto char
insert = go Root
  where
    go _ [] m = m
    go s (x:xs) m = case Map.lookup x sm of
        Nothing -> go s' xs m'
          where
            s' = State $ Map.foldl' (\a -> (a +) . Map.size) 0 m + 1  -- root is 0
            sm' = Map.insert x s' sm
            m' = Map.insert s sm' m
        Just s' -> go s' xs m
      where
        sm = fromMaybe Map.empty $ Map.lookup s m

buildFailure :: (Eq char, Hashable char) => Goto char -> Failure
buildFailure m = foldl' build Map.empty $ toBFList m
  where
    build f s = foldl' (\a (x, s') -> Map.insert s' (failureState f s x) a) f ts
      where
        ts = Map.toList $ lookupDefault Map.empty s m
    failureState _ Root _ = Root
    failureState f s x = head $ mapMaybe (flip goto x) $ iterate failure (failure s)
      where
        failure = fromMaybe (error "failure: ") . flip Map.lookup f
    goto Root x = Just $ fromMaybe Root $ Map.lookup Root m >>= Map.lookup x
    goto s x = Map.lookup s m >>= Map.lookup x

toBFList :: Goto char -> [State]
toBFList m = ss0
  where
    ss0 = Root : go 1 ss0
    go 0 _ = []
    go n (s:ss) = case msm of
        Nothing -> go (n - 1) ss
        Just sm -> children ++ go (n - 1 + Map.size sm) ss
          where
            children = Map.elems sm
      where
        msm = Map.lookup s m
    go _ _ = error "toBFList: invalid state"

lookupDefault :: (Eq k, Hashable k) => v -> k -> HashMap k v -> v
lookupDefault def k m = fromMaybe def $ Map.lookup k m

main :: IO ()
main = do
    return ()
