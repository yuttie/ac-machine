{-# LANGUAGE DeriveGeneric #-}
module AhoCorasick
    ( ACMachine
    , State(..)
    , Match(..)
    , construct
    , run
    , step
    , renderGraph
    ) where

import           Control.Monad
import           Data.Hashable       (Hashable)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
import           Data.List
import           Data.Maybe
import           GHC.Generics        (Generic)


data ACMachine a v = ACMachine (Goto a) Failure (Output v)

type Goto a   = HashMap State (HashMap a State)
type Failure  = HashMap State State
type Output v = HashMap State [(Int, v)]

data State = Root | State Int
           deriving (Eq, Generic)
instance Hashable State

data Match v = Match
    { matchPos   :: Int
    , matchValue :: v
    } deriving (Show)

construct :: (Eq a, Hashable a) => [[a]] -> ACMachine a [a]
construct ps = ACMachine gotoMap failureMap outputMap
  where
    gotoMap = buildGoto ps
    failureMap = buildFailure gotoMap
    outputMap = buildOutput pvs gotoMap failureMap
    pvs = zip ps ps

run :: (Eq a, Hashable a) => ACMachine a v -> [a] -> [Match v]
run acm = go Root . zip [1..]
  where
    go _ [] = []
    go s ((i, x):ixs) = map toMatch vs ++ go s' ixs
      where
        toMatch (l, v) = Match { matchPos = i - l + 1, matchValue = v }
        (s', vs) = step acm x s

step :: (Eq a, Hashable a) => ACMachine a v -> a -> State -> (State, [(Int, v)])
step (ACMachine g f o) x s = (s', output s')
  where
    s' = head $ mapMaybe (flip goto x) $ iterate failure s
    goto Root x' = Just $ fromMaybe Root $ Map.lookup Root g >>= Map.lookup x'
    goto s'' x' = Map.lookup s'' g >>= Map.lookup x'
    failure = fromMaybe (error "failure: ") . flip Map.lookup f
    output = fromMaybe [] . flip Map.lookup o

buildOutput :: (Eq a, Hashable a) => [([a], v)] -> Goto a -> Failure -> Output v
buildOutput pvs gotoMap failureMap = foldl' build o0 $ tail $ toBFList gotoMap
  where
    build o s = foldl' (\a (_, s') -> Map.insertWith (flip (++)) s' (lookupDefault [] (failure s') a) a) o ts
      where
        ts = Map.toList $ lookupDefault Map.empty s gotoMap
    failure = fromMaybe (error "failure: ") . flip Map.lookup failureMap
    o0 = Map.fromList $ fromJust $ mapM toKV pvs
    toKV (p, v) = do
        s <- finalState gotoMap Root p
        return (s, [(length p, v)])

finalState :: (Eq a, Hashable a) => Goto a -> State -> [a] -> Maybe State
finalState m = foldM (\s x -> Map.lookup s m >>= Map.lookup x)

buildGoto :: (Eq a, Hashable a) => [[a]] -> Goto a
buildGoto = foldl' (flip extend) Map.empty

extend :: (Eq a, Hashable a) => [a] -> Goto a -> Goto a
extend = go Root
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

buildFailure :: (Eq a, Hashable a) => Goto a -> Failure
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

toBFList :: Goto a -> [State]
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

renderGraph :: ACMachine Char [Char] -> String
renderGraph (ACMachine g f o) =
    graph "digraph" $ statements [
          attr "graph" [("rankdir", "LR")]
        , statements $ map state (toBFList g)
        , statements $ map stateWithOutput $ filter (not . null . snd) $ Map.toList o
        , statements $ map (\s -> statements $ map (uncurry $ transEdge s) $ Map.toList $ lookupDefault Map.empty s g) (toBFList g)
        , statements $ map (\s -> failEdge s $ failure s) (tail $ toBFList g)
        ]
  where
    failure = fromMaybe (error "failure: ") . flip Map.lookup f
    statements = intercalate " "
    graph typ body = typ ++ " { " ++ body ++ " }"
    attr typ attrList = typ ++ " " ++ "[" ++ intercalate "," (map kvStr attrList) ++ "];"
    node nid attrList = nid ++ " " ++ "[" ++ intercalate "," (map kvStr attrList) ++ "];"
    kvStr (k, v) = k ++ "=" ++ v
    state s@Root = node (stateID s) [("shape", "doublecircle")]
    state s = node (stateID s) [("shape", "circle")]
    stateWithOutput (s, xs) = node (stateID s) [("label", "<" ++ tableHTML (stateID s) ("{" ++ intercalate "," (map snd xs) ++ "}") ++ ">"), ("shape", "none")]
    tableHTML row1 row2 = "<table cellborder=\"0\"><tr><td>" ++ row1 ++ "</td></tr><tr><td>" ++ row2 ++ "</td></tr></table>"
    stateID Root = "Root"
    stateID (State n) = 'S' : show n
    transEdge s x s' = stateID s ++ " -> " ++ stateID s' ++ " [label=\"" ++ [x] ++ "\"];"
    failEdge s s' = stateID s ++ " -> " ++ stateID s' ++ " [style=dashed, constraint=false];"
