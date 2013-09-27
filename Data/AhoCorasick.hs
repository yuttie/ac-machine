{-# LANGUAGE DeriveGeneric #-}
module Data.AhoCorasick
    ( ACMachine
    , State(..)
    , Match(..)
    , construct
    , constructWithValues
    , root
    , run
    , step
    , renderGraph
    ) where

import           Control.Applicative
import           Control.Monad
import           Data.Hashable       (Hashable)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
import           Data.List
import           Data.Maybe
import           Data.Vector         (Vector)
import qualified Data.Vector         as V
import           GHC.Generics        (Generic)


data ACMachine a v = ACMachine (Goto a) Failure (Output v)

type Goto a   = Vector (HashMap a State)
type Failure  = Vector State
type Output v = Vector [(Int, v)]

type GotoMap a   = HashMap State (HashMap a State)
type FailureMap  = HashMap State State
type OutputMap v = HashMap State [(Int, v)]

newtype State = State Int
              deriving (Eq, Generic)
instance Hashable State

data Match v = Match
    { matchPos   :: Int
    , matchValue :: v
    } deriving (Show)

construct :: (Eq a, Hashable a) => [[a]] -> ACMachine a [a]
construct ps = constructWithValues $ zip ps ps

constructWithValues :: (Eq a, Hashable a) => [([a], v)] -> ACMachine a v
constructWithValues pvs = ACMachine g (toFailureArray n failureMap) (toOutputArray n outputMap)
  where
    (m, gotoMap) = buildGoto ps
    n = m + 1
    g = toGotoArray n gotoMap
    failureMap = buildFailure g
    outputMap = buildOutput pvs g failureMap
    ps = map fst pvs

toGotoArray :: Int -> GotoMap a -> Goto a
toGotoArray n m = V.generate n (fromMaybe Map.empty . flip Map.lookup m . State)

toFailureArray :: Int -> FailureMap -> Failure
toFailureArray n m = V.generate n (fromMaybe (error "failure: ") . flip Map.lookup m . State)

toOutputArray :: Int -> OutputMap v -> Output v
toOutputArray n m = V.generate n (fromMaybe [] . flip Map.lookup m . State)

root :: State
root = State 0

run :: (Eq a, Hashable a) => ACMachine a v -> [a] -> [Match v]
run acm = go root . zip [1..]
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
    goto (State 0) x' = Map.lookup x' (g V.! 0) <|> Just root
    goto (State i) x' = Map.lookup x' (g V.! i)
    failure (State i) = f V.! i
    output (State i) = o V.! i

buildOutput :: (Eq a, Hashable a) => [([a], v)] -> Goto a -> FailureMap -> OutputMap v
buildOutput pvs g failureMap = foldl' build o0 $ tail $ toBFList g
  where
    build o (State i) = foldl' (\a (_, s') -> let vs = lookupDefault [] (failure s') a in vs `seq` Map.insertWith (flip (++)) s' vs a) o ts
      where
        ts = Map.toList (g V.! i)
    failure = fromMaybe (error "failure: ") . flip Map.lookup failureMap
    o0 = Map.fromList $ map (fromJust . toKV) pvs
    toKV (p, v) = do
        s <- finalState g root p
        return (s, [(length p, v)])

finalState :: (Eq a, Hashable a) => Goto a -> State -> [a] -> Maybe State
finalState g = foldM (\(State i) x -> Map.lookup x (g V.! i))

buildGoto :: (Eq a, Hashable a) => [[a]] -> (Int, GotoMap a)
buildGoto = foldl' (flip extend) (0, Map.empty)

extend :: (Eq a, Hashable a) => [a] -> (Int, GotoMap a) -> (Int, GotoMap a)
extend = go root
  where
    go _ [] nm = nm
    go s (x:xs) nm@(n, m) = case Map.lookup x sm of
        Nothing -> go s' xs (n', m')
          where
            s' = State n'  -- root is 0
            n' = n + 1
            sm' = Map.insert x s' sm
            m' = Map.insert s sm' m
        Just s' -> go s' xs nm
      where
        sm = fromMaybe Map.empty $ Map.lookup s m

buildFailure :: (Eq a, Hashable a) => Goto a -> FailureMap
buildFailure g = foldl' build Map.empty $ toBFList g
  where
    build f s@(State i) = foldl' (\a (x, s') -> Map.insert s' (failureState f s x) a) f ts
      where
        ts = Map.toList (g V.! i)
    failureState _ (State 0) _ = root
    failureState f s x = head $ mapMaybe (flip goto x) $ iterate failure (failure s)
      where
        failure = fromMaybe (error "failure: ") . flip Map.lookup f
    goto (State 0) x = Map.lookup x (g V.! 0) <|> Just root
    goto (State i) x = Map.lookup x (g V.! i)

toBFList :: Goto a -> [State]
toBFList g = ss0
  where
    ss0 = root : go 1 ss0
    go 0 _ = []
    go n (State i : ss) = children ++ go (n - 1 + Map.size sm) ss
      where
        sm = g V.! i
        children = Map.elems sm
    go _ _ = error "toBFList: invalid state"

lookupDefault :: (Eq k, Hashable k) => v -> k -> HashMap k v -> v
lookupDefault def k m = fromMaybe def $ Map.lookup k m

renderGraph :: ACMachine Char [Char] -> String
renderGraph (ACMachine g f o) =
    graph "digraph" $ statements [
          attr "graph" [("rankdir", "LR")]
        , statements $ map state (toBFList g)
        , statements $ map stateWithOutput $ filter (not . null . snd) $ zip (map State [0..]) (V.toList o)
        , statements $ map (\s@(State i) -> statements $ map (uncurry $ transEdge s) $ Map.toList $ g V.! i) (toBFList g)
        , statements $ map (\s@(State i) -> failEdge s $ f V.! i) (tail $ toBFList g)
        ]
  where
    statements = intercalate " "
    graph typ body = typ ++ " { " ++ body ++ " }"
    attr typ attrList = typ ++ " " ++ "[" ++ intercalate "," (map kvStr attrList) ++ "];"
    node nid attrList = nid ++ " " ++ "[" ++ intercalate "," (map kvStr attrList) ++ "];"
    kvStr (k, v) = k ++ "=" ++ v
    state s@(State 0) = node (stateID s) [("shape", "doublecircle")]
    state s = node (stateID s) [("shape", "circle")]
    stateWithOutput (s, xs) = node (stateID s) [("label", "<" ++ tableHTML (stateID s) ("{" ++ intercalate "," (map snd xs) ++ "}") ++ ">"), ("shape", "none")]
    tableHTML row1 row2 = "<table cellborder=\"0\"><tr><td>" ++ row1 ++ "</td></tr><tr><td>" ++ row2 ++ "</td></tr></table>"
    stateID (State 0) = "Root"
    stateID (State n) = 'S' : show n
    transEdge s x s' = stateID s ++ " -> " ++ stateID s' ++ " [label=\"" ++ [x] ++ "\"];"
    failEdge s s' = stateID s ++ " -> " ++ stateID s' ++ " [style=dashed, constraint=false];"
