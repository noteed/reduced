-- @
--     $ ghci -isrc/ bin/reduced.hs
--
--     $ dot -Nfontname="DejaVu Sans Mono" -Nfontsize=20 -Tsvg example.dot -o example.svg
--     $ kitten icat example.svg
-- @

import Reduced

import Control.Monad.State (State)
import Control.Monad.State qualified as State
import Data.Map (Map)
import Data.Map qualified as Map
import System.Process (callCommand)


--------------------------------------------------------------------------------
graph :: Expr -> IO ()
graph e = do
  dotFn "example.dot" $ build e
  callCommand "dot -Nfontname=\"DejaVu Sans Mono\" -Nfontsize=20 -Tsvg example.dot -o example.svg"
  callCommand "kitten icat --align=left example.svg"


--------------------------------------------------------------------------------
data Node = NApp Int Func Arg
  deriving Show

data Func =
    FComb String
  | FPrim String
  | FRef Int
  | FVar String -- ^ Should not be present in a "compiled" tree.
  deriving Show

data Arg =
    AComb String
  | ARef Int
  | AInt Int
  | AVar String -- ^ Should not be present in a "compiled" tree.
  deriving Show

build :: Expr -> [Node]
build e = Map.elems . snd $ State.execState (allocate e) (0, Map.empty)

-- | Build a graph.
allocate :: Expr -> State (Int, Map Int Node) Int
allocate (Var x) = do
  -- TODO I think we're not supposed to have Vars here.
  (n, nodes) <- State.get
  let node = NApp n (FComb "I") (AVar x)
      nodes' = Map.insert n node nodes
  State.put (n + 1, nodes')
  pure n
allocate (Int i) = do
  (n, nodes) <- State.get
  let node = NApp n (FComb "I") (AInt i)
      nodes' = Map.insert n node nodes
  State.put (n + 1, nodes')
  pure n
allocate (Comb x) = do
  error "TODO No non-applied comb"
allocate (App f a) = do
  n <- inc
  l <- allocateL f
  r <- allocateR a
  (n', nodes) <- State.get
  let node = NApp n l r
      nodes' = Map.insert n node nodes
  State.put (n', nodes')
  pure n

allocateL :: Expr -> State (Int, Map Int Node) Func
allocateL (Comb a) = pure $ FComb a
allocateL (Prim a) = pure $ FPrim a
allocateL (Var x) = pure $ FVar x
allocateL f = do
  fref <- allocate f
  pure $ FRef fref

allocateR :: Expr -> State (Int, Map Int Node) Arg
allocateR (Comb a) = pure $ AComb a
allocateR (Int i) = pure $ AInt i
allocateR (Var x) = pure $ AVar x
allocateR a = do
  aref <- allocate a
  pure $ ARef aref

inc :: State (Int, Map Int Node) Int
inc = do
  (n, nodes) <- State.get
  State.put (n + 1, nodes)
  pure n

dot :: [Node] -> IO ()
dot = putStrLn . dot'

dotFn :: FilePath -> [Node] -> IO ()
dotFn fn = writeFile fn . dot'

dot' :: [Node] -> String
dot' nodes = unlines $
  [ "digraph g {"
  , "node [shape=record];"
  , "edge [tailclip=false];"
  , ""
  ]
  <> concatMap dotOne nodes <>
  [ "}"
  ]

dotOne :: Node -> [String]
dotOne (NApp i (FRef l) (ARef r)) =
  [ "node" <> show i <> " [label=\"<left> | <right>\", xlabel=\""<> show i <> "\"];"
  , "node" <> show i <> ":left:c -> node" <> show l <> ":left"
  , "node" <> show i <> ":right:c -> node" <> show r <> ":left"
  ]
dotOne (NApp i (FRef l) r) =
  [ "node" <> show i <> " [label=\"<left> | <right> " <> dotR r <> "\", xlabel=\""<> show i <> "\"];"
  , "node" <> show i <> ":left:c -> node" <> show l <> ":left"
  ]
dotOne (NApp i (FComb l) (ARef r)) =
  [ "node" <> show i <> " [label=\"<left> " <> l <> " | <right>\", xlabel=\""<> show i <> "\"];"
  , "node" <> show i <> ":right:c -> node" <> show r <> ":left"
  ]
dotOne (NApp i (FComb l) r) =
  [ "node" <> show i <> " [label=\"<left> " <> l <> " | <right> " <> dotR r <> "\", xlabel=\""<> show i <> "\"];"
  ]
dotOne (NApp i (FPrim l) r) =
  [ "node" <> show i <> " [label=\"<left> " <> l <> " | <right> " <> dotR r <> "\", xlabel=\""<> show i <> "\"];"
  ]

dotR r =
  case r of
    AComb s -> s
    AInt i -> show i
    AVar x -> x
