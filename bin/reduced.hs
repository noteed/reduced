{-# LANGUAGE ApplicativeDo #-}

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
import Options.Applicative ((<**>))
import Options.Applicative qualified as A
import System.Process (callCommand)
import Text.Pretty.Simple (pPrintNoColor)

--------------------------------------------------------------------------------
main :: IO ()
main = A.execParser parserInfo >>= run

data Command =
    Parse String
  | Compile String
  | Reduce String Bool -- ^ Show intermediate steps or not.
  | Graph String Bool -- ^ Show intermediate steps or not.

parserInfo :: A.ParserInfo Command
parserInfo =
  A.info (parser <**> A.helper) $
    A.fullDesc
      <> A.header "reduced - playing with combinators"
      <> A.progDesc
        "Reduced is a lambda calculus and combinators playground."

parser :: A.Parser Command
parser =
  A.subparser
    ( A.command
        "parse"
        ( A.info (parserParse <**> A.helper) $
            A.progDesc "Parse a lambda calculus expression"
        )
        <> A.command
          "compile"
          ( A.info (parserCompile <**> A.helper) $
              A.progDesc "Compile a lambda calculus expression to its combinators representation"
          )
        <> A.command
          "reduce"
          ( A.info (parserReduce <**> A.helper) $
              A.progDesc "Parse a lambda calculus expression then reduce its combinators representation"
          )
        <> A.command
          "graph"
          ( A.info (parserGraph <**> A.helper) $
              A.progDesc "Display a combinators expression as SVG"
          )
    )

parserParse :: A.Parser Command
parserParse = do
  s <- A.argument A.str (A.help "Lambda expression to parse")
  pure $ Parse s

parserCompile :: A.Parser Command
parserCompile = do
  s <- A.argument A.str (A.help "Lambda expression to compile")
  pure $ Compile s

parserReduce :: A.Parser Command
parserReduce = do
  s <- A.argument A.str (A.help "Lambda expression to compile and reduce")
  withSteps <-
    A.switch
      (A.long "steps" <> A.help "Show intermediate steps")
  pure $ Reduce s withSteps

parserGraph :: A.Parser Command
parserGraph = do
  s <- A.argument A.str (A.help "Lambda expression to compile and reduce")
  withSteps <-
    A.switch
      (A.long "steps" <> A.help "Show intermediate steps")
  pure $ Graph s withSteps


--------------------------------------------------------------------------------
run :: Command -> IO ()
run (Parse s)= case parse s of
  Right e -> pPrintNoColor e
  Left err -> putStrLn err
run (Compile s)= case parse s of
  Right e -> pPrintNoColor $ compile e
  Left err -> putStrLn err
run (Reduce s withSteps)= case parse s of
  Right e ->
    if withSteps
    then pPrintNoColor . steps $ compile e
    else pPrintNoColor . reduce $ compile e
  Left err -> putStrLn err
run (Graph s withSteps)= case parse s of
  Right e ->
    if withSteps
    then mapM_ graph . steps $ compile e
    else graph . reduce $ compile e
  Left err -> putStrLn err


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
  | APrim String
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
allocateR (Prim a) = pure $ APrim a
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
  [ "node" <> show i <> " [label=\"<left> " <> l <> " | <right> " <> mr <> "\", xlabel=\""<> show i <> "\"];"
  ]
  ++ mr'
 where
  (mr', mr) = case r of
    ARef r' -> (["node" <> show i <> ":right:c -> node" <> show r' <> ":left"], "")
    _ -> ([], dotR r)

dotR r =
  case r of
    AComb s -> s
    APrim s -> s
    AInt i -> show i
    AVar x -> x
