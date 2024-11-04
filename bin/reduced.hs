{-# LANGUAGE ApplicativeDo #-}

-- @
--     $ ghci -isrc/ bin/reduced.hs
--
--     $ dot -Nfontname="DejaVu Sans Mono" -Nfontsize=20 -Tsvg example.dot -o example.svg
--     $ kitten icat example.svg
-- @

import Reduced.Core
import Reduced.Graph

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

--------------------------------------------------------------------------------
data Command =
    Parse String
  | Compile String
  | Reduce String Bool -- ^ Show intermediate steps or not.
  | Graph String Bool -- ^ Show intermediate steps or not.

--------------------------------------------------------------------------------
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
