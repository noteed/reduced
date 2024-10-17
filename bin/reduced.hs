import Control.Applicative


--------------------------------------------------------------------------------
data Expr =
  -- Source language
    Var String
  | App Expr Expr
  | Lam String Expr

  -- Target language

  | Comb String
  -- ^ Some implementations reuse Var instead of using a different constructor.
  deriving (Eq, Show)


--------------------------------------------------------------------------------

data Parser a = Parser { runParser :: String -> Either String (a, String) }

instance Functor Parser where
  fmap f (Parser p) = Parser (fmap (\(a, b) -> (f a, b)) . p)

instance Applicative Parser where
  pure a = Parser (\s -> Right (a, s))
  p <*> x = Parser (
    \s -> do
      (f, t) <- runParser p s
      (a, u) <- runParser x t
      pure (f a, u)
    )

instance Monad Parser where
  p >>= q = Parser (\s -> runParser p s >>= (\(r, t) -> runParser (q r) t))
  return = pure

instance Alternative Parser where
  empty = Parser (\_ -> Left "XXX")
  p <|> q = Parser (\s -> either (const $ runParser q s) Right $ runParser p s)

anychar :: Parser Char
anychar = Parser p
 where
  p (c:cs) = Right (c, cs)
  p "" = Left "Reached end of file"

sat :: (Char -> Bool) -> Parser Char
sat f = Parser p
 where
  p (c:cs) | f c = Right (c, cs)
  p _ = Left "Doesn't satisfy"

char :: Char -> Parser Char
char c = sat (== c)

string :: String -> Parser String
string = mapM char

isAlpha :: Char -> Bool
isAlpha c = c >= 'a' && c <= 'z'

isDigit :: Char -> Bool
isDigit c = c >= '0' && c <= '9'

isUnderscore :: Char -> Bool
isUnderscore c = c == '_'

whitespace = many (sat (== ' '))

eof :: Parser ()
eof = Parser p
 where
  p "" = Right ((), "")
  p _ = Left "Expecting end of file"

var :: Parser Expr
var = do
  a <- var'
  pure (Var a)

var' :: Parser String
var' = do
  a <- sat isAlpha
  as <- many (sat (\c -> isAlpha c || isDigit c || isUnderscore c))
  _ <- whitespace
  pure (a : as)

comb :: Parser Expr
comb = do
  a <- sat (\c -> c `elem` "SKI")
  _ <- whitespace
  pure (Comb [a])

lam :: Parser Expr
lam = do
  _ <- sat (== '\\')
  a <- var'
  _ <- string "->"
  _ <- whitespace
  e <- expr
  _ <- whitespace
  pure (Lam a e)

paren :: Parser Expr
paren = do
  _ <- char '('
  _ <- whitespace
  e <- expr
  _ <- whitespace
  _ <- char ')'
  _ <- whitespace
  pure e

app :: Parser Expr
app = do
  as <- some (var <|> comb <|> paren)
  pure (foldl1 App as)

expr :: Parser Expr
expr = app <|> lam

parse :: String -> Either String Expr
parse s = case runParser (expr <* eof) s of
  Right (e, _) -> Right e
  Left err -> Left err

--------------------------------------------------------------------------------

render :: Expr -> String
render (Var x) = x
render (App f a) = renderL f <> " " <> renderR a
render (Lam x a) = "\\" <> x <> " -> " <> render a
render (Comb x) = x

renderL (Lam x a) = "(" <> render (Lam x a) <> ")"
renderL a = render a

renderR (App f a) = "(" <> render (App f a) <> ")"
renderR a = render a

--------------------------------------------------------------------------------

graph :: Expr -> String
graph (Var x) = x
graph (App f a) = unlines
  [ "  @"
  , " / \\"
  , graph f <> "   " <> graph a
  ]

--------------------------------------------------------------------------------

-- | Rewrite closed lambdas to combinators (S, K, and I), starting with the
-- innermost ones first.
compile :: Expr -> Expr
compile (Var x) = (Var x)
compile (App f a) = App (compile f) (compile a)
compile (Lam x a) = abstract x a

-- | The bracket abstraction (called on lambda terms above).
abstract :: String -> Expr -> Expr
abstract x (Var y) = if x == y then Comb "I" else App (Comb "K") (Var y)
abstract x (App f a) = App (App (Comb "S") (abstract x f)) (abstract x a)
abstract x (Lam y a) = abstract x (abstract y a)


--------------------------------------------------------------------------------
reduce :: Expr -> Expr
reduce expr = head (reduce' [expr] expr)

steps :: Expr -> [Expr]
steps expr = reverse (reduce' [expr] expr)

reduce' :: [Expr] -> Expr -> [Expr]
reduce' acc expr =
  let expr' = step expr
  in
    if expr == expr'
    then acc
    else reduce' (expr' : acc) expr'

step :: Expr -> Expr
step (App (App (App (Comb "S") f) g) x) = App (App f x) (App g x)
step (App (App (Comb "K") a) _) = a
step (App (Comb "I") a) = a
step (App f a) = App f a
step (Var x) = Var x
step (Lam _ _) = error "lambda in object code"
step (Comb x) = Comb x


--------------------------------------------------------------------------------
display :: Expr -> IO ()
display expr = do
  print expr
  putStrLn (render expr)

  let obj = compile expr
  print obj
  putStrLn (render obj)

  let res = reduce obj
  print res
  putStrLn (render res)


--------------------------------------------------------------------------------
example1 = Var "x"
example2 = App (Var "f") (Var "x")
example3 = App (App (Var "f") (Var "x")) (Var "y")
example4 = App (Var "f") (App (Var "g") (Var "x"))
example5 = Lam "x" (App (Var "f") (Var "x"))
example6 = App (Lam "x" (App (Var "f") (Var "x"))) (Var "y")


example7 = App (Comb "I") (Var "x")
example8 = App (App (App s k ) k) (App k (App (App (App s k) k) k))
 where
  s = Comb "S"
  k = Comb "K"

--------------------------------------------------------------------------------
roundtrip :: Expr -> IO ()
roundtrip e = case parse (render e) of
  Right e' | e == e' -> pure ()
  Right e' -> error ("Got " <> show e <> ", expected " <> show e)
  Left _ -> error ("Can't parse back " <> show (render e))

--------------------------------------------------------------------------------
tests :: IO ()
tests = do
  mapM_ roundtrip
    [ example1
    , example2
    , example3
    , example4
    , example5
    , example6
    , example7
    , example8
    ]
  mapM_ (test reduce)
    [ (example7, "x")
    , (example8, "K (S K K K)")
    , (App (App (Comb "K") (Comb "I")) (Var "x"), "I")
    ]
  putStrLn "Ok."
 where
  test f (expr, expected) =
    let reduced = render (f expr)
    in
      if reduced == expected
      then pure ()
      else error ("Got " <> show reduced <> ", expected " <> show expected)
