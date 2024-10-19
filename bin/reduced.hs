import Control.Applicative


--------------------------------------------------------------------------------
data Expr =
  -- Source language
    Var String
  | Int Int
  | App Expr Expr
  | Lam String Expr
  | Add Expr Expr

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

opt :: Parser a -> Parser (Maybe a)
opt p = Parser (
  \s ->
    case runParser p s of
      Right (a, t) -> pure (Just a, t)
      Left _ -> pure (Nothing, s)
  )

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

isDigit' :: Char -> Bool
isDigit' c = c >= '1' && c <= '9'

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

int :: Parser Expr
int = do
  a <- sat isDigit'
  as <- many (sat isDigit)
  _ <- whitespace
  pure (Int (read (a : as)))

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
  as <- some (var <|> int <|> comb <|> paren)
  pure (foldl1 App as)

expr :: Parser Expr
expr = do
  a <- app <|> lam
  _ <- whitespace
  mb <- opt (
    do
      _ <- char '+'
      _ <- whitespace
      expr
    )
  case mb of
    Just b -> pure (Add a b)
    Nothing -> pure a

parse :: String -> Either String Expr
parse s = case runParser (expr <* eof) s of
  Right (e, _) -> Right e
  Left err -> Left err

--------------------------------------------------------------------------------

render :: Expr -> String
render (Var x) = x
render (Int n) = show n
render (App f a) = renderL f <> " " <> renderR a
render (Add a b) = render a <> " + " <> render b
render (Lam x a) = "\\" <> x <> " -> " <> render a
render (Comb x) = x

renderL (Lam x a) = "(" <> render (Lam x a) <> ")"
renderL a = render a

renderR (App f a) = "(" <> render (App f a) <> ")"
renderR (Add a b) = "(" <> render (Add a b) <> ")" -- hack for "(\\x -> \\y -> x) 4 (4 + 5)"
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
compile (Int n) = Int n
compile (App f a) = App (compile f) (compile a)
compile (Lam x a) = abstract x a
compile (Add a b) = Add (compile a) (compile b)

-- | The bracket abstraction (called on lambda terms above).
abstract :: String -> Expr -> Expr
abstract x (Var y) = if x == y then Comb "I" else App (Comb "K") (Var y)
abstract _ (Int n) = App (Comb "K") (Int n)
abstract x (App f a) = App (App (Comb "S") (abstract x f)) (abstract x a)
abstract x (Lam y a) = abstract x (abstract y a)
abstract x (Add a b) = Add (abstract x a) (abstract x b)
abstract _ (Comb c) = App (Comb "K") (Comb c)


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
step (Add a b) = primAdd a b
step (Var x) = Var x
step (Int n) = Int n
step (Lam _ _) = error "lambda in object code"
step (Comb x) = Comb x

primAdd (Int a) (Int b) = Int (a + b)
primAdd _ _ = error "Type mismatch"

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

example9 = Int 4
example10 = Add (Int 4) (Int 5)
example11 = Lam "x" (Lam "y" (Var "x"))
example12 = App (App example11 example9) example10

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

    , example9
    , example10
    , example11
    , example12
    ]
  mapM_ (test reduce)
    [ (example7, "x")
    , (example8, "K (S K K K)")
    , (App (App (Comb "K") (Comb "I")) (Var "x"), "I")
    , (example9, "4")
    , (example10, "9")
    ]
  putStrLn "Ok."
 where
  test f (expr, expected) =
    let reduced = render (f expr)
    in
      if reduced == expected
      then pure ()
      else error ("Got " <> show reduced <> ", expected " <> show expected)
