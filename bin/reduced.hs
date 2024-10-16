


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
tests :: IO ()
tests = do
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
