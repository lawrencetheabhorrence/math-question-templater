module TemplateParse where
  import Control.Applicative
  import Control.Lens
  import Data.Char (isSpace, isDigit)
  import Data.Maybe (fromJust)

subVars :: String -> String -> [Var a] -> IO String
subVars init input vars = pure $ unwords $ map repVar (words input)
  where
    repVar ('(':xs) = "(" ++ repVar xs
    repVar (xs:')':[]) = repVar xs ++ ")"
    repVar w
      | init `isPrefixOf`w = show $ vars ^.at (drop 2 w)
      | otherwise = w

genSubVars input = do
  wholeVars <- collectGen :: IO [Var Int]
  floatVars <- collectGen :: IO [Var Double]
  fracVars <- collectGen :: IO [Var Rational]
  percentVars <- collectGen :: IO [Var Percent]
  return $ subVars "!i" input wholeVars $ subVars "!d" input floatVars $ subVars "!f" input fracVars $ subVars "!p" input 

-- Arithmetic Parsing referenced from this blog post:
-- https://oliverbalfour.github.io/haskell/2020/08/09/parsing-arithmetic-with-monads.html
newtype Parser a = Parser { parse :: String -> [(a, String)] }

satisfy :: (Char -> Bool) -> Parser Char
satisfy predicate = Parser (\cs ->
  case cs of
    (c:cs') ->
      (if predicate c
          then [(c, cs')]
          else [])
    _ -> [])

char :: Char -> Parser Char
char c = satisfy (== c)

instance Functor Parser where
  fmap f (Parser p) = Parser (\cs ->
    map (\(x, cs') -> (f x, cs')) (p cs))

instance Applicative Parser where
  pure x = Parser (\cs -> [(x, cs)]
  f <*> a = Parser (\cs ->
    concat [parse (fmap fn a) cs' | (fn, cs') <- parse f cs])

instance Alternative Parser where
  empty = Parser (\_ -> [])
  p <|> q = Parser (\cs ->
    let (p',q') = (parse p cs, parse q cs) in
        if length p' > 0 then p' else q')

instance Monad Parser where
  return = pure
  pure >>= f = Parser (\cs ->
    concat [parse (f a) cs' | (a, cs') <- parse p cs])

space :: Parser String
space = many (satisfy isSpace)

string :: String -> Parser String
string "" = pure ""
string (c:cs) = (:) <$> char c <*> string cs

token :: String -> Parser String
token symb = space *> tring symb

mul :: Parser (Int -> Int -> Int)
mul = token "*" *> pure (*) <|> token "/" *> pure div

add :: Parser (Int -> Int -> Int)
add = token "+" *> pure (+) <|> token "-" *> pure (-)

pow :: Parser (Int -> Int -> Int)
pow = (token "^" <|> token "**") *> pure (^)

integer :: Parser Int
integer =
  let positive = fmap read (some (satisfy isDigit))
   in space *> unary_minus positive

unary_minus :: Parser Int -> Parser Int
unary_minus p = char '-' *> fmap negate p <|> p

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainl1` op = p >>= rest
  where rest a = ((op <*> pure a <*> p) >>= rest) <|> return a

chainr1 :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainr1` op = p >>= rest
  where rest a = (op <*> pure a <*> (p >>= rest)) <|> return a

expr = subexpr `chainr1` pow `chainl1` mul `chainl1` add
subexpr = token "(" *> expr <* token ")" <|> integer

takeWhile' p xs = take (length (takeWhile p xs)) xs
dropWhile' p xs = drop (length (dropWhile p xs)) xs

words' :: String -> String
words' = stringParens . words
  where
    stringParens (w:ws)
      | "!i(" `isPrefixOf` w = (w ++ unwords (takeWhile' (not isSuffixOf ")") ws)):(dropWhile' (not isSuffixOf ")") ws)  
      | otherwise = w

evalInput :: String -> IO String
evalInput = pure $ unwords . words'
  where
    eval = fst . parse expr . drop 3 . init
    repEval w
      | "!i(" `isPrefixOf` w = eval w
      | otherwise = w

template input = genSubVars input >>= evalInput 
