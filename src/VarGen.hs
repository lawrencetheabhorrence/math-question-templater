module VarGen where
  import qualified System.Random as Rand
  import Data.Ratio
  import Control.Lens

data Var a = Var { name :: String, val :: a }
makeLenses ''Var

data Percent = Integer
data Name = String

-- default integer range
intRange :: (Integer, Integer)
intRange = (1, 110)

decRange :: (Double, Double)
decRange = (1, 30)

percentRange :: (Integer, Integer)
percentRange = (1, 99)

fracRange :: (Integer, Integer)
fracRange = (1, 10)

class GenVar a where
  genVar :: [Rand.StdGen] -> String -> Var a
  assign :: String -> a -> Var a
  collectGen :: String -> String -> IO [Var a] -- this is really a copout since we can only really work with random in IO

  assign n v = Var n v
  collectGen init input = mapM (\(s,n) -> pure $ genVar s n) seedNames
    where
      names = map (drop 2) $ filter (\n -> startsWith init n && not ((drop 2 n) `elem` names || head n == '(')) $ words input
      seedList is = take (length names) $ randoms (mkStdGen is)
      seedsNames = initSeed >>= (\is -> pure $ zip (seedList is) names)

instance GenVar Int where
  genVar s name = Var name (fst $ Rand.uniformR intRange (head s))
  collectGen = collectGen "!i"

instance GenVar Double where
  genVar s name = Var name (fst $ Rand.uniformR decRange (head s))
  collectGen = collectGen "!d"

instance GenVar Percent where
  genVar s name = Var name (fst $ Rand.uniformR percentRange (head s))
  collectGen = collectGen "!p"

instance GenVar Rational where
  genVar (s1:s2:xs) name = Var name $ (genF s1) % (genF s2)
    where genF = fst . Rand.uniformR fracRange
  genVar _ name = Var name (1 % 1)
  collectGen = collectGen "!f"

instance Show Var Percent where
  show v = (show $ val v) ++ "%"

instance Show Var Double where
  show v = trunc2 $ show (val v)
    where trunc2 s = fst (break (==',') s) ++ take 3 $ snd (break (==',') s)

instance Show Var a where
  show v = show $ val v
