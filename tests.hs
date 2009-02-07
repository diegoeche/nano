{-# LANGUAGE NoMonomorphismRestriction #-}
import Test.QuickCheck
import Nano

deepCheck = check (defaultConfig { configMaxTest = 1000})

splitConcat :: (Ord a) => [a] -> Maybe Bool
splitConcat x = do 
  (b, v, a) <- split x
  return $ (b ++ [v] ++ a) == x

defaultMaybe _ (Just x) = x 
defaultMaybe x Nothing  = x     

prop_SplitConcat x = (x /= []) ==> defaultMaybe False . splitConcat $ x

main :: IO ()
main = deepCheck (prop_SplitConcat:: [Int] -> Property)

