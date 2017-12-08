module Trabb where
import Control.Monad (replicateM)

main :: IO ()
main = mapM_ print.reverse.map (f.read.chomp) =<< replicateM 11 getLine

f :: Double -> Maybe Double
f t | t' > 400  = Nothing
    | otherwise = Just t'
  where t' =  sqrt (abs t) + 5 * (t ^ 3)
     
chomp :: String -> String
chomp = takeWhile (/= '\n')