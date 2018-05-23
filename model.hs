--{-# LANGUAGE ScopedTypeVariables #-}
import Kmeans
--import Data.Csv
--import qualified Data.ByteString.Lazy as BL
--import qualified Data.Vector as V
import Data.List.Split


readIn :: String -> IO [[Float]]
readIn fname = do x  <- readFile fname
                  s <- parse (lines x)
                  f <- parseFloat s
                  return f

parse :: [String] -> IO [[String]]
parse xs = return (map (\x -> splitOn "," x) xs)

parseFloat :: [[String]] -> IO [[Float]]
parseFloat xs = return (map (\x -> (map read x)) xs)

