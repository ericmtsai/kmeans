module Kmeans where

import Data.List
import System.Random

-- a point to classify is an n-dimensional vector
type Point = [Float]

{-
-- We want our initial centroids to be reasonable values,
-- so we find the range of each "feature"
range :: [Point] -> [(Float, Float)]
range pss = reverse (rangeHelp (length(head pss)) pss)

--rangeHelp :: Int -> [Point] -> [(IO Float, IO Float)]
rangeHelp 0 _   = []
rangeHelp l pss = (let feature = [ps !! (l-1) | ps <- pss]
                     in (minimum feature, maximum feature)):(rangeHelp (l-1) pss)

-- Create random points within that range for each centroid
--run :: Int -> [Point] -> IO ()
--run k pss = do 
--              c <- (initClusters k pss)
--              print (c)

initClusters :: Int -> [Point] -> IO [[Float]]
initClusters k pss = initClustersCreate k (range pss) 
--getRand (fst (range pss)) (snd (range pss))

initClustersCreate k rs = do if k == 0 then 
                               return []
                             else
                               do x <- initClustersHelp rs
                                  xs <- initClustersCreate (k-1) rs
                                  return (x:xs)

initClustersHelp :: [(Float, Float)] -> IO [Float]
initClustersHelp rs  = do if rs == [] then
                             return []
                          else
                             do y <- getRand (fst (head rs)) (snd (head rs))
                                ys <- initClustersHelp (tail rs)
                                return (y:ys)

getRand:: Float -> Float ->IO Float
getRand f s= (randomRIO (f, s))

getRange :: (Float, Float)
getRange = (0.0, 100.0)
-}

initClusters :: Int -> [Point] -> IO [[Float]]
initClusters k pss = initClustersHelp (length pss) k pss

initClustersHelp :: Int -> Int -> [Point] -> IO [[Float]]
initClustersHelp _ k _   = [[]]
initClustersHelp l k pss = (pss ! (randomRIO 0, l)):(initClustersHelp l (k-1) pss)

--assign :: [[Point]] -> [Point] -> Float -> [[Point]]
-- EM algorithm until the next assigment doesn't change the MSE more than
-- some error threshold
assign pss cs err = 
  let mse = findMSE pss cs 
    in let newCluster = findCentroids pss
       in if mse - (findMSE (cluster (concat pss) newCluster) newCluster) < err then
             cluster (concat pss) newCluster
             --newCluster
          else assign (cluster (concat pss) newCluster) newCluster err


-- a centroid is the mean of all the points
findCentroid :: [Point] -> Point
findCentroid []     = []
findCentroid (p:ps) = reverse (findCentroidHelp (length p) (p:ps))

findCentroidHelp :: Int -> [Point] -> Point
findCentroidHelp _ [] = []
findCentroidHelp 0 _  = []
findCentroidHelp l ps = (let p = ([x!!(l-1) | x <- ps]) 
                             in (sum p)/fromIntegral (length p)):findCentroidHelp (l-1) ps

-- find centroids on clusters (lists) of points
findCentroids :: [[Point]] -> [Point]
findCentroids pss = map (findCentroid) pss

-- assign unclustered points to centroids
-- each cluster name is the index in the list
cluster (x:xs) (c:cs) = reverse (clusterHelper (length (c:cs)) (x:xs) (c:cs))

clusterHelper 0 _ _   = []
clusterHelper l xs cs = [x | x <- xs, (findCluster x cs) == (l-1)]:(clusterHelper (l-1) xs cs)

-- find index of the cluster c with minimum distance to the point x
findCluster :: Point -> [Point] -> Int
findCluster _ []     = error "No clusters given"
findCluster x (c:cs) = case elemIndex (minimum (findClusterHelper x (c:cs))) (findClusterHelper x (c:cs)) of
                            Just x  -> x
                            Nothing -> error "No cluster found"

findClusterHelper :: Point -> [Point] -> [Float]
findClusterHelper _ []     = []
findClusterHelper x (c:cs) = (findDist x c):(findClusterHelper x (cs))

findDist x c = sum [((fst d)-(snd d))**2 |d <- zip x c]

-- the error is the mean total distance of a cluster from its centroid 
findMSE [] _  = 0
findMSE _ []  = 0
findMSE xs cs = ((findDistRec (head xs) (head cs)) / fromIntegral (length (head xs))) + (findMSE (tail xs) (tail cs))

findDistRec [] _  = 0
findDistRec xs c = (findDist (head xs) c) + (findDistRec (tail xs) c) 
