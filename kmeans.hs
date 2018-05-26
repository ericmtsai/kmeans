module Kmeans where

import Data.List
import System.Random

-- a point to classify is an n-dimensional vector
type Point = [Float]

-- We want our initial centroids to be reasonable values,
-- so we randomly take points from the initial dataset
-- and assign those as the initial k centroids to cluster around
initClusters :: Int -> [Point] -> IO [Point]
initClusters k ps = initClustersHelp (length ps) k ps

--probably want to add some checking here that we don't take the same on etwice
initClustersHelp :: Int -> Int -> [Point] -> IO [Point]
initClustersHelp _ 0 _   = return []
initClustersHelp l k ps = do  i <- randomRIO(0, (l-1))::IO Int
                              let x = ps !! i
                              do xs <- initClustersHelp l (k-1) ps
                                 return (x:xs)

-- EM algorithm until the next assigment doesn't change the MSE more than
-- some error threshold
assign :: [[Point]] -> [Point] -> Float -> [[Point]]
assign pss cs err = 
  let mse = findMSE pss cs 
    in let newCluster = findCentroids pss
       -- Check if % change in finding the next cluster iteration is larger than error threshold
       in if ((mse - (findMSE (cluster (concat pss) newCluster) newCluster)) / mse) > err then
             --if so, continue clustering
             assign (cluster (concat pss) newCluster) newCluster err
          -- otherwise, we're done
          else cluster (concat pss) newCluster

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
cluster :: [Point] -> [Point] -> [[Point]]
cluster (x:xs) (c:cs) = reverse (clusterHelper (length (c:cs)) (x:xs) (c:cs))

clusterHelper :: Int -> [Point] -> [Point] -> [[Point]]
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

-- get the distance between two points
findDist :: Point -> Point -> Float
findDist x c = sum [((fst d)-(snd d))**2 |d <- zip x c]

-- the error is the mean total distance of a cluster from its centroid 
-- this computes MSE for the first cluster, then recurses on the tail
findMSE :: [[Point]] -> [Point] -> Float
findMSE [] _  = 0
findMSE _ []  = 0
findMSE xs cs = ((findDistRec (head xs) (head cs)) / fromIntegral (length (head xs))) + (findMSE (tail xs) (tail cs))

-- finds Euclidean distance for a cluster and its centroid
findDistRec :: [Point] -> Point -> Float
findDistRec [] _  = 0
findDistRec xs c = (findDist (head xs) c) + (findDistRec (tail xs) c) 
