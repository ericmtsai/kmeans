import Data.List

-- a point to classify is an n-dimensional vector
type Point = [Float]

--
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
