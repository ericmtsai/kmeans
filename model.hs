import Kmeans
import Data.List.Split
import Data.List
import System.IO

-- Read data in from a csv, no headers
readIn :: String -> IO [Point]
readIn fname = do x  <- readFile fname
                  s <- parse (lines x)
                  f <- parseFloat s
                  return f

parse :: [String] -> IO [[String]]
parse xs = return (map (\x -> splitOn "," x) xs)

parseFloat :: [[String]] -> IO [Point]
parseFloat xs = return (map (\x -> (map read x)) xs)


--run :: String -> Int -> Float -> Int -> IO [[[Point]]]
run fname k err r = do ps <- readIn fname
                       -- a list of length r of assignments
                       -- multiple assignements
                       mas <- runHelp ps k err r
                       -- now we parse this data structure and find the
                       -- single run with the lowest MSE.
                       -- This is not at all efficient because we already 
                       -- calculate these as we're creating the assignments.
                       let res = findMinIndex mas
                           i   = fst res
                           mse = snd res
                           as  = mas !! i
                           cs  = findCentroids as
                       saveModel cs
                       assignPoints fname as
                       putStrLn ("Created "++(show k)++" clusters resulting in MSE of "++(show mse))

-- Now we save the model
-- just a wrapper for a cleaner "main"
saveModel :: [Point] -> IO()
saveModel cs = saveModelRec cs 0

saveModelRec cs label = do if cs == [] then
                             return ()
                           else
                             do formatAndWrite "model.km" (head cs) label
                                saveModelRec (tail cs) (label + 1)

formatAndWrite :: String -> Point -> Int -> IO()
formatAndWrite fname centroid label = do
                                  appendFile fname ((init (tail str))++","++(show label)++"\n")
                                    where str = (show centroid)

-- write out the assignment of points:
assignPoints :: String -> [[Point]] -> IO()
assignPoints fname as = assignPointsRec ("assigned_" ++ fname) as 0

assignPointsRec fname as label = do if as == [] then
                                      return ()
                                    else
                                      do assignPointsRecToWrite fname (head as) label
                                         assignPointsRec fname (tail as) (label + 1)

assignPointsRecToWrite fname a label = do if a == [] then
                                            return ()
                                          else
                                            do formatAndWrite fname (head a) label
                                               assignPointsRecToWrite fname (tail a) label

-- Out of all r models built, finds index of the one with minimum MSE
findMinIndex :: [[[Point]]] -> (Int, Float)
findMinIndex mas = let f = map (\x -> findSSE x (findCentroids x)) mas
                      in let (Just i) = elemIndex (minimum f) f
                           in (i, f !! i)

--so we don't read the file repeatedly in our recursive call
runHelp :: [Point] -> Int -> Float -> Int -> IO [[[Point]]]
runHelp ps k err r = do if r == 0 then
                           return []
                        else
                           do as <- runSingle k ps err
                              mas <- runHelp ps k err (r-1)
                              return (as:mas)

-- create a single instance of the model
-- There is some randomization involved so a single model
-- may not always result in a good classification
runSingle :: Int -> [Point] -> Float -> IO [[Point]]
runSingle k ps err = do cs <- initClusters k ps
                        -- we need initial clustering to pass into the recursive algorithm
                        -- back to the functional world
                        let inits = cluster ps cs
                            assignments = assign inits cs err
                        return assignments
