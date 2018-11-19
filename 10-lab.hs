-- Lab 10

import Data.List

-- Fixed alphabet, but everything below should work for any sigma!
sigma :: [Char]
sigma = "ab"

-- normalize a list, i.e., sort and remove (now adjacent) duplicates
norm :: Ord a => [a] -> [a]
norm = mynub . sort where
  mynub [] = []
  mynub [x] = [x]
  mynub (x:ys@(y:zs)) | x == y = mynub ys
                      | otherwise = x : mynub ys

-- Finite state machines, indexed by the type of their states
-- M = (states, start, finals, transitions)  
type Trans a = [(a,Char,a)]
type FSM a = ([a], a, [a], Trans a)

-- ap ts q a == the unique q' such that (q, a, q') is in ts;  assumes success
ap :: Eq a => Trans a -> a -> Char -> a 
ap ((q1, a1, q2):ts) q a = if (q1,a1) == (q,a) then q2 else ap ts q a

-- Unary set closure (where "set" = normalized list)
-- uclosure xs g == smallest set containing xs and closed under g
uclosure :: Ord a => [a] -> (a -> [a]) -> [a]
uclosure xs g = sort $ stable $ iterate close (xs, []) where
  stable ((fr,xs):rest) = if null fr then xs else stable rest
  close (fr, xs) = (fr', xs') where
    xs' = fr ++ xs
    fr' = norm $ filter (`notElem` xs') $ concatMap g fr


---------------- Lab 10 begins here ----------------

-- Minimize
minimize :: Ord a => FSM a -> FSM a
minimize (qs, s, fs, ts) = undefined

-- Isomorphism
iso :: Ord a => FSM a -> FSM a -> Bool
iso (qs1, s1, fs1, ts1) (qs2, s2, fs2, ts2) = undefined

-- Test
