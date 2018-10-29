-- Lab 8: Additional constructions, nondeterministic machines

import Data.List
import Control.Monad (replicateM)  -- for strings function below

-- Fixed alphabet, but everything below should work for any sigma!
sigma :: [Char]
sigma = "ab"


---------------- Given functions ----------------

-- All strings on sigma of length <= n (useful for testing)
strings :: Int -> [String]
strings n = concat $ [replicateM i sigma | i <- [0..n]]

-- normalize a list, i.e., sort and remove (now adjacent) duplicates
norm :: Ord a => [a] -> [a]
norm = mynub . sort where
  mynub [] = []
  mynub [x] = [x]
  mynub (x:ys@(y:zs)) | x == y = mynub ys
                      | otherwise = x : mynub ys

-- Cartesian product
(><) :: [a] -> [b] -> [(a,b)]
xs >< ys = [(x,y) | x <- xs, y <- ys]   

-- Check whether two lists overlap
overlap :: Eq a => [a] -> [a] -> Bool
overlap [] ys = False
overlap (x:xs) ys = elem x ys || overlap xs ys

-- Unary set closure (where "set" = normalized list)
-- uclosure xs g == smallest set containing xs and closed under g
uclosure :: Ord a => [a] -> (a -> [a]) -> [a]
uclosure xs g = sort $ stable $ iterate close (xs, []) where
  stable ((fr,xs):rest) = if null fr then xs else stable rest
  close (fr, xs) = (fr', xs') where
    xs' = fr ++ xs
    fr' = norm $ filter (`notElem` xs') $ concatMap g fr
  

-- Ordinary regular expressions and a show method for them
data RE  = Empty | Letter Char | Union RE RE | Cat RE RE | Star RE

instance (Show RE) where    -- use precedence to minimize parentheses
  showsPrec d Empty         = showString "@"
  showsPrec d (Letter c)    = showString [c]
  showsPrec d (Union r1 r2) = showParen (d > 6) $  -- prec(Union) = 6
                              showsPrec 6 r1 .
                              showString "+" .
                              showsPrec 6 r2
  showsPrec d (Cat r1 r2)   = showParen (d > 7) $  -- prec(Cat) = 7
                              showsPrec 7 r1 .
                              showsPrec 7 r2
  showsPrec d (Star r1)     = showsPrec 9 r1 .     -- prec(Star) = 8
                              showString "*"


-- Finite state machines, indexed by the type of their states
-- M = (states, start, finals, transitions)  
type Trans a = [(a,Char,a)]
type FSM a = ([a], a, [a], Trans a)

-- ap ts q a == the unique q' such that (q, a, q') is in ts;  assumes success
ap :: Eq a => Trans a -> a -> Char -> a 
ap ((q1, a1, q2):ts) q a = if (q1,a1) == (q,a) then q2 else ap ts q a

-- Iterated ap
ap_star :: Eq a => Trans a -> a -> String -> a
ap_star = foldl' . ap

-- reachable m == the part of m that is reachable from the start state
reachable :: Ord a => FSM a -> FSM a
reachable m@(qs, s, fs, ts) = (qs', s, fs', ts') where
  qs' = uclosure [s] (\q -> map (ap ts q) sigma)
  fs' = filter (`elem` fs) qs'
  ts' = take (length qs' * length sigma) $ filter (\(q,_,_) -> q `elem` qs') ts

-- Change the states of an FSM from an equality type to Int
intify :: Eq a => FSM a -> FSM Int
intify (qs, s, fs, ts) = (qs', s', fs', ts') where
  qs' = map index qs
  s'  = index s
  fs' = map index fs
  ts' = [(index q, a, index q') | (q,a,q') <- ts]
  index q = lookup qs q
  lookup (q':qs) q = if q == q' then 0 else 1 + lookup qs q

reduce :: Ord a => FSM a -> FSM Int
reduce = intify . reachable


---------------- Part 1: Additional constructions ----------------
-- Define the operations given in Section 7 of the notes

-- Intersection
inters :: (Eq a, Eq b) => FSM a -> FSM b -> FSM (a,b)
inters m1 m2 = undefined

-- Complement
compl :: Eq a => FSM a -> FSM a
compl m = undefined

-- Direct homomorphic image
homo_dir :: (Char -> String) -> RE -> RE
homo_dir k r = undefined

-- Inverse homomorphic image
homo_inv :: Eq a => (Char -> String) -> FSM a -> FSM a
homo_inv k m = undefined

-- Right quotient
quot_right :: Eq a => [String] -> FSM a -> FSM a
quot_right ws m = undefined


---------------- Part 2: Nondeterministic machines ----------------

-- Nondeterministic FSMs, indexed by their type of state
-- M = (states, starts, finals, transitions)  
type NFSM a = ([a], [a], [a], Trans a)

-- nap_hat ts qs a == normalized list of all transitions possible from qs on a
nap_hat :: Ord a => Trans a -> [a] -> Char -> [a]
nap_hat ts qs a = undefined

-- nap_hat_star ts qs w == normalized list of transitions possible from qs on w
nap_hat_star :: Ord a => Trans a -> [a] -> [Char] -> [a]
nap_hat_star = undefined

-- naccept m w == m accepts the string w
naccept :: Ord a => NFSM a -> [Char] -> Bool
naccept (qs, ss, fs, ts) w = undefined


-- Nondeterministic FSMs with epsilon moves, indexed by their type of state
-- M = (states, starts, finals, transitions, epsilon-moves)
type Eps a = [(a, a)]
type EFSM a = ([a], [a], [a], Trans a, Eps a)

-- Normalized epsilon closure of a set of states (Hint: use uclosure)
eclose :: Ord a => Eps a -> [a] -> [a]
eclose es qs = undefined
  
-- eap_has ts es qs a == eclosure of transitions possible from qs on a
eap_hat :: Ord a => (Trans a, Eps a) -> [a] -> Char -> [a]
eap_hat (ts,es) qs a = undefined

-- eap_hat_star ts es q w == eclosure of transitions possible from qs on w
eap_hat_star :: Ord a => (Trans a, Eps a) -> [a] -> [Char] -> [a]
eap_hat_star = undefined

eaccept :: Ord a => EFSM a -> [Char] -> Bool
eaccept (qs, ss, fs, ts, es) w = undefined


---------------- Part 3: Conversion between machines ----------------

-- Easy conversion from FSM to NFSM (given)
fsm_to_nfsm :: Eq a => FSM a -> NFSM a
fsm_to_nfsm (qs, s, fs, ts) = (qs, [s], fs, ts)


-- Conversion from NFSM to FSM by the "subset construction"
nfsm_to_fsm :: Ord a => NFSM a -> FSM [a]
nfsm_to_fsm (qs1, ss1, fs1, ts1) = (qs, s, fs, ts) where
  qs = undefined
  s  = undefined
  fs = undefined
  ts = undefined


-- Similar conversion from EFSM to FSM using epsilon closure
efsm_to_fsm :: Ord a => EFSM a -> FSM [a]
efsm_to_fsm (qs1, ss1, fs1, ts1, es1) = (qs, s, fs, ts) where
  qs = undefined
  s  = undefined
  fs = undefined
  ts = undefined


{- Tests:

1. m and fsm_to_nfsm m accept the same strings
2. m and nfsm_to_fsm m accept the same strings
3. m and efsm_to_fsm m accept the same strings

-}