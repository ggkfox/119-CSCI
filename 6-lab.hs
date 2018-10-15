-- CSCI 119 lab 6
-- joshua holland
import Data.List
import Control.Monad (replicateM)  -- for strings function below

-- Fixed alphabet, but everything below should work for any sigma!
sigma :: [Char]
sigma = "ab"

-- All strings on sigma of length <= n (useful for testing)
strings :: Int -> [String]
strings n = concat $ [replicateM i sigma | i <- [0..n]]

-- Finite state machines, now indexed by the type of their states
-- M = (states, start, finals, transitions)  
type FSM a = ([a], a, [a], [(a, Char, a)])


---------------- A solution to Lab 5, ported to FSM a ------------------------
  
-- no_dups xs = "xs has no duplicates"
no_dups :: Eq a => [a] -> Bool
no_dups [] = True           
no_dups (x:xs) = notElem x xs && no_dups xs

-- subset xs ys == "xs is a subset of ys"
subset :: Eq a => [a] -> [a] -> Bool
subset [] ys = True
subset (x:xs) ys = elem x ys && subset xs ys

-- func3 as bs ts == "ts : as X bs -> cs"
func3 :: (Eq a, Eq b, Eq c) => [a] -> [b] -> [c] -> [(a,b,c)] -> Bool
func3 as bs cs ts = and [single (thirds a b ts) cs | a <- as, b <- bs] where
  thirds a b ts = [c' | (a',b',c') <- ts, (a',b') == (a, b)]
  single [x] ys = elem x ys
  single _ _ = False

-- check whether a finite state machine is correct/complete:
checkFSM :: Eq a => FSM a -> Bool
checkFSM (qs, s, fs, d) = no_dups qs &&        -- (1)
                          elem s qs &&         -- (2)
                          subset fs qs &&      -- (3)
                          func3 qs sigma qs d  -- (4)
                           
-- All functions below assume that the machine is correct

-- ap ts q a == the unique q' such that (q, a, q') is in ts;  assumes success
ap :: Eq a => [(a,Char,a)] -> a -> Char -> a 
ap ((q1, a1, q2):ts) q a = if (q1,a1) == (q,a) then q2 else ap ts q a

delta :: Eq a => FSM a -> a -> Char -> a
delta (_, _, _, d) = ap d
                                             
delta_star :: Eq a => FSM a -> a -> [Char] -> a
delta_star = foldl . delta

accept1 :: Eq a => FSM a -> [Char] -> Bool
accept1 m@(_, s, fs, _) w = elem (delta_star m s w) fs

accept2_aux :: Eq a => FSM a -> a -> [Char] -> Bool
accept2_aux m@(_, _, fs, _) q [] = elem q fs
accept2_aux m q (a:w) = accept2_aux m (delta m q a) w

accept2 :: Eq a => FSM a -> [Char] -> Bool
accept2 m@(_, s, _, _) w = accept2_aux m s w

-- odd_bs is a machine that accepts strings with an odd number of b's
-- states: (number of b's read so far) mod 2
odd_bs :: FSM Int
odd_bs = ([0,1], 0, [1], [(i, a, d i a) | i <- [0,1], a <- sigma]) where
  d i a = if a == 'b' then (i+1) `mod` 2 else i

-- avoid w is a machine that accepts strings that don't have w as a substring
-- states: prefixes of w read so far (with w itself as a trap state)
avoid :: String -> FSM String
avoid xs = (qs, "", init qs, [(w, a, d w a) | w <- qs, a <- sigma]) where
  qs = inits xs
  d w a = if w == xs then w else head [w | w <- tails (w++[a]), isPrefixOf w xs]

-- no_aab is a machine that accepts strings that don't have "aab" as a substring
no_aab :: FSM String
no_aab = avoid "aab"

-- ends_in w is a machine that accepts strings that end in w
-- states: last <= n characters read (in reverse order, for ease of computation)
-- Note: this machine has 2^(n+1) - 1 states when |sigma| = 2
ends_in :: String -> FSM String
ends_in xs = (qs, "", [reverse xs], [(w, a, d w a) | w <- qs, a <- sigma]) where
  n = length xs
  qs = strings n
  d w a = take n (a : w)

-- ends_in_ab is a machine that accepts strings ending in "ab"
ends_in_ab :: FSM String
ends_in_ab = ends_in "ab"


---------------- Some additional useful functions --------------------------

-- Normalize a list, i.e., sort and remove (now adjacent) duplicates.
-- Two lists determine the same set if they normalize to the same list
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


---------------- Lab 6 begins here -----------------------------------------

-- Complete the FSM constructions for the regular expression operators
-- and test your functions adquately


-- Machine that accepts the empty language
emptyFSM :: FSM Int
emptyFSM = ([0],0,[],[(0,x,0) | x <- sigma])


-- Machine that accepts the language {"a"} where a in sigma
letterFSM :: Char -> FSM Int
letterFSM a = ([0,1,2], 0, [1], [(q,x, d q x)| q<-[0..2], x<-sigma]) where
d q' s' = if (q',s') == (0,'a') then 1 else 2


-- Machine that accepts the language {w} where w in sigma*
-- You can change the type of states from Int to something else if you like
stringFSM :: String -> FSM Int
stringFSM w = ([0..n+1], 0 , [n+1], [(q,s, d q s)| q<-[0..n+1], s<-sigma])where
  n = length w
  d q' s' = if q' <= n && w !! (q') == s' then q'+1 else n+1


-- Machine that accepts the union of the languages accepted by m1 and m2
unionFSM :: (Eq a, Eq b) => FSM a -> FSM b -> FSM (a, b)
unionFSM (qs1, s1, fs1, d1) (qs2, s2, fs2, d2) = (qs, s, fs, d) where
  qs = qs1 >< qs2
  s  = (s1,s2)
  fs = [(a,b)| (a,b)<-qs, a `elem` fs1, b `elem` fs2]
  d  = [(q,a,(_unionFSM q a))| q<-qs, a<-sigma, f'<-fs1, f1'<- fs2] where
  _unionFSM (x,y) a = ((delta (qs1, s1, fs1, d1) x a), (delta (qs2, s2, fs2, d2) y a))


-- Machine that accepts the concatenation of the languages accepted by m1 and m2
catFSM :: (Eq a, Ord b) => FSM a -> FSM b -> FSM (a, [b])
catFSM (qs1, s1, fs1, d1) (qs2, s2, fs2, d2) = (qs, s, fs, d) where
  qs = [(q,X)| q<-qs1, subsequences X qs2, union X [s2| elem q fs1]]
  s  = (s1, empty1) where empty1 = [a| elem s1 fs2]
  fs = [(q,x)| subsequences x qs2, q<-qs1, (q,x)<-qs, overlap X fs2]
  d  = [(y,(z X a))| a<-sigma] where
  z = delta qs1 a
  y x2 a = [delta(q2, a, fs2, d2)| q2<- qs2, elem q2 x2]


-- Machine that accepts the Kleene star of the language accepted by m1
starFSM :: Ord a => FSM a -> FSM [a]
starFSM (qs1, s1, fs1, d1) = (qs, s, fs, d) where
  qs = undefined
  s  = undefined
  fs = undefined
  d  = undefined



---------------- Bonus Features (for testing and experimenting) ------------

-- reachable m == the part of m that is reachable from the start state
reachable :: Ord a => FSM a -> FSM a
reachable m@(qs, s, fs, d) = (qs', s, fs', d') where
  qs' = sort $ stable $ iterate close ([s], [])
  fs' = filter (`elem` fs) qs'
  d'  = filter (\(q,_,_) -> q `elem` qs') d
  stable ((fr,qs):rest) = if null fr then qs else stable rest
  close (fr, xs) = (fr', xs') where
    xs' = fr ++ xs
    fr' = norm $ filter (`notElem` xs') (concatMap step fr)
    step q = map (ap d q) sigma

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

---- Regular expressions, along with output and input
data RegExp = Empty
             | Let Char
             | Union RegExp RegExp
             | Cat RegExp RegExp
             | Star RegExp

instance (Show RegExp) where    -- use precedence to minimize parentheses
  showsPrec d Empty         = showString "@"
  showsPrec d (Let c)    = showString [c]
  showsPrec d (Union r1 r2) = showParen (d > 6) $  -- prec(Union) = 6
                              showsPrec 6 r1 .
                              showString "+" .
                              showsPrec 6 r2
  showsPrec d (Cat r1 r2)   = showParen (d > 7) $  -- prec(Cat) = 7
                              showsPrec 7 r1 .
                              showsPrec 7 r2
  showsPrec d (Star r1)     = showsPrec 9 r1 .     -- prec(Star) = 8
                              showString "*"

-- Quick and dirty postfix regex parser, gives non-exaustive match on error
toRE :: String -> RegExp
toRE w = toRE' w [] where
  toRE' [] [r] = r
  toRE' ('+':xs) (r2:r1:rs) = toRE' xs (Union r1 r2:rs)
  toRE' ('.':xs) (r2:r1:rs) = toRE' xs (Cat r1 r2:rs)
  toRE' ('*':xs) (r:rs) = toRE' xs (Star r:rs)
  toRE' ('@':xs) rs = toRE' xs (Empty:rs)
  toRE' (x:xs) rs = toRE' xs (Let x:rs)

-- Use constructions above to get reduced machine associated with regex
-- Warning: it can take a lot of time/memory to compute these for "big" regex's
-- We will see much better ways later in the course
re2fsm :: RegExp -> FSM Int
re2fsm Empty = emptyFSM
re2fsm (Let c) = letterFSM c
re2fsm (Union r1 r2) = reduce $ unionFSM (re2fsm r1) (re2fsm r2)
re2fsm (Cat r1 r2) = reduce $ catFSM (re2fsm r1) (re2fsm r2)
re2fsm (Star r1) = reduce $ starFSM (re2fsm r1)