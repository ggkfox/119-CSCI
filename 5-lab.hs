-- CSci 119, Lab 5
-- joshua holland
-- Reference: Lecture notes, Sections 4.1, 4.2

-- For this (and most) labs, the alphabet will be {a,b} to keep testing easier,
-- but your code should work just as well for any finite list.
sigma = ['a', 'b']

-- Finite State Machine M = (Q, s, F, d)
-- states that exist, start states, finish states -- Q, a, Q'
type FSM = ([Int], Int, [Int], [(Int,Char,Int)])

-- function to help other functions
unique :: Eq a => [a] -> Bool
unique [] = True
unique (x:xs) = (not (elem x xs)) && (unique xs)

-- Check whether a finite state machine (qs, s, fs, ts) is correct/complete:
-- (1) States qs are unique (no duplicates)
-- (2) Start state is a state (s is in qs)
-- (3) Final states are states (fs is a subset of qs)
-- (4) Transition relation is a function from qs and sigma to qs (exactly one
--     output state for each state and letter from sigma)
checkFSM :: FSM -> Bool
checkFSM (qs, s, fs, ts) = unique qs && elem s qs && and[elem f qs | f <- fs] && and[elem b sigma | (a,b,c) <- ts] && unique [(a,b) | (a,b,c) <- ts]
--               states are unique | start is unique |  finish states exist   |       letter is in sigma        |   no state points to 2 diff states

-- Gives the transition function of the machine as a function
-- i.e., delta m q a = the state machine m goes to when reading a in state q
delta :: FSM -> Int -> Char -> Int
delta (qs, s, fs, ((a,b,f):ts)) q s' = if a == q && b == s' then f else delta (qs, s, fs, ts) q s'

-- Gives the delta* function (recursive in w)
delta_star :: FSM -> Int -> [Char] -> Int
delta_star m q [] = q
delta_star m q (w:ws) = delta_star m (delta m q w) ws

-- Machine acceptance, Definition 1 (via delta*)
accept1 :: FSM -> [Char] -> Bool
accept1 (qs, s, fs, ts) [] = False
accept1 (qs, s, fs, ts) (w:ws) = elem (delta (qs, s, fs, ts) s w) fs


-- Machine acceptance, Definition 2 (via L_q(M))

-- accept2_aux m q w = whether m, starting in q, accepts w (recursive in w)
accept2_aux :: FSM -> Int -> [Char] -> Bool
accept2_aux (qs,s,fs,ts) q [] = elem q fs
accept2_aux m q (w:ws) = accept2_aux m (delta m q w) ws

-- Acceptance, defined (non-recursively) in terms of accept2_aux
accept2 :: FSM -> [Char] -> Bool
accept2 (qs, s, fs, ts) w = accept2_aux (qs, s, fs, ts) s w


-- Define a machine that accepts exactly the strings with an odd number of b's
-- and test it adequately
test1 :: FSM
test1 = ([1,2],1,[2],[(1,'a',1),(1,'b',2),(2,'a',2),(2,'b',1)])

-- Define a machine that accepts exactly the strings that do not contain "aab"
-- as a substring and test it adequately
test2 :: FSM
test2 = ([1,2,3,4],1,[4],[(1,'a',2),(1,'b',1),(2,'a',3),(2,'b',1),(3,'a',3),(3,'b',4),(4,'a',4),(4,'b',4)])

-- Define a machine that accepts all strings that end in "ab" and test
test3 :: FSM
test3 = ([1,2,3],1,[3],[(1,'a',2),(1,'b',1),(2,'a',2),(2,'b',3),(3,'a',2),(3,'b',1)])