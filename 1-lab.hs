---- CSci 119, Lab 1 ----
---- joshua holland  ---- 

-- Note: you will replace all instances of "undefined" below with your answers.


---- Boolean operators

-- The following code tests whether "and" is commutative:
bools = [True, False]
and_commutes = and [(p && q) == (q && p) | p <- bools, q <- bools]

-- Write similar defintions that test whether "or" is commutative,
-- "and" and "or" are associative, "and" distributes over "or",
-- "or" distributes over "and"
or_commutes = and [(p || q) == (q || p)                      | p <- bools, q <- bools]
and_assoc   = and [(a && (b && c)) == (b && (a && c))        | a <- bools, b <- bools, c <- bools]
or_assoc    = and [(a || (b || c)) == (b || (a || c))        | a <- bools, b <- bools, c <- bools]
and_dist    = and [(a && (b || c)) == ((a && b) || (a && c)) | a <- bools, b <- bools, c <- bools]
or_dist     = and [(a || (b && c)) == ((a || b) && (a || c)) | a <- bools, b <- bools, c <- bools]
          
-- The exclusive-or operation on Bool in Haskell is equivalent to /=.
-- Test the properties of this operation (commutativity, associativity,
-- distributivity over and+or, and distributivity of and+or over it)
-- using the same method as above
xor_commutes = and [(a /= b) == (b /= a)                      | a <- bools, b <- bools]
xor_assoc    = and [(a /= (b /= c)) == (b /= (a /= c))        | a <- bools, b <- bools, c <- bools]
xor_dist_and = and [(a /= (b && c)) == ((a /= b) && (a /= c)) | a <- bools, b <- bools, c <- bools]
xor_dist_or  = and [(a /= (b || c)) == ((a /= b) || (a /= c)) | a <- bools, b <- bools, c <- bools]
and_dist_xor = and [(a && (b /= c)) == ((a && b) /= (a && c)) | a <- bools, b <- bools, c <- bools]
or_dist_xor  = and [(a || (b /= c)) == ((a || b) /= (a || c)) | a <- bools, b <- bools, c <- bools]
               
-- The implication operator on Bool in Haskell is equivalent to <=.
-- Check whether implication is associative or commutative:
assoc_imp = and [(a <= (b <= c)) == ((a <= b) <= c) | a <- bools, b <- bools, c <- bools]
comm_imp  = and [(a <= b) == (a >= b)               | a <- bools, b <- bools, c <- bools]


----- Evaluating statements involving quantifiers

-- Assume that the universe of discourse is the set {1,2,3,4,5,6,7,8},
-- that is, that the word "number" temporarily means 1, 2, ..., 8.
-- Your solutions to the problems below should work no matter what
-- finite list of integers u is. For example, u = [5, 2, 17, 58, 21].

u = [1..8]

-- Translate each of the following statements first, in a comment, into a
-- logical statement involving forall, exists, and, or, imp, and not,
-- and then into Haskell code that checks ("brute force") whether
-- the statement is true. I'll work one example.

-- 1. "Every number that's greater than 2 is greater than 1"
-- A: forall n, (n > 2) imp (n > 1)
prob1_1 = and [(n > 2) <= (n > 1) | n <- u]   -- direct translation
prob1_2 = and [n > 1 | n <- u, n > 2]         -- using bounded quantifier

-- 2. Every number is either greater than 1 or less than 2
-- A: forall n, (n > 1) or (n < 2)
prob2 = and [(n > 1) || (n < 2) | n <- u]

-- 3. Every two numbers are comparable with <= (i.e., either one is <=
--    the other or vice-versa)
-- A: forall n, there exists a number where (n <= m) or (m <= n)
prob3 = and [a <= b || b <= a | a <- u, b <- u]

-- 4. There is an odd number greater than 4
-- A: there exists an n, where n > 4
prob4 = or [n > 4 | n <- u, odd n]

-- 5: There are two odd numbers that add up to 10
-- A: there exists n and m, where n%2=0, m%2=0, n + m = 10
prob5 = or [(n + m == 10) |n <- u, m <- u, odd n, odd m]

-- 6. For every odd number, there is a greater even number (use the Haskell
--    predicates odd, even :: Integral a => a -> Bool)
-- A: for all n, there exists m, where (n%2==1) && (m%2==0) and (m > n).
prob6 = and [ or [n < m | m <- u, even m] | n <- u, odd n]

-- 7. For every even number, there is a greater odd number
-- A: 
prob7 = and [ or [n < m | m <- u, odd m] | n <- u, even n]

-- 8. There are two odd numbers that add up to 6
-- A: there exists a and b, where a%2==1 and b%2==1 and a+b == 6
prob8 = or [ a + b == 6 | a <- u, b <- u, odd a, odd b]

-- 9. There is a number that is at least as large as every number
--    (i.e., according to >=)
-- A: there exists a, for all of b , where a >= b
prob9 = or [ and [ a >= b | b <- u] | a <- u]

-- 10. For every number, there is a different number such that there are no
--    numbers between these two.
-- A: for all a, there exists b where a+1==b || a-1==b.
prob10 = and [ or [ not ( or [ a < c && c > b | c <- u ] ) | b <- u] | a <- u]
