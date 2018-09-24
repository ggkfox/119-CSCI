-- CSci 119, Lab 3

-- Except when you're explicitly asked to provide a RECURSIVE definition,
-- you are free to use any functions available in the Data.List library
-- See http://hackage.haskell.org/package/base-4.11.1.0/docs/Data-List.html
import Data.List


---- String operations (note: String = [Char])

-- Length, concatenation, and reverse (RECURSIVELY, as in Def 2.2).
-- These reimplement Haskell's len, (++), and reverse
strlen :: String -> Int
strlen [] = 0
strlen (x:xs) = 1 + strlen xs

strcat :: String -> String -> String
strcat [] ys = ys
strcat (x:xs) ys = x:strcat xs ys

strrev :: String -> String
strrev [] = []
strrev (x:xs) = (strrev xs) ++ [x]


---- Language operations. Assume inputs contain no duplicates, and insure that
---- outputs also contain no duplicates. You might find "nub" useful.

type Language = [String]

-- Reimplementation of elem for Languages, RECURSIVELY
element :: String -> Language -> Bool
element xs [] = False
element xs (l:ls) = if xs == l then True else element xs ls

-- L1 subset L2, RECURSIVELY
subset :: Language -> Language -> Bool
subset [] l2 = False
subset (l:l1) l2 = if or([l == x| x<-l2]) then True else subset l1 l2


-- Regular operations on languages (with bounded Kleene star to keep it finite)
-- Remember: no duplicates in the result

-- Union of languages, L1 U L2
union_lang :: Language -> Language -> Language
union_lang [] l2 = l2
union_lang (l:l1) l2 = nub(l:union_lang l1 l2)

-- Concatenation of languages, L1 * L2 (Def 2.7)
concat_lang :: Language -> Language -> Language
concat_lang l1 l2 = [strcat a b | a<-l1, b<-l2]

-- L^n = L * L * ... * L (n times)
power_lang :: Language -> Int -> Language
power_lang l 0 = [""]
power_lang l 1 = l
power_lang l n = concat_lang l (power_lang l (n-1))

-- Bounded Kleene star, L* = L^0 U L^1 U L^2 U ... U L^n (n is the "bound")
bstar_lang :: Language -> Int -> Language
bstar_lang l 0 = [""]
bstar_lang l n = union_lang (power_lang l n) (bstar_lang l (n-1))

-- Extra operations

-- Left and right quotients of a language by a string (Def 2.16)
leftq :: String -> Language -> Language
leftq s [] = []
leftq s (l:ls)  | (s == take (length s) l) = [(drop (length s) ls)] ++ (leftq s ls)
                | otherwise = [] ++ (leftq s ls)

rightq :: String -> Language -> Language
rightq s l = undefined

-- Nonempty part of a language (Def 2.18)
nepart :: Language -> Language
nepart l = undefined


---- Regular expressions and the languages they denote 
data RegExp = Empty                 -- Empty language
             | Let Char             -- Single letter language
             | Cat RegExp RegExp    -- Concatenation
             | Union RegExp RegExp  -- Union
             | Star RegExp          -- Kleene star
             deriving (Show, Eq)

-- [[r]], except use bound 5 for Kleene star. This should be defined 
-- recursively on RegExp using the regular operations you defined above.
lang_of :: RegExp -> Language
lang_of Empty = []
lang_of (Let r) = [[r]]
lang_of (Union r1 r2) = union_lang(lang_of r1) (lang_of r2) 
lang_of (Cat r1 r2) = concat_lang (lang_of r1) (lang_of r2) 
lang_of (Star r) = bstar_lang (lang_of r) 5 


-- The one-string and finite languages of Theorem 3.2. It should be the case
-- that, for any string w, lang_of (onestr w) == [w], and for any (finite) list
-- of (distinct) strings l, lang_of (finite l) == l.
onestr :: String -> RegExp
onestr xs = undefined

finite :: Language -> RegExp
finite l = undefined


-- Test all of the above operations extensively!            
