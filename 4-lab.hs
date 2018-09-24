-- CSci 119, Lab 4
-- Joshua Holland

---- Regular expressions, along with input and output
data RegExp = Empty
             | Letter Char
             | Union RegExp RegExp
             | Cat RegExp RegExp
             | Star RegExp

instance (Show RegExp) where    -- use precedence to minimize parentheses
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

-- Quick and dirty postfix regex parser, gives non-exaustive match on error
toRE :: String -> RegExp
toRE w = toRE' w [] where
  toRE' [] [r] = r
  toRE' ('+':xs) (r2:r1:rs) = toRE' xs (Union r1 r2:rs)
  toRE' ('.':xs) (r2:r1:rs) = toRE' xs (Cat r1 r2:rs)
  toRE' ('*':xs) (r:rs) = toRE' xs (Star r:rs)
  toRE' ('@':xs) rs = toRE' xs (Empty:rs)
  toRE' (x:xs) rs = toRE' xs (Letter x:rs)


---------------- Part 1 ----------------

-- Implement the seven recursive predications/operations on RegExp given in
-- Section 3.3 of the notes. Each should begin with a type declaration.
-- Include several tests for each function.

--emptiness
emptiness :: RegExp -> Bool
emptiness Empty = True
emptiness (Letter a) = False
emptiness (Union a b) = emptiness (a) && emptiness (b)
emptiness (Cat a b) = emptiness (a) || emptiness (b)
emptiness (Star a) = False

--unitarity
unitarity :: RegExp -> Bool
unitarity Empty = False
unitarity (Letter a) = False
unitarity (Union a b) = or [unitarity a && emptiness b, unitarity a && emptiness b, unitarity a && unitarity b]
unitarity (Cat a b) = undefined
unitarity (Star a) = undefined

--bypassability
bypassability :: RegExp -> Bool
bypassability Empty = False
bypassability (Letter a) = False
bypassability (Union a b) = undefined
bypassability (Cat a b) = undefined
bypassability (Star a) = undefined

--infiniteness
infiniteness :: RegExp -> Bool
infiniteness Empty = False
infiniteness (Letter a) = False
infiniteness (Union a b) = undefined
infiniteness (Cat a b) = undefined
infiniteness (Star a) = undefined

--reversal
reversal :: RegExp -> RegExp
reversal Empty = undefined
reversal (Letter a) = undefined
reversal (Union a b) = undefined
reversal (Cat a b) = undefined
reversal (Star a) = undefined

--leftQuotient
-- leftQuotient :: Char -> RegExp -> RegExp
-- leftQuotient Empty = undefined
-- leftQuotient (Letter a) = undefined
-- leftQuotient (Union a b) = undefined
-- leftQuotient (Cat a b) = undefined
-- leftQuotient (Star a) = undefined

--nonemptyPart
nonemptyPart :: RegExp -> RegExp
nonemptyPart Empty = undefined
nonemptyPart (Letter a) = undefined
nonemptyPart (Union a b) = undefined
nonemptyPart (Cat a b) = undefined
nonemptyPart (Star a) = undefined




---------------- Part 2 ----------------

-- Implement the two matching algorithms given in Section 3.4 of the notes.
-- Call them match1 and match2. Start by implementing splits:

-- splits xs = list of all possible splits of xs, in order. For example,
-- splits "abc" = [("","abc"), ("a","bc"), ("ab","c"), ("abc","")]
splits :: [a] -> [([a], [a])]
splits xs = undefined


match1 :: RegExp -> String -> Bool
match1 r w = undefined


match2 :: RegExp -> String -> Bool
match2 r w = undefined



-- Some regular expressions for testing. Also, test them on other solutions
-- to the exercises of Section 3.2 (as many as you can get).

ab   = toRE "aa.bb.+*"            -- every letter is duplicated
ttla = toRE "ab+*a.ab+.ab+."      -- third to last letter is a
ena  = toRE "b*a.b*.a.*b*."       -- even number of a's
bb1  = toRE "aba.+*b.b.aab.+*."   -- contains bb exactly once