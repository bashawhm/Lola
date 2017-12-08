module Parse where 
import Data.Char

{-
This sets parser composers to be right associative
 and gives theie precedence
-}
infixr 5 >*>
infixr 5 <*<
infixr 5 >*<
infixr 5 >*>>

{-  
Generally a parse takes a sring of characters
  and returns all possible parses of that string
It returns a list of pairs, which consist of
  1. output of the parse
  2. The characters left in the spring
The output of the parse is of type b
Note we have generalize things so that instead of
  saying a parser takes a list of characters,
  we say a parset takes a list of a's.
-}
type Parse a b = [a] -> [(b,[a])]
  
{-
none is a parser that always fails, so has no parses
I can't think of a good example where it is used
-}
none :: Parse a b
none inp = []

{-
succeed is a parser that always succeeds
  and parses as the argument passed to it
-}
succeed :: b -> Parse a b
succeed val inp = [(val,inp)]


--token looks for a particular character
token :: Eq a => a -> Parse a a 
token t = spot (==t)

{-
spot cheks if character satisies a predicate
  a predicate is just a boolean fuction
-}
spot :: (a -> Bool) -> Parse a a
spot p (x:xs)
  | p x = [(x,xs)]
  | otherwise = []
spot p [] = []

{-
alt takes two parsers and gives all parses
  resulting from either one
Think of it like an OR
-}
alt :: Parse a b -> Parse a b -> Parse a b
alt p1 p2 inp = p1 inp ++ p2 inp

{-
>*> composes two parsers together
It returns a pair of their two parses
-}
(>*>) :: Parse a b -> Parse a c -> Parse a (b,c)
(>*>) p1 p2 inp
  = [((y,z),rem2) | (y,rem1) <- p1 inp, (z,rem2) <- p2 rem1]

-- >*>> is just like >*> but is passes along its value    
(>*>>) :: Parse a b -> (b -> Parse a c) -> Parse a c
(>*>>) p1 p2 inp
  = [(z,rem2) | (y,rem1) <- p1 inp, (z,rem2) <- p2 y rem1]

-- <*< is just like >*> but it ignores the first value  
(<*<) :: Parse a b -> Parse a c -> Parse a c
(<*<) p1 p2 = (p1 >*> p2) `build` snd

-- >*< is just like >*> but it ignores the second value 
(>*<) :: Parse a b -> Parse a c -> Parse a b
(>*<) p1 p2 = (p1 >*> p2) `build` fst  

-- build applies a function to the value parsed
build :: Parse a b -> (b -> c) -> Parse a c
build p f inp = [(f x,rem) | (x,rem) <- p inp]

-- list creates a list out of what was parsed
list :: Parse a b -> Parse a [b]
list p = (succeed []) `alt`
         ((p >*> list p) `build` (uncurry (:)))

-- parses a string instead of a single character
tokens :: Eq a => [a] -> Parse a [a]
tokens [] = succeed []
tokens (t:ts) = (token t >*> tokens ts) `build` (uncurry (:))

{-
Parses a string and finds all the parses that consume the whole string
  - if there is more than one parse then the grammar is wrong
  - if there are no parses then the input is wrong
  - if there is one parse it returns that parse
-}
parse :: (Parse Char b) -> String -> b
parse f s
  | null parses = error "There are no parses"
  | (not . null . tail) parses = error "Too many parses"
  | otherwise = head parses
    where parses = map fst $ filter (null . snd) (f (filter (not . isSpace) s))
