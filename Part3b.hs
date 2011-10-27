module Part3b where

import Control.Applicative ((<|>))
import Data.List (isInfixOf)
import Data.Maybe (listToMaybe)

tvShows :: [(Int, String)] -- a list of pairs
tvShows =
  [ (1966, "Star Trek")
  , (1969, "Monty Python's Flying Circus")
  , (1989, "The Simpsons")
  , (2005, "Judging Amy")
  ]

showForYear :: Int -> Maybe String
showForYear y = lookup y tvShows
    -- lookup "lookup" w/Hoogle: http://www.haskell.org/hoogle/?hoogle=lookup

showWithName :: String -> Maybe String
showWithName n = (listToMaybe . filter (isInfixOf n) . map snd) tvShows
    -- for a good exercise, figure out what this does
    -- look these functions up in Hoogle (just follow the first hit for each)

favoriteShow :: String -> Maybe String
favoriteShow "Amy" = Just "Batman"
favoriteShow "Bob" = Just "Iron Chef"
favoriteShow _     = Nothing

data Person = Person { name :: String, year :: Int }
    -- This has "named" fields, which act as accessor functions

amy = Person { name = "Amy", year = 1971 }
cam = Person { name = "Cam", year = 1989 }
deb = Person { name = "Deb", year = 1967 }
monty = Person { name = "Monty", year = 1973 }

pickShow :: Person -> Maybe String
pickShow p =
  favoriteShow (name p)
    <|> showWithName (name p)
    <|> showForYear (year p)

-- Using >> gives the same type as pickShow but a much different result.
allShows' :: Person -> Maybe String
allShows' p =
  favoriteShow (name p)
    >> showWithName (name p)
    >> showForYear (year p)

eg1 = map pickShow [amy, cam, deb, monty]
-- [Just "Batman",Just "The Simpsons",Nothing,Just "Monty Python's Flying Circus"]

eg2 = map allShows' [amy, cam, deb, monty]
-- [Nothing,Nothing,Nothing,Nothing] -- oops!

--
-- Turns out that pickShow' compiles but produces the wrong answer.
-- It only gives a non Nothing result if each of the 3 ways of finding a show
-- are successful and then it evaluates to the last result in the chain.
-- If I add "Judging Amy" to the list of TV shows above, the following gives
-- an answer.
--
eg3 = allShows' (Person "Amy" 1966)
-- Just "Star Trek"

--
-- If we use >>=, we can find all the answers of the 3 different expressions (but
-- only if each succeeds).
--
allShows'' :: Person -> Maybe (String, String, String)
allShows'' p =
  favoriteShow (name p) >>= \show1 -> 
  showWithName (name p) >>= \show2 -> 
  showForYear (year p) >>= \show3 -> 
  return (show1, show2, show3)

eg4 = map allShows'' [amy, cam, deb, monty, Person "Amy" 1966]
-- [Nothing,Nothing,Nothing,Nothing,Just ("Batman","Judging Amy","Star Trek")]
