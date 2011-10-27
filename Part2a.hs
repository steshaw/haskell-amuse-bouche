module Part2a where

data List α = EndOfList
            | Link α (List α)
    deriving Show   -- makes printing out results possible

empty = EndOfList
oneWord = Link "apple" EndOfList
twoWords = Link "banana" (Link "cantaloupe" EndOfList)

mystery1 = Link "pear" empty
mystery2 = Link "peach" oneWord
mystery3 = Link "pineapple" mystery3
-- mystery4 = Link 42 (Link "apple" EndOfList) -- won't compile

dropOne :: List a -> List a
dropOne (Link first rest) = rest
dropOne EndOfList = EndOfList

-- Forgot the case of EndOfList. Need ghci -Wall to see the compiler warning!
dropOneOops :: List a -> List a
dropOneOops (Link first rest) = rest

justOne :: List a -> List a
justOne (Link a _) = Link a EndOfList
justOne EndOfList = EndOfList
