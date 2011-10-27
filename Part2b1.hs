-- Here is an attempt to build a user-defined list with something like built-in syntax.
module Part2b1 where

-- Note: cannot use :: rather than ::: but the compiler error isn't clear.
data L a = Nil | a ::: (L a)
infixr 5 :::

-- dodgy Show instance. Is confused by mystery3.
instance Show a => Show (L a) where
  show Nil = "[]"
  show xs = "[" ++ (showMiddle xs) ++ "]"
    where
      showMiddle Nil = ""
      showMiddle (a ::: b ::: xs) = (show a) ++ ", " ++ (show b) ++ (showMiddle xs)
      showMiddle (a ::: Nil) = (show a)

fromList :: [a] -> L a
fromList xs = foldr (:::) Nil xs

empty = Nil
oneWord = "apple" ::: Nil
twoWords = "banana" ::: "cantaloupe" ::: Nil

empty' = fromList []
oneWord' = fromList ["apple"]
twoWords' = fromList ["banana", "cantaloupe"]

mystery1 = "pear" ::: empty
mystery2 = "peach" ::: oneWord
mystery3 = "pineapple" ::: mystery3
-- mystery4 = 42 ::: "apple" ::: Nil -- won't compile

mystery1' = fromList ["pear"]
mystery2' = "peach" ::: oneWord
mystery3' = "pineapple" ::: mystery3
-- mystery4 = fromList [42, "apple"] -- won't compile

dropOne :: L a -> L a
dropOne (_:::rest) = rest
dropOne Nil = Nil

justOne :: L a -> L a
justOne (a:::_) = a:::Nil
justOne Nil = Nil
