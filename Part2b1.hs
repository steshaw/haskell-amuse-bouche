--
-- An attempt to build a user-defined list with something like built-in syntax.
--
module Part2b1 where

import Data.List (intersperse)

-- Note: cannot use :: rather than ::: but the compiler error isn't clear.
data L a = Nil | a ::: (L a)
infixr 5 :::

instance Show a => Show (L a) where
  show xs = "[" ++ (intersperse ", " xs) ++ "]"
    where
      -- Similar to Data.List.intersperse (but that only works on built-in lists).
      -- See mkString below.
      intersperse :: (Show a) => String -> L a -> String
      intersperse separator Nil = ""
      intersperse separator (a ::: xs) = (show a) ++ (prependToAll separator xs)

      prependToAll :: (Show a) => String -> L a -> String
      prependToAll _ Nil = ""
      prependToAll separator (a ::: xs) = separator ++ (show a) ++ (prependToAll separator xs)

mkString :: (Show a) => String -> [a] -> String
mkString sep = concat . intersperse sep . map show

fromList :: [a] -> L a
fromList xs = foldr (:::) Nil xs

empty = Nil
oneWord = "apple" ::: Nil
twoWords = "banana" ::: "cantaloupe" ::: Nil
threeWords = "banana" ::: "cantaloupe" ::: "orange" ::: Nil

empty' = fromList []
oneWord' = fromList ["apple"]
twoWords' = fromList ["banana", "cantaloupe", "orange"]

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
