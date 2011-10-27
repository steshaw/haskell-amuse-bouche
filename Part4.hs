module Part4 where

runLengthEncode :: Eq a => [a] -> [(a, Int)]
runLengthEncode [] = []
runLengthEncode (x:xs) = nextGroup x 1 xs 
  where
    nextGroup e n [] = [(e, n)]
    nextGroup e n (y:ys)
      | e == y    =          nextGroup e (n + 1) ys
      | otherwise = (e, n) : nextGroup y  1      ys


prop_rleLengthPreserved :: [Int] -> Bool
prop_rleLengthPreserved as = length as == (sum $ map snd $ runLengthEncode as)

prop_rleDupesCollapsed :: Int -> Bool
prop_rleDupesCollapsed n
  | m == 0    = runLengthEncode "" == []
  | otherwise = runLengthEncode (replicate m 'x') == [('x', m)]
  where m = n `mod` 100

prop_rleRoundTrip :: [Int] -> Bool
prop_rleRoundTrip ns = runLengthEncode xs == is
  where is = zip ['a'..] $ map (\n -> n `mod` 100 + 1) ns
        xs = concatMap (\(i,n) -> replicate n i) is
