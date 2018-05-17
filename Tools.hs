module Tools where

maxim :: (Ord a) => [a] -> (a, Int)
maxim l =
  let pmaxim :: (Ord a) => [a] -> Int -> (a, Int)
      pmaxim [x] xi = (x, xi)
      pmaxim (x:xs) xi
        | x > t     = (x, xi)
        | otherwise = (t, ti)
        where (t, ti) = pmaxim xs (xi + 1)
  in pmaxim l 0