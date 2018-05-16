module Tools where

maxim :: (Ord a) => [a] -> (a, Int)
maxim l =
  let pmaxim :: (Ord a) => [a] -> Int -> (a, Int) -- Internal function to do the work
      pmaxim [] _  = error "Empty list"           -- List is empty, error
      pmaxim [x] xi = (x, xi)                     -- List has one item, return it and the index
      pmaxim (x:xs) xi                            -- More than one item, break list apart
        | x > t     = (x, xi)                     -- If current item is bigger, return it and its index
        | otherwise = (t, ti)                     -- If list tail has a bigger item, return that
        where (t, ti) = pmaxim xs (xi + 1)        -- Get max of tail of the list
  in pmaxim l 0