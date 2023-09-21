getDenomination :: Int -> [Int] -> [Int]
getDenomination x d | x < 0 = error "Negative value"
getDenomination x [] = []
getDenomination x d =
    f [d] where
    f [y:ys] = [x `div` y] ++ (getDenomination (x `mod` y) ys)

hasExactChange :: Int -> [Int] -> Bool
hasExactChange x d | x < 0 = error "Negative value"
hasExactChange x [] = (x == 0)
hasExactChange x d =
    f [d] where
    f [y:ys] = hasExactChange (x `mod` y) ys

main = do
    mapM_
        (\ i -> do
            print i
            print [100, 50, 20, 10, 5, 1]
            print $ getDenomination i [100, 50, 20, 10, 5, 1]
            print $ hasExactChange i [100, 50, 20, 10, 5, 1]
            print [100, 50, 20, 10, 5]
            print $ getDenomination i [100, 50, 20, 10, 5]
            print $ hasExactChange i [100, 50, 20, 10, 5]
        ) [200..300]


