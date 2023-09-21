merge :: [Int] -> [Int] -> [Int]
merge [] [] = []
merge [x] [] = [x]
merge [] [y] = [y]
merge x [] = x
merge [] y = y
merge [x] [y] = if x < y then [x, y] else [y, x]
merge [x] (y:ys) =
    f (y:ys) where
    f (yy:yys) =
        if x < yy then
        [x] ++ (merge [] (yy:yys)) else
        [yy] ++ (merge [x] yys)
merge (x:xs) [y] =
    f y where
    f yy =
        if x < yy then
        [x] ++ (merge xs [yy]) else
        [yy] ++ (merge (x:xs) [])
merge (x:xs) (y:ys) =
    f (y:ys) where
    f (yy:yys) =
        if x < yy then
        [x] ++ (merge xs (yy:yys)) else
        [yy] ++ (merge (x:xs) yys)

mergeSort :: [Int] -> [Int]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort x =
    merge
        (mergeSort (take ((length x) `div` 2) x))
        (mergeSort (drop ((length x) `div` 2) x))

main = print $ mergeSort [8, 3, 2, 6, 1, 4, 5, 7, 9, 10]
