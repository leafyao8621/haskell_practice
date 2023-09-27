removeDuplicateHelper :: [String] -> [String]
removeDuplicateHelper [] = []
removeDuplicateHelper [x] = []
removeDuplicateHelper [x0, x1]
    | x0 == x1 = [""]
    | otherwise = [x1]
removeDuplicateHelper (x0:x1:xs)
    | x0 == x1 = "" : removeDuplicateHelper (x1:xs)
    | otherwise = x1 : removeDuplicateHelper (x1:xs)

removeDuplicate :: [String] -> [String]
removeDuplicate [] = []
removeDuplicate x = take 1 x ++ removeDuplicateHelper x

main = do
    print $ removeDuplicate []
    print $ removeDuplicate ["a"]
    print $ removeDuplicate ["a", "a"]
    print $ removeDuplicate ["a", "b"]
    print $ removeDuplicate ["a", "a", "b", "c", "c", "d"]
    print $ removeDuplicate ["a", "a", "b", "c", "c", "d", "e"]
    print $ removeDuplicate ["a", "a", "b", "c", "c", "c", "d", "e"]
