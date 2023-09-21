isPrimeHelper :: Int -> Int -> Int
isPrimeHelper n i | n /= i = -1
isPrimeHelper n i | n < 2 = 0
isPrimeHelper n i =
    f i where
    f 2 = 1
    f x = if n `mod` (x - 1) /= 0 then f (x - 1) else 0

isPrime :: Int -> Int
isPrime x = isPrimeHelper x x

primeCnt :: Int -> Int
primeCnt x | x < 2 = 0
primeCnt 2 = 1
primeCnt x = isPrime x + primeCnt (x - 1)

main = print(primeCnt 10000)
