is_prime_helper :: Int -> Int -> Int
is_prime_helper n i | n /= i = -1
is_prime_helper n i | n < 2 = 0
is_prime_helper n i =
    f i where
    f 2 = 1
    f x = if n `mod` (x - 1) /= 0 then f (x - 1) else 0

is_prime :: Int -> Int
is_prime x = is_prime_helper x x

prime_cnt :: Int -> Int
prime_cnt x | x < 2 = 0
prime_cnt 2 = 1
prime_cnt x = is_prime x + prime_cnt (x - 1)

main = print(prime_cnt 100)
