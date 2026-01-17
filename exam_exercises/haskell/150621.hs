import Data.List (intersect, (\\))

-- Lazily generates an infinite list of prime numbers using a highly optimized
-- Sieve of Eratosthenes. This is the foundation for all other functions.
-- It works by recursively using the list of generated primes to "sieve" a
-- list of candidates. The `span` optimization avoids filtering by primes
-- whose square is larger than the numbers being checked.
primes :: [Integer]
primes = 2 : 3 : sieve (tail primes) [5, 7 ..]
  where
    sieve (p : ps) xs = h ++ sieve ps [x | x <- t, x `rem` p /= 0]
      where
        (h, ~(_ : t)) = span (< p * p) xs

-- A recursive helper function to find prime factors. It is not intended to be
-- called directly, but through `primeFactors`.
findFactors :: Integer -> [Integer] -> [Integer]
findFactors n (p : ps)
  -- If the square of the current prime `p` is greater than the remaining
  -- number `n`, then `n` itself must be the last prime factor (if it's > 1).
  | p * p > n = [n | n > 1]
  -- If `n` is divisible by the current prime `p`, then `p` is a factor.
  -- We add `p` to our list and continue factoring the rest (`n `div` p`),
  -- reusing the same prime `p` to handle repeated factors (e.g., 12 = 2*2*3).
  | n `rem` p == 0 = p : findFactors (n `div` p) (p : ps)
  -- If `n` is not divisible by `p`, we move on to the next prime in the list.
  | otherwise = findFactors n ps

-- Checks if a number is prime.
-- For efficiency, it only checks for divisibility by primes up to the
-- square root of the number `n`.
isPrime :: Integer -> Bool
isPrime n = n > 1 && all (\p -> n `mod` p /= 0) (takeWhile (\p -> p * p <= n) primes)

-- Ex 1.
-- Calculates the list of prime factors for a given integer `n`.
primeFactors :: Integer -> [Integer]
primeFactors n
  -- Numbers less than 2 do not have prime factors.
  | n < 2 = []
  -- For other numbers, it starts the factorization process by calling the
  -- helper function with the full list of primes.
  | otherwise = findFactors n primes

-- Ex 2.
-- Finds the common prime numbers present in two lists of integers.
both :: [Integer] -> [Integer] -> [Integer]
both [] _ = []
both _ [] = []
-- It works by filtering each list to contain only primes, then finding the
-- intersection (common elements) of the two resulting lists.
both list1 list2 = filter isPrime list1 `intersect` filter isPrime list2

-- Ex 3.
-- Calculates the Least Common Multiple (LCM) of two integers using the
-- prime factorization method.
mylcm :: Integer -> Integer -> Integer
-- The LCM of any number and 0 is defined as 0. This is our edge case.
mylcm n m
  | n == 0 || m == 0 = 0
  | otherwise =
      let -- 1. Get the prime factor lists for both numbers.
          nFactors = primeFactors n
          mFactors = primeFactors m

          -- 2. Find the "extra" factors in the second list. This is a multiset
          --    difference, ensuring that we have enough factors for both numbers.
          --    e.g., lcm(12, 18) -> nFactors=[2,2,3], mFactors=[2,3,3].
          --    `extra` will be `[3]`.
          extra = mFactors \\ nFactors

          -- 3. The full list of factors for the LCM is the first list plus the
          --    extra factors from the second. e.g., [2,2,3] ++ [3] = [2,2,3,3].
          -- 4. The final result is the product of this combined list of factors.
          result = product (nFactors ++ extra)
       in result
