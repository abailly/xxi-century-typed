module Crt where

import Debug.Trace

-- | Chinese Remainder Theorem constructive proof.
-- Given a list of coprime numbers and a list of factors, find the (unique up to congruence modulo N)
-- number x that's equal to each factor a_i moduloe n_i.
--
-- See
-- * [this page](https://crypto.stanford.edu/pbc/notes/numbertheory/crt.html]
-- * Another page with [2 algorithms](https://www.di-mgt.com.au/crt.html)
--
-- >>> crt ((11,6) :| [(16,13),(21,9),(25,19)])
-- 89469
--
-- >>> crt ((3,2) :| [(5,3),(7,2)])
-- 23
--
-- >>> crt ((5,2) :| [(7,3)])
-- 17
--
-- >>> crt ((3,0) :| [(4,3),(5,4)])
-- 39
crt :: [(Integer, Integer)] -> Integer
crt coeffs =
  trace (show coeffs) $ sum (fmap term coeffs) `mod` m
  where
    m = product (fmap fst coeffs) -- fsts are assumed to be coprimes
    term (n_i, a_i) =
      let z_i = m `div` n_i
          (y_i, _, _) = extended_gcd z_i n_i
          y_i' = if y_i < 0 then n_i + y_i else y_i
       in a_i * y_i' * z_i

(×) :: (Num a) => a -> a -> a
(×) = (*)

-- | Extended GCD computation
-- This function computes the GCD of two numbers and also the Bezout coefficients of
-- the equation @n * a + m * b = gcd@
-- See [wikipedia page](https://en.wikipedia.org/wiki/Extended_Euclidean_algorithm)
--
-- >>> extended_gcd 240 46
-- (-9, 47, 2)
extended_gcd :: Integer -> Integer -> (Integer, Integer, Integer)
extended_gcd a b = extended_gcd' 0 1 b 1 0 a
  where
    extended_gcd' _ _ 0 old_s old_t old_r = (old_s, old_t, old_r)
    extended_gcd' s t r old_s old_t old_r =
      let quotient = old_r `div` r
       in extended_gcd' (old_s - quotient × s) (old_t - quotient × t) (old_r - quotient × r) s t r

-- from https://github.com/byorgey/comprog-hs/blob/master/NumberTheory.hs
egcd :: Integer -> Integer -> (Integer, Integer, Integer)
egcd a 0
  | a < 0     = (-a,-1,0)
  | otherwise = (a,1,0)
egcd a b = (g, y, x - (a `div` b) * y)
  where
    (g,x,y) = egcd b (a `mod` b)

gcrt :: [(Integer, Integer)] -> Maybe (Integer, Integer)
gcrt [e]        = Just e
gcrt (e1:e2:es) = gcrt2 e1 e2 >>= \e -> gcrt (e:es)

gcrt2 :: (Integer, Integer) -> (Integer, Integer) -> Maybe (Integer, Integer)
gcrt2 (a,n) (b,m)
  | a `mod` g == b `mod` g = Just (((a*v*m + b*u*n) `div` g) `mod` k, k)
  | otherwise              = Nothing
  where
    (g,u,v) = egcd n m
    k = (m*n) `div` g
