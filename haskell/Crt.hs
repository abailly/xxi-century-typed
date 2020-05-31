module Crt where

import Data.List.NonEmpty
import Debug.Trace

-- | Chinese Remainder Theorem constructive proof.
-- Given a list of coprime numbers and a list of factors, find the (unique up to congruence modulo N)
-- number x that's equal to each factor a_i moduloe n_i.
-- See [this page](https://crypto.stanford.edu/pbc/notes/numbertheory/crt.html]
crt :: NonEmpty (Integer, Integer) -> Integer
crt ((_,x) :| []) = x
crt ((n_1,a_1) :| ((n_2,a_2): moduli)) =
  let (m_1, m_2, 1) = extended_gcd n_1 n_2 -- n_1 and n_2 are co-primes so gcd is 1
      x = (a_1 * m_2 * n_2 + a_2 * m_1 * n_1) `mod` n_1 * n_2
  in trace (show x) $ crt ((n_1 * n_2, if x < 0 then (n_1 * n_2 + x) else x) :| moduli)

(×) :: (Num a) => a -> a -> a
(×) = (*)

-- | Extended GCD computation
-- This function computes the GCD of two numbers and also the Bezout coefficients of
-- the equation @n * a + m * b = gcd@
-- See [wikipedia page](https://en.wikipedia.org/wiki/Extended_Euclidean_algorithm)
--
-- >>> extended_gcd 240 46
-- (-9, 47, 2)
--
extended_gcd :: Integer -> Integer -> (Integer, Integer, Integer)
extended_gcd a b = extended_gcd' 0 1 b 1 0 a
  where
    extended_gcd' _ _ 0 old_s old_t old_r = (old_s, old_t, old_r)
    extended_gcd' s t r old_s old_t old_r =
      let quotient = old_r `div` r
      in extended_gcd' (old_s - quotient × s) (old_t - quotient × t) (old_r - quotient × r) s t r
