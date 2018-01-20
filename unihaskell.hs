#!/usr/bin/env ghc

{-# LANGUAGE UnicodeSyntax #-}

{-|
Module      : UniHaskell
Description : Convenience/golfing library
Copyright   : (c) Sumant Bhaskaruni, 2018
License     : MIT
Maintainer  : bsumantb@gmail.com
Stability   : stable

A convenience library for Haskell that also functions as a golfing library.
-}

module UniHaskell where

import Data.Bits
import Data.List

infixl 9 ‼
infixl 8 ≪, ≫
infixl 7 ×, ÷, %, //, ⋏
infixl 6 ⊻
infixl 5 ⋎
infixl 4 §

infixr 9 ∘
infixr 5 ⋅
infixr 3 ∧, ∨, ⊕, ⊙, ⊼, ⊽

infix 5 ∩, ∪, ∖, ⊗
infix 4 ≡, ≢, ≠, ≤, ≥, ∣, ∤, ∈, ∉, ∋, ∌, ⊆, ⊇, ⊈, ⊉, ⊂, ⊃, ⊄, ⊅

-- | @a ⊷ b@ returns the first argument, or @a@.
(⊷)        ∷ a → b → a
a ⊷ b      = a

-- | @a ⊶ b@ returns the second argument, or @b@.
(⊶)        ∷ a → b → b
a ⊶ b      = b

-- | @a ∧ b@ returns the logical AND of @a@ and @b@.
(∧)        ∷ Bool → Bool → Bool
(∧)        = (&&)

-- | @a ∨ b@ returns the logical OR of @a@ and @b@.
(∨)        ∷ Bool → Bool → Bool
(∨)        = (||)

-- | @a ⊕ b@ returns the logical XOR of @a@ and @b@.
(⊕)        ∷ Bool → Bool → Bool
a ⊕ b      = (a ∨ b) ∧ (a ⊼ b)

-- | @a ⊙ b@ returns the logical XNOR of @a@ and @b@.
(⊙)        ∷ Bool → Bool → Bool
a ⊙ b      = (a ∧ b) ∨ (a ⊽ b)

-- | @a ⊼ b@ returns the logical NAND of @a@ and @b@.
(⊼)        ∷ Bool → Bool → Bool
(⊼)        = (not ∘) ∘ (∧)

-- | @a ⊽ b@ returns the logical NOR of @a@ and @b@.
(⊽)        ∷ Bool → Bool → Bool
(⊽)        = (not ∘) ∘ (∨)

-- | @a ≡ b@ returns whether @a@ is equal to @b@.
(≡)        ∷ Eq a ⇒ a → a → Bool
(≡)        = (==)

-- | @a ≢ b@ returns whether @a@ is inequal to @b@.
(≢)        ∷ Eq a ⇒ a → a → Bool
(≢)        = (/=)

-- | @a ≠ b@ returns whether @a@ is inequal to @b@.
(≠)        ∷ Eq a ⇒ a → a → Bool
(≠)        = (/=)

-- | @a ≤ b@ returns whether @a@ is lesser than or equal to @b@.
(≤)        ∷ Ord a ⇒ a → a → Bool
(≤)        = (<=)

-- | @a ≥ b@ returns whether @a@ is greater than or equal to @b@.
(≥)        ∷ Ord a ⇒ a → a → Bool
(≥)        = (>=)

-- | @a × b@ returns @a@ multiplied by @b@.
(×)        ∷ Num a ⇒ a → a → a
(×)        = (*)

-- | @a ÷ b@ returns @a@ divided by @b@.
(÷)        ∷ Fractional a ⇒ a → a → a
(÷)        = (/)

-- | @a // b@ returns @a@ floor divided by @b@.
(//)       ∷ Integral a ⇒ a → a → a
(//)       = div

-- | @a % b@ returns @a@ modulo @b@.
(%)        ∷ Integral a ⇒ a → a → a
(%)        = mod

-- | @a ⋏ b@ returns the bitwise AND of @a@ and @b@.
(⋏)        ∷ Int → Int → Int
(⋏)        = (.&.)

-- | @a ⋏ b@ returns the bitwise OR of @a@ and @b@.
(⋎)        ∷ Int → Int → Int
(⋎)        = (.|.)

-- | @a ≪ b@ returns @a@ with its bits shifted left by @b@ places.
(≪)        ∷ Int → Int → Int
(≪)        = shiftL

-- | @a ≫ b@ returns @a@ with its bits shifted right by @b@ places.
(≫)        ∷ Int → Int → Int
(≫)        = shiftR

-- | @a ⊻ b@ returns the bitwise XOR of @a@ and @b@.
(⊻)        ∷ Int → Int → Int
(⊻)        = xor

-- | @a ∣ b@ returns whether @a@ evenly divides @b@, or @b % a = 0@.
(∣)        ∷ Integral a ⇒ a → a → Bool
a ∣ b      = b % a ≡ 0

-- | @a ∤ b@ returns whether @a@ does not evenly divide @b@, or @b % a ≠ 0@.
(∤)        ∷ Integral a ⇒ a → a → Bool
(∤)        = (not ∘) ∘ (∣)

-- | @a … b@ returns a range from @a@ to @b@.
(…)        ∷ (Enum a, Ord a) ⇒ a → a → [a]
a … b      | b ≥ a     = [a .. b]
           | otherwise = [b .. a]

-- | @π@ is a floating-point representation of pi.
π          ∷ Floating a ⇒ a
π          = pi

-- | @f ∘ g@ returns @f@ composed with @g@.
(∘)        ∷ (b → c) → (a → b) → a → c
(∘)        = (.)

-- | @xs ⋅ ys@ returns @xs@ concatenated with @ys@.
(⋅)        ∷ [a] → [a] → [a]
(⋅)        = (++)

-- | @xs ‼ n@ returns the @n@th element (0-indexed) of @xs@.
(‼)        ∷ [a] → Int → a
(‼)        = (!!)

-- | @f § x@ returns @f@ mapped over @x@.
(§)        ∷ Functor f ⇒ (a → b) → f a → f b
(§)        = fmap

-- | @x ∈ xs@ returns whether @x@ is an element of @xs@.
(∈)        ∷ (Foldable t, Eq a) ⇒ a → t a → Bool
(∈)        = elem

-- | @x ∉ xs@ returns whether @x@ is not an element of @xs@.
(∉)        ∷ (Foldable t, Eq a) ⇒ a → t a → Bool
(∉)        = notElem

-- | @xs ∋ x@ returns whether @xs@ contains @x@.
(∋)        ∷ (Foldable t, Eq a) ⇒ t a → a → Bool
(∋)        = flip (∈)

-- | @xs ∌ x@ returns whether @xs@ doesn't contain @x@.
(∌)        ∷ (Foldable t, Eq a) ⇒ t a → a → Bool
(∌)        = flip (∉)

-- | @xs ⊗ ys@ returns the cartesian product of @xs@ and @ys@.
(⊗)        ∷ [a] → [b] → [(a, b)]
a ⊗ b      = [(x, y) | x ← a, y ← b]

-- | @xs ∩ ys@ returns the intersection of @xs@ and @ys@.
(∩)        ∷ Eq a ⇒ [a] → [a] → [a]
(∩)        = intersect

-- | @xs ∪ ys@ returns the union of @xs@ and @ys@.
(∪)        ∷ Eq a ⇒ [a] → [a] → [a]
(∪)        = union

-- | @xs ∖ ys@ returns the set minus of @xs@ and @ys@.
(∖)        ∷ Eq a ⇒ [a] → [a] → [a]
(∖)        = (\\)

-- | @xs ⊆ ys@ returns whether @xs@ is a subset of or equal to @ys@.
(⊆)        ∷ Eq a ⇒ [a] → [a] → Bool
(⊆)        = isSubsequenceOf

-- | @xs ⊇ ys@ returns whether @xs@ is a superset of or equal to @ys@.
(⊇)        ∷ Eq a ⇒ [a] → [a] → Bool
(⊇)        = flip (⊆)

-- | @xs ⊈ ys@ returns whether @xs@ is not a subset of or equal to @ys@.
(⊈)        ∷ Eq a ⇒ [a] → [a] → Bool
(⊈)        = (not ∘) ∘ (⊆)

-- | @xs ⊉ ys@ returns whether @xs@ is not a superset of or equal to @ys@.
(⊉)        ∷ Eq a ⇒ [a] → [a] → Bool
(⊉)        = flip (⊈)

-- | @xs ⊂ ys@ returns whether @xs@ is a subset of @ys@.
(⊂)        ∷ Eq a ⇒ [a] → [a] → Bool
a ⊂ b      = (a ⊆ b) ∧ (a ≠ b)

-- | @xs ⊃ ys@ returns whether @xs@ is a superset of @ys@.
(⊃)        ∷ Eq a ⇒ [a] → [a] → Bool
(⊃)        = flip (⊂)

-- | @xs ⊄ ys@ returns whether @xs@ is not a subset of @ys@.
(⊄)        ∷ Eq a ⇒ [a] → [a] → Bool
(⊄)        = (not ∘) ∘ (⊂)

-- | @xs ⊅ ys@ returns whether @xs@ is not a superset of @ys@.
(⊅)        ∷ Eq a ⇒ [a] → [a] → Bool
(⊅)        = flip (⊄)

divisors   ∷ Integral a ⇒ a → [a]
divisors n = filter (∣ n) [1..n]

-- | @factors n@ returns the prime factors of @n@.
factors    ∷ Integral a ⇒ a → [a]
factors n
    | isPrime n = [n]
    | otherwise = i : factors (n // i)
        where i = head (dropWhile (∤ n) (drop 1 primes))

-- | @isPrime n@ returns whether @n@ is prime.
isPrime    ∷ Integral a ⇒ a → Bool
isPrime 2  = True
isPrime 3  = True
isPrime n
    | (2 ∣ n) ∨ (3 ∣ n) = False
    | otherwise         = check 5 2
  where check i w
            | i × i > n = True
            | i ∣ n     = False
            | otherwise = check (i + w) (6 - w)

-- | @primes@ is an infinite list of prime numbers.
primes     ∷ Integral a ⇒ [a]
primes     = filter isPrime [1..]

-- | @prime n@ returns the @n@th (0-indexed) prime number.
prime      ∷ Integral a ⇒ Int → a
prime      = (primes ‼)

-- | @fibList@ is an infinite list of Fibonacci numbers (@[0, 1, 1, 2...]@).
fibList    ∷ Integral a ⇒ [a]
fibList    = 0 : 1 : zipWith (+) fibList (tail fibList)

-- | @fib n@ returns the @n@th (0-indexed) Fibonacci number.
fib        ∷ Integral a ⇒ Int → a
fib        = (fibList ‼)

-- | @isFib n@ returns whether @n@ is a Fibonacci number.
isFib      ∷ Integral a ⇒ a → Bool
isFib n    = head (dropWhile (< n) fibList) ≡ n

-- | @deltas xs@ returns the incremental differences of @xs@.
deltas     ∷ Num a ⇒ [a] → [a]
deltas l   = zipWith (-) (tail l) l

-- | @find p xs@ finds the first element of @xs@ for which @p@ holds true.
find       ∷ (a → Bool) → [a] → a
find       = (head ∘) ∘ dropWhile 
