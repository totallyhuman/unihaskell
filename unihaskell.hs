
{-# LANGUAGE UnicodeSyntax #-}

module UniHaskell
    ( (∧), (∨), (⊕), (⊙), (⊼), (⊽)
    , (≡), (≢), (≠)
    , (≤), (≥)
    , π
    , (÷), (×), (%), (//)
    , (∘)
    , (‼), (⋅), (∩), (∪), (∖)
    , (⊆), (⊇), (⊈), (⊉), (⊂), (⊃), (⊄), (⊅)
    , (∈), (∉), (∋), (∌)
    , (∣), (∤)
    , isPrime, prime, primes
    , fib, fibList
    ) where

import Data.List

infixl 9 ‼
infixl 7 ×, ÷, %, //

infixr 9 ∘
infixr 5 ⋅
infixr 3 ∧, ∨, ⊕, ⊙, ⊼, ⊽

infix 5 ∩, ∪, ∖
infix 4 ≡, ≢, ≠, ≤, ≥, ∣, ∤, ∈, ∉, ∋, ∌, ⊆, ⊇, ⊈, ⊉, ⊂, ⊃, ⊄, ⊅

-- Logical AND
(∧)       ∷ Bool → Bool → Bool
(∧)       = (&&)

-- Logical OR
(∨)       ∷ Bool → Bool → Bool
(∨)       = (||)

-- Logical XOR
(⊕)       ∷ Bool → Bool → Bool
a ⊕ b     = (a ∨ b) ∧ (a ⊼ b)

-- Logical XNOR
(⊙)       ∷ Bool → Bool → Bool
a ⊙ b     = (a ∧ b) ∨ (a ⊽ b)

-- Logical NAND
(⊼)       ∷ Bool → Bool → Bool
a ⊼ b     = not (a ∧ b)

-- Logical NOR
(⊽)       ∷ Bool → Bool → Bool
a ⊽ b     = not (a ∨ b)

-- Equal to
(≡)       ∷ Eq a ⇒ a → a → Bool
(≡)       = (==)

-- Not equal to
(≢)       ∷ Eq a ⇒ a → a → Bool
(≢)       = (/=)

-- Not equal to
(≠)       ∷ Eq a ⇒ a → a → Bool
(≠)       = (/=)

-- Less than or equal to
(≤)       ∷ Ord a ⇒ a → a → Bool
(≤)       = (<=)

-- Greater than or equal to
(≥)       ∷ Ord a ⇒ a → a → Bool
(≥)       = (>=)

-- Multiplication
(×)       ∷ Num a ⇒ a → a → a
(×)       = (*)

-- Division
(÷)       ∷ Fractional a ⇒ a → a → a
(÷)       = (/)

-- Integer division
(//)      ∷ Integral a ⇒ a → a → a
(//)      = div

-- Modulo
(%)       ∷ Integral a ⇒ a → a → a
(%)       = mod

-- Divisible by
(∣)       ∷ Integral a ⇒ a → a → Bool
a ∣ b     = a % b ≡ 0

-- Not divisible by
(∤)       ∷ Integral a ⇒ a → a → Bool
(∤)       = (not ∘) ∘ (∣)

-- Pi
π         ∷ Floating a ⇒ a
π         = pi

-- Function composition
(∘)       ∷ (b → c) → (a → b) → a → c
(∘)       = (.)

-- List concatenation
(⋅)       ∷ [a] → [a] → [a]
(⋅)       = (++)

-- List index
(‼)       ∷ [a] → Int → a
(‼)       = (!!)

-- Element of
(∈)       ∷ (Foldable t, Eq a) ⇒ a → t a → Bool
(∈)       = elem

-- Not an element of
(∉)       ∷ (Foldable t, Eq a) ⇒ a → t a → Bool
(∉)       = notElem

-- Contains
(∋)       ∷ (Foldable t, Eq a) ⇒ t a → a → Bool
(∋)       = flip (∈)

-- Does not contain
(∌)       ∷ (Foldable t, Eq a) ⇒ t a → a → Bool
(∌)       = flip (∉)

-- Intersection
(∩)       ∷ Eq a ⇒ [a] → [a] → [a]
(∩)       = intersect

-- Union
(∪)       ∷ Eq a ⇒ [a] → [a] → [a]
(∪)       = union

-- Set subtraction
(∖)       ∷ Eq a ⇒ [a] → [a] → [a]
(∖)       = (\\)

-- Subset of or equal to
(⊆)       ∷ Eq a ⇒ [a] → [a] → Bool
(⊆)       = isInfixOf

-- Superset of or equal to
(⊇)       ∷ Eq a ⇒ [a] → [a] → Bool
(⊇)       = flip (⊆)

-- Not subset of or equal to
(⊈)       ∷ Eq a ⇒ [a] → [a] → Bool
(⊈)       = (not ∘) ∘ (⊆)

-- Not superset of or equal to
(⊉)       ∷ Eq a ⇒ [a] → [a] → Bool
(⊉)       = flip (⊈)

-- Subset of
(⊂)       ∷ Eq a ⇒ [a] → [a] → Bool
a ⊂ b     = (a ⊆ b) ∧ (a ≠ b)

-- Superset of
(⊃)       ∷ Eq a ⇒ [a] → [a] → Bool
a ⊃ b     = flip (⊂)

-- Not subset of
(⊄)       ∷ Eq a ⇒ [a] → [a] → Bool
(⊄)       = (not ∘) ∘ (⊂)

-- Not superset of
(⊅)       ∷ Eq a ⇒ [a] → [a] → Bool
(⊅)       = flip (⊄)

-- Primality
isPrime   ∷ Int → Bool
isPrime 2 = True
isPrime 3 = True
isPrime n
    | (n ∣ 2) ∨ (n ∣ 3) = False
    | otherwise         = check 5 2
  where check i w
            | i × i ≤ n = if n ∣ i
                          then False
                          else check (i + w) (6 - w)
            | otherwise = True

-- Infinite list of primes
primes    ∷ [Int]
primes    = filter isPrime [1..]

-- Nth prime (0-indexed)
prime     ∷ Int → Int
prime     = (primes ‼)

-- Infinite Fibonacci list [0, 1, 1, 2...]
fibList   ∷ [Int]
fibList   = 0 : 1 : zipWith (+) fibList (drop 1 fibList)

-- Nth Fibonacci number
fib       ∷ Int → Int
fib       = (fibList ‼)
