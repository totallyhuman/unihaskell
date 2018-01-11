{-# LANGUAGE UnicodeSyntax #-}

import Data.List

infixl 9 ‼
infixl 7 ×, ÷, %, //

infixr 9 ∘
infixr 5 ⋅
infixr 3 ∧, ∨

infix 5 ∩, ∪, ∖
infix 4 ≡, ≢, ≠, ≤, ≥, ∣, ∤, ∈, ∉, ∋, ∌, ⊆, ⊇, ⊈, ⊉, ⊂, ⊃, ⊄, ⊅

(∧)   ∷ Bool → Bool → Bool
(∧)   = (&&)

(∨)   ∷ Bool → Bool → Bool
(∨)   = (||)

(≡)   ∷ Eq a ⇒ a → a → Bool
(≡)   = (==)

(≢)   ∷ Eq a ⇒ a → a → Bool
(≢)   = (/=)

(≠)   ∷ Eq a ⇒ a → a → Bool
(≠)   = (/=)

(≤)   ∷ Ord a ⇒ a → a → Bool
(≤)   = (<=)

(≥)   ∷ Ord a ⇒ a → a → Bool
(≥)   = (>=)

(×)   ∷ Num a ⇒ a → a → a
(×)   = (*)

(÷)   ∷ Fractional a ⇒ a → a → a
(÷)   = (/)

(//)  ∷ Integral a ⇒ a → a → a
(//)  = div

(%)   ∷ Integral a ⇒ a → a → a
(%)   = mod

(∣)   ∷ Integral a ⇒ a → a → Bool
a ∣ b = a % b == 0

(∤)   ∷ Integral a ⇒ a → a → Bool
(∤)   = (not ∘) ∘ (∣)

π     ∷ Floating a ⇒ a
π     = pi

(∘)   ∷ (b → c) → (a → b) → a → c
(∘)   = (.)

(⋅)   ∷ [a] → [a] → [a]
(⋅)   = (++)

(‼)   ∷ [a] → Int → a
(‼)   = (!!)

(∈)   ∷ (Foldable t, Eq a) ⇒ a → t a → Bool
(∈)   = elem

(∉)   ∷ (Foldable t, Eq a) ⇒ a → t a → Bool
(∉)   = notElem

(∋)   ∷ (Foldable t, Eq a) ⇒ t a → a → Bool
(∋)   = flip elem

(∌)   ∷ (Foldable t, Eq a) ⇒ t a → a → Bool
(∌)   = flip notElem

(∩)   ∷ Eq a ⇒ [a] → [a] → [a]
(∩)   = intersect

(∪)   ∷ Eq a ⇒ [a] → [a] → [a]
(∪)   = union

(∖)   ∷ Eq a ⇒ [a] → [a] → [a]
(∖)   = (\\)

(⊆)   ∷ Eq a ⇒ [a] → [a] → Bool
(⊆)   = isInfixOf

(⊇)   ∷ Eq a ⇒ [a] → [a] → Bool
(⊇)   = flip isInfixOf

(⊈)   ∷ Eq a ⇒ [a] → [a] → Bool
(⊈)   = (not ∘) ∘ isInfixOf

(⊉)   ∷ Eq a ⇒ [a] → [a] → Bool
(⊉)   = (not ∘) ∘ flip isInfixOf

(⊂)   ∷ Eq a ⇒ [a] → [a] → Bool
a ⊂ b = (a ⊆ b) ∧ (a ≠ b)

(⊃)   ∷ Eq a ⇒ [a] → [a] → Bool
a ⊃ b = (a ⊇ b) ∧ (a ≠ b)

(⊄)   ∷ Eq a ⇒ [a] → [a] → Bool
(⊄)   = (not ∘) ∘ (⊂)

(⊅)   ∷ Eq a ⇒ [a] → [a] → Bool
(⊅)   = (not ∘) ∘ (⊃)
