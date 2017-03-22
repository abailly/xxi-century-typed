-- data Reachable {A : Set} (_R_ : A → A → Set) : ℕ → (A → A → Set) where
--   [_] : ∀ {i j : A} {m : ℕ}   → i R j → Reachable _R_ (suc m) i j
--   _∷_ : ∀ {i j k : A} {m : ℕ} → i R j → Reachable _R_ m j k → Reachable _R_ (suc m) i k

-- stretch : ∀ {A : Set} {_R_ : A → A → Set} {m n : ℕ} {i j : A} (↔ᵃᵐ : A ↔ Fin m) →
--   Reachable _R_ n i j → Reachable _R_ m i j
-- stretch = {!!}

import Data.Fin

data Reachable : {a : Type} -> (rel : a -> a -> Type) -> Nat -> (i : a) -> (j : a) -> Type where
  One     : (m : Nat) -> rel i j                        -> Reachable rel (S m) i j
  Compose : (m : Nat) -> rel i j -> Reachable rel m j k -> Reachable rel (S m) i k

stretch : Reachable {a = Fin m} rel n i j -> Reachable {a = Fin m } rel m i j
stretch (One Z x) = ?stretch_rhs_1
stretch (One (S k) x) = ?stretch_rhs_4
stretch (Compose m y z) = ?stretch_rhs_2

