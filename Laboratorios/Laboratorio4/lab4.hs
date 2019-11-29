-- Alumno: Víctor Manuel Cavero Gracia

import Data.List
import Data.Char

data Nat =  Cero | Suc Nat deriving (Show, Eq, Ord)

suc :: Nat -> Nat
suc = Suc

suma :: Nat -> Nat -> Nat
Cero `suma` n = n
(Suc n) `suma` a = suc $ n `suma` a

producto :: Nat -> Nat -> Nat
Cero `producto` n = Cero
(Suc n) `producto` a = suc $ n `producto` a

natToInt :: Nat -> Int 
natToInt Cero = 0
natToInt (Suc n) = (natToInt n) + 1