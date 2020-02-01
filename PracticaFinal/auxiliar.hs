data Tip = A | B Int Tip | C (Int,Tip,Tip)
data Tap = A | B (Int,Bool) | A (Int,Int,Tap)