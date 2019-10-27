f1 x y = if x < y then x else y

f2 x y = x (y + 1)

f3 x y = (x y) + 1

f4 x y z = x y (y z)

mcd :: Integral a => a -> a -> a
mcd a 0 = a
mcd a b = mcd b (rem a b)
