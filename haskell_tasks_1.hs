f x = let dingo z = z + x
    in
        let
            bingo w = dingo (47 - w)
        in
            g (bingo 419)

g u = let mango x = x + h u
    in
        let
            dingo k = k
        in
            mango (dingo u)
h w = 42

dingo' (x) (z) = z + x

bingo' (x) (w) = dingo' (x) (47 - w)

f' () (x) = g' () (bingo' (x) (419))

mango' (u) (x) = x + h' () (u)

dingo'' (u) (k) = k

g' () (u) = mango' (u) (dingo'' (u) (u))

h' () (w) = 42