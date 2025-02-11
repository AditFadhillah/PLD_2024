bizarre g x = let m z = g z in
                m x
                
mango f u v = f v u

dingo w = let plip k m = w k m in
            let plop n = plip n 484000 in
            plop 17
            
plys x y = x + 4

ninka x = 0 - x

zap x y = x*y

mina = mango plys (bizarre ninka) (dingo zap)

m' (g) z = g z
bizarre' () g x = m' g x
mango' () f u v = f v u
plip' (w) k m = w k m
plop' (w) n = plip' (w) n 484000
dingo' () w = plop' (w) 17
plys' () x y = x + 4
ninka' () x = 0 - x
zap' () x y = x * y

mina' = mango' () plys  (bizarre' () ninka) (dingo' () zap)

-- reverse :: list(a) -> list(a)

-- notatall :: list([a, b], b) -> list(a)

-- Eq a => a list x a -> a list

-- in Haskell:
-- [a], a
-- [a], b