bizarre g x = let m z = g z in
                m x

mango f u v = f v u

dingo w =  let plip k m = w k m in
              let plop n = plip n 484000 in
                  plop 17

plys x y = x + 4

ninka x = 0 - x

zap x y = x*y

mina = mango plys (bizarre ninka) (dingo zap)