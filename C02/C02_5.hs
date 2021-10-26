numberOfRoots :: Int -> Int -> Int -> Int

numberOfRoots a b c =   let d = b*b - 4*a*c
                        in if d<0 then 0 else if d==0 then 1 else 2

