-- (\x -> x * 2) 3
-- (\x -> x * 2) 9


sumSquareOrSquareSum x y = if sumSquare > squareSum
                           then sumSquare
                           else squareSum
    where sumSquare = x ^ 2 + y ^ 2
          squareSum = (x + y) ^ 2

sumSquareOrSquareSum' x y = if (x ^ 2 + y ^ 2) > ((x + y) ^ 2)
                           then x ^ 2 + y ^ 2
                           else (x + y) ^ 2

body sumSquare squareSum = if sumSquare > squareSum
                           then sumSquare
                           else squareSum

sumSquareOrSquareSum'' x y = body (x ^ 2 + y ^ 2) ((x + y) ^ 2)

-- body' = (\sumSquare squareSum ->
--         if sumSquare > squareSum
--         then sumSquare
--         else squareSum)


doubleDouble x = dubs * 2
    where dubs = x * 2

doubleDouble' x = (\a -> a * 2) (x * 2)


sumSquareOrSquareSum''' x y =
    let sumSquare = x ^ 2 + y ^ 2
        squareSum = (x + y) ^ 2
    in if sumSquare > squareSum
       then sumSquare
       else squareSum


overwrite x =
    let x = 2
    in  let x = 3
        in let x = 4
           in  x

-- overwrite' x =
--     (x ->
--         (\x ->
--             (\x -> x) 4
--         ) 3
--     ) 2


-- Q3.1 Practice writing lambda functions by rewriting each
-- function in lesson 3 as a lambda expression
-- all functions already presented in lambda form...


-- Q3.2 Using a let expression and a lambda function aren’t
-- exactly the same thing under the hood. For example, the
-- following code will cause an error if you try to run it:
--  counter x = let x = x + 1
--              in
--               let x = x + 1
--                in
--                 x
-- To prove that let and lambda aren’t identical, rewrite
-- the counter function exactly as it is here, but use
-- nested lambdas instead of let.
-- (Hint: Start at the end.)
counter x =
    (\x -> x + 1)
        ((\x -> x + 1)
            ((\x -> x) x))
