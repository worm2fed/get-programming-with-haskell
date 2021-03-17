simple x = x


calcChange owed given = if given - owed > 0
                        then given - owed
                        else 0

calcChange' owed given = if change > 0
                         then change
                         else 0
    where change = given - owed


doublePlusTwo x = doubleX + 2
    where doubleX = x * 2


-- let x = simple simple
-- let x = 6
-- the final result of x will be 6


-- Q2.1 You used Haskell’s if then else expression to write
-- calcChange. In Haskell, all if statements must include
-- an else component. Given our three rules for functions,
-- why can’t you have an if statement all by itself?

-- because we have to return something


-- Q2.2 Write functions named inc, double, and square that
-- increment, double, and square an argument n, respectively
inc x = x + 1
double x = x * 2
square x = x * x


-- Q2.3 Write a function that takes a value n. If n is even,
-- the function returns n - 2, and if the number is odd, the
-- function returns 3 × n + 1. To check whether the number
-- is even, you can use either Haskell’s even function or
-- mod (Haskell’s modulo function).
checkN n = if even n
    then n - 2
    else 3 * n + 1
