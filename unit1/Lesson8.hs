myLength []     = 0
myLength (_:xs) = 1 + myLength xs

myTake _ [] = []
myTake 0 _  = []
myTake n (x:xs) = x : rest
    where rest = myTake (n - 1) xs

finiteCycle (first:rest) = first:rest ++ [first]

myCycle (first:rest) = first:myCycle (rest ++ [first])


ackerman 0 n = n + 1
ackerman m 0 = ackerman (m - 1) 1
ackerman m n = ackerman (m - 1) (ackerman m (n - 1))

collatz 1 = 1
collatz n = if even n
            then 1 + collatz (n `div` 2)
            else 1 + collatz (n * 3 + 1)


-- Q8.1 Implement your own version of reverse, which
-- reverses a list.
myReverse []     = []
myReverse [x]    = [x]
myReverse (x:xs) = myReverse xs ++ [x]


-- Q8.2 Calculating Fibonacci numbers is perhaps the
-- single most common example of a recursive function. The
-- most straightforward definition is as follows:
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

-- Like the Ackermann function, this implementation quickly
-- explodes due to the mutually recursive calls. But unlike
-- the Ackermann function, there’s a much more efficient way
-- to compute the nth Fibonacci number. Write a function,
-- fastFib, that can compute the 1,000th Fibonacci number
-- nearly instantly.
-- Hint: fastFib takes three arguments: n1, n2, and counter.
-- To calculate the 1,000th Fibonacci number, you call
-- fastFib 1 1 1000 and for the 5th, you call fastFib 1 1 5.
fastFib _ _ 0         = 0
fastFib _ _ 1         = 1
fastFib _ _ 2         = 1
fastFib n1 n2 3       = n1 + n2
fastFib n1 n2 counter = fastFib (n1 + n2) n1 (counter - 1)
fib' n = fastFib 1 1 n
