import Debug.Trace

fib :: Int -> Int
fib 0 = 1
fib n = (trace ("First value is: " ++ show (n)) (fib (n - 1))) + 1