main = do
  putStrLn "Hello, everybody!"
  putStrLn ("Please look at my favorite odd numbers: " ++ show (filter odd [10..20]))

-- Define the factorial function
factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- Main function to output the factorial of 5
fact :: IO ()
fact = do
    let result = factorial 5
    putStrLn ("The factorial of 5 is: " ++ show result)