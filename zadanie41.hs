--WIP dokończę jutro
import Data.List (find)
import Data.Maybe (fromMaybe)

-- Funkcja obliczająca sumę kwadratów dzielników liczby n
sigma2 :: Integer -> Integer
sigma2 n = sum [d^2 | d <- [1..n], n `mod` d == 0]

-- Funkcja sprawdzająca, czy liczba jest kwadratem liczby naturalnej
isPerfectSquare :: Integer -> Bool
isPerfectSquare n = let root = floor (sqrt (fromIntegral n)) in root * root == n

-- Funkcja znajdująca największe y ≤ x, takie że σ2(y) jest kwadratem liczby naturalnej
largestY :: Integer -> Integer
largestY x = fromMaybe 0 $ find (\y -> isPerfectSquare (sigma2 y)) [x, x-1..1]

-- Główna funkcja do testowania
main :: IO ()
main = do
    putStrLn "Podaj x:"
    input <- getLine
    let x = read input :: Integer
    --let result = sigma2 x
    let result = largestY x
    putStrLn $ "Największe y ≤ " ++ show x ++ " takie, że σ2(y) jest kwadratem to: " ++ show result
