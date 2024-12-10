-- suma kwadratów dzielników liczby n
sigmaBoy :: Integer -> Integer
sigmaBoy n = sum [d^2 | d <- [1..n], n `mod` d == 0]

-- sprawdza czy liczba jest kwadratem liczby całkowitej
isSquareOfInt :: Integer -> Bool
isSquareOfInt n = let root = floor (sqrt (fromIntegral n)) in root * root == n

-- znajdowanie największego y < x spełniającego warunki
largestY :: Integer -> Integer
largestY x = checkedNumber x
  where
    checkedNumber y
      | y < 1 = error "Nie znaleziono"
      | isSquareOfInt (sigmaBoy y) = y
      | otherwise = checkedNumber (y - 1)

main :: IO ()
main = do
    input <- getLine
    let x = read input :: Integer
    let result = largestY x
    putStrLn $ show result
