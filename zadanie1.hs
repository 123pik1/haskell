import Data.List (group, sort, maximumBy)
import Data.Ord (comparing)

-- znadjuje a,b,c i sprawdza czy tworzą one trójkąt prostokatny oraz sprawdza czy w sumie dają x, zwraca listę trójek
pythagoreanTriplets :: Int -> [(Int, Int, Int)]
pythagoreanTriplets x = [(a, b, c) | a <- [1..x], b <- [a..x], let c = x - a - b, a^2 + b^2 == c^2, a + b + c == x]

-- generuje x, a potem zlicza dla każdego wygenerowanego x trójki takie że a+b+c=x i tworzące trójkąt prostokątny, 
-- zwraca liczbę x i ilość trójek w tuplu
countTriplets :: Int -> [(Int, Int)]
countTriplets n = [(x, length (pythagoreanTriplets x)) | x <- [1..n], not (null (pythagoreanTriplets x))]

-- zwraca listę x dla których jest najwięcej trójek
maxTriplets :: Int -> [Int]
maxTriplets n = map fst ( filter ((== maxCount) . snd) tripletCounts)
  where
    tripletCounts = countTriplets n
    maxCount = maximum (map snd tripletCounts)

-- zwraca listę x dla których jest najwięcej trójek
findMaxTriplets :: Int -> [Int]
findMaxTriplets n = maxTriplets n

-- testy
main :: IO ()
main = do
  let n = 40
  print (findMaxTriplets n)
  let n1 = 200
  print (findMaxTriplets n1)
