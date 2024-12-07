import Data.List (group, sort, maximumBy)
import Data.Ord (comparing)

-- znadjuje a,b,c i sprawdza czy tworzą one trójkąt prostokatny oraz sprawdza czy w sumie dają x, zwraca listę trójek
trojkiPitagorejskie :: Int -> [(Int, Int, Int)]
trojkiPitagorejskie x = [(a, b, c) | a <- [1..x], b <- [a..x], let c = x - a - b, a^2 + b^2 == c^2, a + b + c == x]

-- generuje x, a potem zlicza dla każdego wygenerowanego x trójki takie że a+b+c=x i tworzące trójkąt prostokątny, 
-- zwraca liczbę x i ilość trójek w tuplu
zliczanieTrojek :: Int -> [(Int, Int)]
zliczanieTrojek n = [(x, length (trojkiPitagorejskie x)) | x <- [1..n], not (null (trojkiPitagorejskie x))]

-- zwraca listę x dla których jest najwięcej trójek
maxZtrojek :: Int -> [Int]
maxZtrojek n = map fst ( filter ((== maxCount) . snd) tripletCounts)
  where
    tripletCounts = zliczanieTrojek n
    maxCount = maximum (map snd tripletCounts)

-- zwraca listę x dla których jest najwięcej trójek
findMaxZtrojek :: Int -> [Int]
findMaxZtrojek n = maxZtrojek n

-- testy
main :: IO ()
main = do
  let n = 40
  print (findMaxZtrojek n)
  let n1 = 200
  print (findMaxZtrojek n1)
