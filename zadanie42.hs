

square :: Int -> Int
square x = x^2

funkcjaT :: Int -> [Int]
funkcjaT t = [2* square x -1 | x <- [2..t]]

czyPierwsza :: Int -> Bool
czyPierwsza n = n > 1 && all (\x -> n `mod` x /= 0) [2..n-1]

zliczaniePierwszych :: [Int] -> Int
zliczaniePierwszych [] = 0
zliczaniePierwszych (x:xs) = if czyPierwsza x then 1 + zliczaniePierwszych xs else zliczaniePierwszych xs




main :: IO ()
main = do
  let t = 10
  print (zliczaniePierwszych (funkcjaT t))
  