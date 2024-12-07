import Data.List (group, sort, maximumBy)
import Data.Ord (comparing)

-- Function to generate all Pythagorean triplets (a, b, c) such that a + b + c = x
pythagoreanTriplets :: Int -> [(Int, Int, Int)]
pythagoreanTriplets x = [(a, b, c) | a <- [1..x], b <- [a..x], let c = x - a - b, a^2 + b^2 == c^2, a + b + c == x]

-- Function to count the number of triplets for each x ≤ n
countTriplets :: Int -> [(Int, Int)]
countTriplets n = [(x, length (pythagoreanTriplets x)) | x <- [1..n], not (null (pythagoreanTriplets x))]

-- Function to find the maximum count and return all x values with this count
maxTriplets :: Int -> [Int]
maxTriplets n = map fst ( filter ((== maxCount) . snd) tripletCounts)
  where
    tripletCounts = countTriplets n
    maxCount = maximum (map snd tripletCounts)

-- Main function to get the result
findMaxTriplets :: Int -> [Int]
findMaxTriplets n = maxTriplets n

-- Example usage
main :: IO ()
main = do
  let n = 40
  print (findMaxTriplets n)