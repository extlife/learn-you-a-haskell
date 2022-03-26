module Ch06.Core where

import Data.List ( find, group, isPrefixOf, nub, sort, tails )
import Data.Char ( ord, digitToInt, chr, isDigit )
import qualified Data.Map as Map

numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub

wordNums :: String -> [(String, Int)]
wordNums = map (\ws -> (head ws, length ws)) . group . sort . words

isIn :: (Eq a) => [a] -> [a] -> Bool
isIn needle haystack = any (needle `isPrefixOf`) (tails haystack)

encode :: Int -> String -> String
encode offset = map (\c -> chr $ ord c + offset)

decode :: Int -> String -> String
decode shift = encode (negate shift)

digitSum :: Int -> Int
digitSum = sum . map digitToInt . show

firstTo40 :: Maybe Int
firstTo40 = find (\x -> digitSum x == 40) [1 ..]

firstTo :: Int -> Maybe Int
firstTo n = find (\x -> digitSum x == n) [1 ..]

phoneBook :: Map.Map String String
phoneBook = Map.fromList
    [ ("betty","555-2938")
    , ("bonnie","452-2928")
    , ("patsy","493-2928")
    , ("lucille","205-2928")
    , ("wendy","939-8282")
    , ("penny","853-2492")
    ]

findKey :: (Eq k) => k -> [(k, v)] -> Maybe v
findKey key = foldr (\(k, v) acc -> if key == k then Just v else acc) Nothing

stringTodigit :: String -> [Int]
stringTodigit = map digitToInt . filter isDigit

phoneBookToMap :: (Ord k) => [(k, a)] -> Map.Map k [a]
phoneBookToMap = Map.fromListWith (++) . map (\(k, v) -> (k, [v]))
