--Christoffer Lindkvist

import Data.List

-- Generererar x sublists av längd n, t.ex.
-- subListsWithLength 3 [1,2,3,4,5] ger oss: [[1,2,3],[2,3,4],[3,4,5]]
subListsWithLength :: Int -> [a] -> [[a]]
subListsWithLength n xs@(_:rest) | length xs < n = []
                                 | otherwise = take n xs : subListsWithLength n rest
subListsWithLength _ _ = []

-- Genererar alla möjliga subsets av en lista
subsets :: [a] -> [[a]]
subsets [] = [[]]
subsets (x:xs) = map (x:) (subsets xs) ++ subsets xs --map appliar her x i början av alla subsets t.ex. om x = 1: till [2,3], [3,4], [2,4] blir 1, 2, 3; 1, 3, 4; 1, 2, 4

-- Räknar ihop summan av ett givet subset, t.ex. [1, 2] ger 3
sumSubsets :: [Int] -> [(Int, [Int])]
sumSubsets [] = [(0, [])]
sumSubsets xs = [(sum xs', xs') | n <- [1..length xs], xs' <- subListsWithLength n xs]

-- Sorterar subset efter storlek, t.ex. [-99] hamnar före [-98]
insertionSort :: Ord a => [(a, [b])] -> [(a, [b])]
insertionSort [] = []
insertionSort (x:xs) = insert x (insertionSort xs)
    where insert x [] = [x]
          insert x (y:ys) = if fst x <= fst y then x:y:ys else y:insert x ys

-- getIndex of i and j
getIndex :: Eq a => [a] -> [a] -> Maybe (Int, Int)
getIndex subset list = do
    let startIndexMaybe = findIndex (isPrefixOf subset) (tails list) --hittar startindexen av subsetet
        endIndexMaybe = findIndex (isSuffixOf (reverse subset)) (reverse (tails (reverse list))) --hittar slutindexen av subsetet
    startIndex <- maybe (Just (-1)) Just startIndexMaybe  
    let endIndex = maybe (startIndex + length subset - 1) (\i -> length list - i - length subset) endIndexMaybe
    let startIndex' = startIndex +1
    let endIndex' = endIndex +1
    return (startIndex', endIndex')

--ksmallest
ksmallest :: [Int] -> Int -> [(Int, [Int])]
--fst returnar första värdet i en tupel
--concatmap genererar alla möjliga subsets av xs, sen sorteras de efter fst
ksmallest xs k = take k $ sortOn fst $ concatMap subsetsSum [1..length xs]
  where
    subsetsSum n = map (\ys -> (sum ys, ys)) $ subListsWithLength n xs

smallestKset :: [Int] -> Int -> IO ()
smallestKset xs k
  | k <= 0 = putStr "There are no sets to pick. :/"
  | otherwise = putStr $ smallestKstring (ksmallest xs k)
  where
    curlist = xs --curlist är listan xs så vi kan compara
    smallestKstring [] = "\n"
    smallestKstring ((size, lst):xs) = "size: [" ++ show size ++ "] " ++ printIndex ++ " - sublist: " ++ show lst  ++ "\n" ++ smallestKstring xs
      where
        printIndex = 
            case getIndex lst curlist of
            Just (i, j) -> " i: " ++ show i ++ " j: " ++ show j --pure print of i & j
            Nothing -> "Could not find indices"


-- Test case 1
list_test1 :: [Int]
list_test1 = map (\x -> x * (-1)^x) [1..100] --Genererar [-1, 2, -3, ... , 98, -99]

-- Test case 2
list_test2 :: [Int]
list_test2 =  [24,-11,-34,42,-24,7,-19,21]

--Test case 3
list_test3 :: [Int]
list_test3 = [3,2,-4,3,2,-5,-2,2,3,-3,2,-5,6,-2,2,3]



--Main function
main :: IO ()
main = do   
    --test case 1, k = 15
    smallestKset list_test1 15 
    --test case 2, k = 6
    smallestKset list_test2 6
    --test case 3, k = 8
    smallestKset list_test3 8