import Data.List (sortOn)

-- Generererar x sublists av längd n, t.ex.
-- subListsWithLength 3 [1,2,3,4,5] ger oss: [[1,2,3],[2,3,4],[3,4,5]]
subListsWithLength :: Int -> [a] -> [[a]]
subListsWithLength n xs@(_:rest) | length xs < n = []
                                 | otherwise = take n xs : subListsWithLength n rest
subListsWithLength _ _ = []

-- Genererar alla möjliga subsets av en lista
subsets :: [a] -> [[a]]
subsets [] = [[]]
subsets (x:xs) = map (x:) (subsets xs) ++ subsets xs

-- Räknar ihop summan av ett givet subset
sumSubsets :: [Int] -> [(Int, [Int])]
sumSubsets [] = [(0, [])]
sumSubsets xs = [(sum xs', xs') | n <- [1..length xs], xs' <- subListsWithLength n xs]

-- Sorterar subset efter storlek
insertionSort :: Ord a => [(a, [b])] -> [(a, [b])]
insertionSort [] = []
insertionSort (x:xs) = insert x (insertionSort xs)
    where insert x [] = [x]
          insert x (y:ys) = if fst x <= fst y then x:y:ys else y:insert x ys


ksmallest :: [Int] -> Int -> [(Int, [Int])]
--fst returnar första värdet i en tupel
--concatmap genererar alla möjliga subsets av xs, sen sorteras de efter fst
ksmallest xs k = take k $ sortOn fst $ concatMap subsetsSum [1..length xs]
  where
    subsetsSum n = map (\ys -> (sum ys, ys)) $ subListsWithLength n xs

smallestKset :: [Int] -> Int -> IO ()
smallestKset xs k
  | k <= 0 = putStr "There are no sets to pick. :/"
  | otherwise = putStr $ smallestKstring (ksmallest xs k) -- Printar minsta K set
  where
    smallestKstring [] = "\n"
    smallestKstring ((size, lst):xs) =
      "size: " ++ show size ++ "  subset: " ++ show lst ++ "\n" ++ smallestKstring xs


-- Genererar [-1, 2, -3, ... , 98, -99]
list_test1 :: [Int]
list_test1 = map (\x -> x * (-1)^x) [1..100]

list_test2 :: [Int]
list_test2 =  [24,-11,-34,42,-24,7,-19,21]

list_test3 :: [Int]
list_test3 = [3,2,-4,3,2,-5,-2,2,3,-3,2,-5,6,-2,2,3]

-- Genererar [-1, 2, -3, 4, -5]
list_test :: [Int]
list_test = map (\x -> x * (-1)^x) [1..5]


--main function
main :: IO ()
main = do   
    --test case 1
    smallestKset list_test1 15 
    --test case 2
    smallestKset list_test2 6
    --test case 3
    smallestKset list_test3 8
