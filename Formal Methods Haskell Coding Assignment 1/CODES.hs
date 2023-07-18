-- 19F-0916 Muhammad Abdullah SE (4A) Formal Methods Assignment # 1 Haskell Codes


--Question # 1 Merging sorted array, even if it is not sorted at the time of insertion

--First Way by using two functions

sortList :: (Ord a) => [a] -> [a]
sortList [] = []
sortList (h:[]) = [h]
sortList (h:hs)=
				let lesser = sortList [ a | a <- hs, a <= h]
				    higher = sortList [ a | a <- hs, a > h]
				 in lesser ++ [h] ++ higher 
		
mergeSorted x y = sortList (x ++ y) 

--Second Way by using one function

mergeSorted' :: (Ord a) => [a] -> [a] -> [a]
mergeSorted' firstRem [] = firstRem                       
mergeSorted' [] secondRem = secondRem
mergeSorted' (first:firstRem) (second:secondRem)
			 | (first <= second) = first : mergeSorted' firstRem (second:secondRem)
			 | otherwise = second : mergeSorted' (first:firstRem) secondRem
		
--------------------------------------------------
---------------QUESTION # 1 ENDED-----------------
-------------------------------------------------- 



--Question # 2 Taking 2 list in ordered pair after splitting a whole list into half	
	

halfit :: [a] -> ([a],[a])
halfit [] = ([],[])
halfit (xs:[]) = ([xs],[])
halfit xs = (take temp xs, drop temp xs ) 
			where temp = (length xs) `div` 2
		

		 
--------------------------------------------------
---------------QUESTION # 2 ENDED-----------------
--------------------------------------------------



--Question # 3 Sorting array/list on the time of insertion
-- PS : I'll be taking same function for sorting from 1st question 

mergeSort :: (Ord a) => [a] -> [a]
mergeSort [] = []
mergeSort (h:[]) = [h]
mergeSort (h:hs)=
				let lesser = mergeSort [ a | a <- hs, a <= h]
				    higher = mergeSort [ a | a <- hs, a > h]
				 in lesser ++ [h] ++ higher

--------------------------------------------------
---------------QUESTION # 3 ENDED-----------------
--------------------------------------------------



--Question # 4 Using binary search for searching element from list

--using halfit function to distribute the incoming list for searching

halfit' :: [a] -> ([a],[a])
halfit' [] = ([],[])
halfit' (xs:[]) = ([xs],[])
halfit' xs = (take temp xs, drop temp xs ) 
			where temp = (length xs) `div` 2

binarySearch :: (Eq a) => a -> [a] -> String
binarySearch x [] = error "Empty List"
binarySearch x [y] = if x == y
						then " Value Found in List"
						else " Value Not Found in List"
binarySearch x xs = if x `elem` (fst temp)
					then binarySearch x (fst temp)
					else binarySearch x (snd temp)
				where temp = halfit' xs



--------------------------------------------------
---------------QUESTION # 4 ENDED-----------------
--------------------------------------------------

-- 19F-0916 Muhammad Abdullah SE (4A) Formal Methods Assignment # 1 Haskell Codes--