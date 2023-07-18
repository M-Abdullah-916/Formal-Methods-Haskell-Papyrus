-- 19F-0916 Muhammad Abdullah SE (4A) Formal Methods Assignment # 2 Haskell Codes

----------------------------------------------------
--Question # 1 Haskell Function for Prime Numbers
----------------------------------------------------

divides' :: (Integral a) => a -> a -> Bool              --Divides Function for checking
divides' x y = if x `mod` y == 0                        --Either Given Number is divible
			  then True                                --By 2nd number or not
			  else False

any' :: (Integral a) => a -> a -> Bool  --Uses Divides as a Higher order
any' x y 
	| sub > 1 && fun == False = any' x sub
	| sub <= 1 = True
	| otherwise = False
	where sub = y - 1
	      fun = divides' x sub
	
isPrime :: (Integral a) => (a -> a -> Bool) -> a -> Bool  --Used to Check either number is 
isPrime f z                                         --Prime or not
	| fun == True = True
	| otherwise = False
	where fun = any' z z

firstNPrime :: (Integral a) => (a -> Bool) -> [a] -> [a] --Gives Prime numbers upto N values
firstNPrime f [] = error "Empty List"
firstNPrime f (x:xs)
	| fun1 == True = x : firstNPrime f xs
	| otherwise = firstNPrime f xs
	where fun1 = isPrime any' x



-------------------------------------------------------------
--Question # 2 Haskell Function for Selection Sorting
-------------------------------------------------------------
maximum' :: (Ord a) => [a] -> a        --Finding Maximuim Value From List
maximum' [] = error "Empty List"
maximum' [x] = x
maximum' (x:xs)
	| x > mt = x
	| otherwise = mt
	where mt = maximum' xs             -- mt for maximum value in tail
	

minimum' :: (Ord a) => [a] -> a        --Finding Minimum Value From List
minimum' [] = error "Empty List"
minimum' [x] = x
minimum' (x:xs)
	| x < mt = x
	| otherwise = mt
	where mt = minimum' xs             -- mt for minimum value in tail
	

delete' :: (Eq a) => [a] -> a -> [a]        -- Deleting 1st Element of List
delete' [] y = error "Empty List"
delete' (x:xs) y 
	| x == y = xs
	| otherwise = x : delete' xs y

selectionSort :: (Eq a) => ([a] -> a) -> [a] -> [a]   --Performing Selection Sort
selectionSort f [] = []
selectionSort f [x] = [x]
selectionSort f (x:xs) 
	| x == max = x : selectionSort f xs
	| otherwise = max : selectionSort f sub
	where max = f (x:xs)          -- takes maximum value from list with higher order fucntion
	      sub = delete' (x:xs) max -- use to implement delete to delete value