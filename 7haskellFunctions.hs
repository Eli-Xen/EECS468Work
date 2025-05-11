--basic Haskell functions 
--inputs: Ints, lists, basic types a 
--outputs: list manipulation, Int, list, etc. 
--creation date: 4/19/25 

--PART 1 
replicate'::Int -> a -> [a] --function that takes something of type 'a' and makes a list with 'Int' of those 'a' things 
replicate' n x=[x | _<-[1..n]] --takes Int n and value x of any type a and for each element 1-n, whatever it is _ append x n times 

perfects::Int -> [Int] --takes an int and returns list of int where the factors add to the int 
perfects n=[x | x<-[2..n], sum[y | y<-[1..x-1], x `mod` y==0] == x] --go through ints from 2-n and if the sum of all y factors of that x==x then its perfect 


find::Eq a=> a -> [(a,b)] -> [b] --takes comparable type a and list of tuples (a,b) and return list of values where key appears in tuple 
find key pairs=[b | (a,b)<-pairs, a==key] --find() takes key to find and pairs, for every (x,y) tuple in pairs and finds the ones where x==key, and then takes the value b from those selected tuples and puts it into a list 

positions::Eq a=> a -> [a] -> [Int] --takes comparable type a and a list of type a and returns a list of ints for index positions of match for a 
positions x items = find x (zip items [0..(length items - 1)]) -- takes x and list of items, then uses zip on the list and numbers 0-infinity to makes tuples of the value and its index key, then gives those tuples to find so find can match the thing were looking for and return the index key its at 

scalarproduct::[Int] -> [Int] -> Int --takes two lists of ints and outputs an int 
scalarproduct xs ys=sum[x*y | (x,y)<-zip xs ys] --in lists xs and ys that the function takes in, sum the results of the list comprehension result that multiplies the x and y in a tuple that zip function created from lists xs and ys 

--PART 2 
factorial::Integer -> Integer --supporting factorial function that takes an int and returns in the factorial of it 
factorial n=product[1..n] --does factorial by computing the product of all numbers 1-n 

dodb :: Integer -> Integer -> Integer -> Integer ---takes 3 ints and returns an int; returns # of ways to distribute distinguishable objects n into distinguishable boxes k with m objects per box 
dodb n k m = factorial n `div` (factorial m^k) --factorial of n divided by factorial of m to the power of k =(product[1..n] `div` (m*product[1..k])) 

iodb::Integer -> Integer -> Integer --takes int n objects and int k distinguishable boxes and finds how many ways to distribute n into k boxes 
iodb n k=factorial (n+k-1) `div` (factorial (k-1) *factorial n) --uses factorial to compute combination of n+k-1 and n below: 
--C(n+k-1, n)=(n+k-1)!/(n+k-1-n)!n!=(n+k-1)!/(k-1)!n!

doib::Integer -> Integer -> Integer --takes int n distinguishable objects into int k boxes and finds how many ways to distribute n objects into k boxes; number of ways to partition a set of n objects into k subsets 
doib 0 0=1 --base case, if both passed are 0,0 return 1 
doib n 0=0 --base case, if passed n,0 return 0 
doib 0 k=0 --base case, if passed 0,k return 0 
doib n k =k*doib(n-1) k + doib (n-1) (k-1) --recursivley adds n-1 and k-1 then multiplies by k 

ioib::Integer->Integer->Integer --takes int n objects into int k boxes, finds how many ways to distribute n objects into k boxes 
ioib n 1=1
ioib 0 k=1 
ioib n k =	if k<1 || n<0 then 0 
		else (ioib (n-k) k+ioib n (k-1)) 
