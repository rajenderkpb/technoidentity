--Head function override
head' ::[a] -> a  
head' [] = error "list is empty"  
head' (x:_) = x 

--Tail function override
tail'::[a]->[a]
tail' []=error "list is empty"
tail' (x:xs)=xs

--last function override
last'::[a]->a
last' []=error "list is empty"
last' [a]=a
last' (x:xs)=last' xs

--init function overriden
init'::[a]->[a]
init' []=error "list is empty"
init' [a]=[]
init' (x:xs)=x:init' xs

--take function overriden
take'::Int->[a]->[a]
take' _ []= []  
take' n (x:xs)
    | n <= 0 = []
    | otherwise = x:take' (n-1) xs 


--drop function overriden
drop'::Int->[a]->[a]
drop' _ []=[]
drop' n xs
    | n <= 0 = xs
    | otherwise = drop' (n-1) (tail' xs)	

--elem function overriden
iselem::(Eq a)=>a->[a]->Bool
iselem _ []=False
iselem a (x:xs)
    | a==x = True
    | otherwise = iselem a xs
  
--signum function overriden
signum'::(Num a,Ord a)=>a->a
signum' a
    | a > 0 = 1
    | a < 0 = -1
    | otherwise = 0


--abs function overriden
abs'::(Num a,Ord a)=>a->a
abs' a
    | a >= 0 = a
    | otherwise = -a



