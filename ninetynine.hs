module NinetyNine where

myLast :: [a] -> a
myLast [] = error "No end for empty list!"
myLast [x] = x
myLast (x : xs) = myLast xs

myButLast :: [a] -> a
myButLast [] = error "No butlast for empty list!"
myButLast [x] = error "No butlast for single element list!"
myButLast (x : _ : []) = x
myButLast (_ : xs) = myButLast xs

myButLast2 = last . init


elementAt :: [a] -> Int -> a
elementAt (x:xs) 1 = x
elementAt [] i = error "List too short"
elementAt (x:xs) i = elementAt xs (i-1)

elementAt2 :: [a] -> Int -> a
elementAt2 xs i = (head . drop (i-1)) xs

myLength :: [a] -> Int
myLength = foldl (\ acc x -> acc + 1) 0

myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = xs == reverse xs

data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List xs) = foldl (\ acc x -> acc ++ (flatten x)) [] xs

flatten' :: NestedList a -> [a]
flatten' (Elem x) = [x]
flatten' (List xs) = concatMap flatten' xs

compress :: Eq a => [a] -> [a]
compress [] = []
compress [x] = [x]
compress (x:y:xs) = if x == y
                  then compressed
                  else x :compressed
  where compressed = compress(y:xs)

compress' :: Eq a => [a] -> [a]
compress' xs = foldr (\x acc-> if not (null acc) && x == head acc
                                then acc
                                else x : acc) [] xs

pack :: Eq a => [a] -> [[a]]
pack xs = foldr (\x acc -> if not (null acc) && x == head (head acc)
                           then (x :  (head acc)) : (tail acc)
                           else [x] : acc) [] xs

pack' :: Eq a => [a] -> [[a]]
pack' = foldr f []
  where f x [] = [[x]]
        f x (a:acc) = if x == head a
                      then (x : a) : acc
                      else [x] : a : acc

pack'' :: Eq a => [a] -> [[a]]
pack'' [] = []
pack'' (x:xs) = (x : takeWhile (==x) xs ) : pack''(dropWhile (==x) xs)

encode :: Eq a => [a] -> [(Int, a)]
encode xs = map (\x -> (length x, head x)) (pack' xs)

encode' :: Eq a => [a] -> [(Int, a)]
encode' [] = []
encode' (x:xs) = (length $ x : (takeWhile (==x) xs), x) : encode'(dropWhile (==x) xs)
