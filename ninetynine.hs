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


data EncodeNode a = Multiple Int a | Single a

instance Show a => Show (EncodeNode a) where
  show x = case x of
    Multiple i n -> "Multiple '" ++ (show i) ++ "' '" ++ show n ++  "']"
    Single n -> "Single '" ++ show n ++ "'"

instance Eq a => Eq (EncodeNode a) where
  (==) (Single a) (Single b) = a == b
  (==) (Multiple i a) (Multiple j b) = i == j && a == b
  (==) _ _ = False

encodeModified :: Eq a => [a] -> [EncodeNode a]
encodeModified = foldr fn []
  where fn x [] = [(Single x)]
        fn x acc@((Single a):as) = if x == a
                                  then (Multiple 2 a) : as
                                  else (Single x) : acc
        fn x acc@((Multiple c a): as) = if x == a
                                       then (Multiple (c + 1) a) : as
                                       else (Single x) : acc

encodeModified' :: Eq a => [a] -> [EncodeNode a]
encodeModified' = map fn . encode
  where fn (1, x) = Single x
        fn (i, x) = Multiple i x



decodeModified :: Eq a => [EncodeNode a] -> [a]
decodeModified [] = []
decodeModified ((Single c):xs) = c : decodeModified xs
decodeModified ((Multiple i c):xs) = (map (const c) [1..i]) ++ decodeModified xs

decodeModified' :: Eq a => [EncodeNode a] -> [a]
decodeModified' = concatMap decodeFn
  where decodeFn (Single a) = [a]
        decodeFn (Multiple i a) = replicate i a

encodeDirect :: Eq a => [a] -> [EncodeNode a]
encodeDirect =  foldr fn []
  where fn x [] = [(Single x)]
        fn x acc@((Single a):as) = if x == a
                                  then (Multiple 2 a) : as
                                  else (Single x) : acc
        fn x acc@((Multiple c a): as) = if x == a
                                       then (Multiple (c + 1) a) : as
                                       else (Single x) : acc


dupli :: [a] -> [a]
dupli = concatMap (replicate 2)

repli :: [a] -> Int -> [a]
repli xs n = concatMap (replicate n) xs

dropEvery :: [a] -> Int -> [a]
dropEvery [] _ = []
dropEvery xs 0 = xs
dropEvery xs n | length xs < n = xs
dropEvery xs n = init a ++ dropEvery b n
  where (a, b) = splitAt n xs

dropEvery' :: [a] -> Int -> [a]
dropEvery' xs n | n > 0 = map fst $ filter ((n/=) . snd) (zip xs (cycle [1..n]))
dropEvery' xs n = xs

split :: [a] -> Int -> ([a],[a])
split xs n = (map fst (takeWhile ((n >=) . snd) indexed),
              map fst (dropWhile ((n >=) . snd) indexed))
  where indexed = zip xs [1..]

(|>) :: a -> (a -> t) -> t
(|>) x f = f x

slice :: [a] -> Int -> Int -> [a]
slice xs a b = zip xs (cycle [1..])
               |> filter (\ (x, i) -> i >= a && i <= b)
               |> map fst


rotate :: [a] -> Int -> [a]
rotate xs n | n >= 0 = b ++ a
  where (a, b) = splitAt n xs
rotate xs n = reverse $ rotate (reverse xs) (-n)

rotate' :: [a] -> Int -> [a]
rotate' xs n = take (length xs) $ drop (length xs + n) $ cycle xs

removeAt ::  Int -> [a] -> (a,[a])
removeAt n xs = (xs !! (n-1), (take (n - 1) xs) ++ (drop n xs))
