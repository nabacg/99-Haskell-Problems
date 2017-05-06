module NinetyNineTests () where

import NinetyNine
import Test.QuickCheck
import Control.Monad

prop_myLast xs = not (null xs) ==> myLast xs == last xs
prop_myButLast xs = length xs > 1 ==> (myButLast xs) == (head $ drop 1 (reverse xs))
prop_myButLast2 xs = length xs > 1 ==> (myButLast2 xs) == (head $ drop 1 (reverse xs))
prop_elementAt ::  Eq a => [a] -> Int -> Property
prop_elementAt xs i = (i > 0 && i <= length xs) ==> (elementAt xs i) == (xs !! (i - 1))
prop_elementAt2 xs i = (i > 0 && i <= length xs) ==> (elementAt2 xs i) == (xs !! (i - 1))

prop_myLength xs = myLength xs == length xs
prop_myReverse xs = myReverse (myReverse xs) == xs

prop_isPalindrome xs = isPalindrome xs == (xs == reverse xs)


instance Show a => Show (NestedList a) where
  show a = case a of
    Elem x -> show x
    List xs -> concatMap (\x -> show x) xs

instance Arbitrary a => Arbitrary (NestedList a) where
  arbitrary = sized arbList

-- thanks to smart people from SO http://stackoverflow.com/a/15959889
-- and quickcheck manual http://www.cse.chalmers.se/~rjmh/QuickCheck/manual_body.html#16
arbList 0 = do
    a <- arbitrary
    return $ Elem a
arbList n = do
  (Positive m) <- arbitrary
  let n' = n `div` (m + 1)
  f <- replicateM m (arbList n')
  return $ List f

prop_flatten xs = flatten xs == flatten' xs
prop_compress xs = compress xs == compress' xs
prop_pack xs = pack xs == pack'' xs
prop_encode xs = encode xs == encode' xs

instance Arbitrary a => Arbitrary (EncodeNode a) where
  arbitrary =  oneof [liftM Single arbitrary,
                       liftM2 Multiple arbitrary arbitrary]

prop_encodeModified xs = encodeModified xs == encodeModified' xs
prop_decodeModified xs = decodeModified xs == decodeModified' xs

prop_encodeDirect xs = encodeDirect xs == encodeModified' xs
prop_dupli xs = dupli xs == concat [[x,x] | x <- xs]
prop_repli xs n = (repli xs n) == ( xs >>= replicate n)

prop_dropEvery xs n = n >= 0 ==> (dropEvery xs n) == dropEvery' xs n

prop_split xs n = n >= 0 ==> (split xs n) == (flip splitAt) xs n

prop_rotate xs n =  n > 0 && n <= length xs ==> rotate xs n == rotate' xs n

prop_removeAt n xs = n > 0 && n <= length xs ==> removeAt n xs == (xs !! (n-1), [x | (i, x) <- zip [1..] xs, i /= n])

prop_insertAt x xs n  = n > 0 && n <= length xs ==> insertAt x xs n == take (n-1) xs ++ [x] ++ drop (n-1) xs

prop_range i j = i >=0 && i < j ==> range i j == [i..j]

main = do
   quickCheck (prop_myLast :: [Integer] -> Property)
   quickCheck (prop_myButLast :: [Integer] -> Property)
   quickCheck (prop_elementAt :: [Integer] -> Int -> Property)
   quickCheck (prop_elementAt2 :: [Integer] -> Int -> Property)
   quickCheck (prop_myLength :: [Integer] -> Bool)
   quickCheck (prop_myReverse :: [Integer] -> Bool)
   quickCheck (prop_isPalindrome :: [Char] -> Bool)
   quickCheck (prop_flatten :: (NestedList Integer)  -> Bool)
   quickCheck (prop_compress :: [Char] -> Bool)
   quickCheck (prop_pack :: [Int] -> Bool)
   quickCheck (prop_encode :: [Char] -> Bool)
   quickCheck (prop_encodeModified :: [Integer] -> Bool)
   quickCheck (prop_decodeModified :: [EncodeNode Int] -> Bool)
   quickCheck (prop_encodeDirect :: [EncodeNode Int] -> Bool)
   quickCheck (prop_dupli :: [Integer] -> Bool)
   quickCheck (prop_repli :: [Integer] -> Int -> Bool)
   quickCheck (prop_dropEvery :: [Char] -> Int -> Property)
   quickCheck (prop_split :: [Int] -> Int -> Property)
   quickCheck (prop_rotate :: [Int] -> Int -> Property)
   quickCheck (prop_removeAt :: Int -> [Char] -> Property)
   quickCheck (prop_insertAt :: Int -> [Int] -> Int -> Property)
   quickCheck (prop_range :: Int -> Int -> Property)
  -- runTests "1-9" options
  -- [run prop_myLast]
