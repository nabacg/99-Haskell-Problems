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

main = do
   quickCheck (prop_myLast :: [Integer] -> Property)
   quickCheck (prop_myButLast :: [Integer] -> Property)
   quickCheck (prop_elementAt :: [Integer] -> Int -> Property)
   quickCheck (prop_elementAt2 :: [Integer] -> Int -> Property)
   quickCheck (prop_myLength :: [Integer] -> Bool)
   quickCheck (prop_myReverse :: [Integer] -> Bool)
   quickCheck (prop_isPalindrome :: [Char] -> Bool)
   quickCheck (prop_flatten :: (NestedList Integer)  -> Bool)
  -- runTests "1-9" options
  -- [run prop_myLast]