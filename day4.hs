#!/usr/bin/env stack
-- stack --resolver lts-15.4 script

import Data.Maybe (isJust, isNothing)
import Control.Monad (foldM_)
import Data.Foldable (foldl')

main :: IO ()
main = do
  let ruleOneTwoList = [x | x <-[234208..765869], ruleOne x, ruleTwo x]
  let ruleOneTwoThreeList = [x | x <- ruleOneTwoList, ruleThree x]

  print "Numbers that never decrease and have two adjacent matching digits:"
  print . length $ ruleOneTwoList

  print "Rule out the numbers where the adjacent matching digits are part of larger group match:"
  print . length $ ruleOneTwoThreeList

-- Three rules:
--
-- 1. Two adjacent digits are the same (like 22 in 122345).
-- 2. Going from left to right, the digits never decrease.
-- 3. 2 Matching adjacent digits are not part of a larger group of matching adjacent digits.
--
ruleOne :: Integral a => Show a => a -> Bool
ruleOne = isNothing . foldM_ (\acc x -> if acc == x then Nothing else return x) '0' . show

ruleTwo :: Integral a => Show a => a -> Bool
ruleTwo = isJust . foldM_ (\acc x -> if acc > x then Nothing else return x) '0' . show

ruleThree :: Integral a => Show a => a -> Bool
ruleThree num = length twosomes > 0
  where
    twosomes = [x | x <- substringsOfMatchingAdjacentChars, length x == 2]
    substringsOfMatchingAdjacentChars = (\(x,y) -> x:y) . foldl' ff ("0", []) . show $ num

    ff (currentSubstring, substrings) c = if c == head currentSubstring then
        (c:currentSubstring, substrings)
      else
        ([c], currentSubstring:substrings)

