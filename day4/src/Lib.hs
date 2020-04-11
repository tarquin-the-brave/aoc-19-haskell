module Lib
    ( ruleOne
    , ruleTwo
    , ruleThree
    , generalFoldFunction
    ) where

import Data.Maybe

-- Two rules:
--
-- 1. Two adjacent digits are the same (like 22 in 122345).
-- 2. Going from left to right, the digits never decrease.
--
ruleOne :: Integral a => Show a => a -> Bool
ruleOne = isNothing . survivalFold (==) '0'

ruleTwo :: Integral a => Show a => a -> Bool
ruleTwo = isJust . survivalFold (>) '0'

-- Given a condition `f :: Char -> Char -> Bool` we fold over
-- the digits of a number, comparing consecutive digits until the condition is met.
-- `Nothing` indicates the condition is met,
-- `Just` caries the digit for comparison with the next one.
survivalFold :: Integral a => Show a => (Char -> Char -> Bool) -> Char -> a -> Maybe Char
survivalFold f init x = foldl (generalFoldFunction f) (Just init) . show $ x

generalFoldFunction :: Eq a => (a -> a -> Bool) -> Maybe a -> a -> Maybe a
generalFoldFunction _ Nothing _ = Nothing
generalFoldFunction f (Just x) y = if (f x y) then Nothing else Just y

-- PART 2: adjacent digits can't be part of a larger group.
ruleThree :: Integral a => Show a => a -> Bool
ruleThree num = (length twosomes) > 0
  where
    twosomes = [x | x <- adjacents, length x == 2]
    adjacents = (\(x,y) -> x:y) . gatherAdjacents . show $ num

gatherAdjacents :: String -> (String, [String])
gatherAdjacents = foldl foldFun ("0", [])
  where
    foldFun acc x = if x == (head . fst $ acc) then
      (x:(fst acc), snd acc)
    else
      ([x], (fst acc):(snd acc))

