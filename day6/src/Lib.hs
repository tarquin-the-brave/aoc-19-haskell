module Lib
    ( roots
    , forestBuilder
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- Which bodies do not orbit anything?
roots :: Eq a => [(a,a)] -> [a]
roots orbits = [x | (x, y) <- orbits, notElem x . fmap snd $ orbits]

-- Given the orbit data, what orbits a given body?
forestBuilder :: Eq a => [(a,a)] -> a -> (a,[a])
forestBuilder orbits body = (body, [snd orbit | orbit <- orbits, body == fst orbit])

