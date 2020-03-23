# day8

Lots of lists and folds.

2 interesting things learned.

`zip` & `zipWith`
```haskell
zip :: [a] -> [b] -> [(a, b)]
zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
```

`undefined` can be used to represent values that aren't evaluated, even at
runtime! But if you've expressing something that never evaluates, that might
be a sign of something wrong.

I used `Data.Default` from the `data-default` package to express a value
I didn't care about.

```haskell
sndMinFst :: Bounded a => Integral a => Default b => [(a , b)] -> b
sndMinFst = snd . foldl (\acc x -> if fst x < fst acc then x else acc) (maxBound, def)
```
