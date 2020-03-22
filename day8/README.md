# day8

Lots of lists and folds.

2 interesting things learned.

`zip` & `zipWith`
```haskell
zip :: [a] -> [b] -> [(a, b)]
zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
```

`undefined` can be used to represent values that aren't evaluated, even at
runtime! So I was able to use it in the fold when I didn't care about or ever
evaluate part of the initial value. I've used it when testing type declarations
by binding a function to `undefined`.

This function build and could be called at runtime.

```haskell
sndMinFst :: Bounded a => Integral a => [(a , b)] -> b
sndMinFst = snd . foldl (\acc x -> if fst x < fst acc then x else acc) (maxBound, undefined)
```
