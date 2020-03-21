# day6

## Notes before Starting

**Input data** is a bunch of pairs of what is orbited by what.

I need to find out what **bodies** (nodes) are the roots,i.e.
don't orbit something else.

I can then use these as a starter values to [unfold a forest][tree].

[tree]: https://hackage.haskell.org/package/containers-0.6.2.1/docs/Data-Tree.html#g:2

The function that builds out the trees basically needs to say:
```
Given the input data: what bodies orbit this one?
```

It'll be  `Forest` of `String` where the `String` is the name
of each body given in the data.

So the types in `unfoldForest` will be:
```haskell
unfoldForest :: (b -> (a, [b])) -> [b] -> Forest a
```

where `a` and `b` will be `String` for this usage.

## Findings

This problem was all about trees in the end.  Load a tree with
the data, then walk the tree, summing the depths of each node.

I used a forest just incase there was more than one root in the data.

I was hoing to use `unfoldForest` for loading the data and
[`foldTree`][fold] to get the answer.  I couldn't see how to do it
with `foldTree`.  I had a tree of `String` where the string data
was wholy unintersting, other then as a name for the planetary body,
ad I wanted to sum the depths of each node. The type signature of
`foldTree` appears to want a function that uses the node values:

```haskell
foldTree :: (a -> [b] -> b) -> Tree a -> b
```

[fold]: https://hackage.haskell.org/package/containers-0.6.2.1/docs/Data-Tree.html#g:3

I ended up spliting the `Tree` into its levels with `levels` and
then folding over that.

```haskell
levels :: Tree a -> [[a]]

sumDepthsTree :: Integral b => Tree a -> b
sumDepthsTree = snd . foldl foldFunc (0, 0) . levels

foldFunc :: Integral a => (a,a) -> [b] -> (a,a)
foldFunc (depth, acc) x = (depth + 1, acc + depth * genericLength x)
```

I used a tuple accumulator to record the depth (i.e.the index within
`levels mytree` and the accumulating sum of the depths.

If I can resolve the differences in type between `foldFunc` and the
type of the first argument of `foldTree` I might be able to use
`foldTree` here and not have to implement this so manually.
