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
