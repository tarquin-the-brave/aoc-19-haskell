# day4

I tried to implement a "bail out fold" by wrapping the accumulator in a
`Maybe`and have the fold function set the accumulator to `Nothing` if a
condition is met, and pass through `Nothing` values. So if the accumulator
becomes `Nothing` we pass that value up and do no more calculations.

I could then use this to assess if a number obeys a rule by folding over
the digits with a given condition for adjacent digits and see if the fold
completes and returns a `Just`, i.e. the condition wasn't met, or returns
`Nothing` the condition was met.

I suffered again from the issue of chosing a starting value.

```haskell
survivalFold f x = foldl (generalFoldFunction f) (Just '0') . show $ x

generalFoldFunction _ Nothing _ = Nothing
generalFoldFunction f (Just x) y = if (f x y) then Nothing else Just y
```

I initially had `X` instead of `0` which was screwing up the comparison in
the first step of the fold for the rule where digits can't decrease, as
`'X' > '0-9'` when comparing `Char`s.  Changing it to `'0'` fixed it.  I
was concerned that having to set this initial value for the fold breaks the
abstraction of the function.  We've got a generic function that can work
for any number of rules. If the rules are being used together there must be
character that doesn't fit all conditions, otherwise there will never be a
number that can pass all the rules.  But we don't know what that character
is, or should be upfront...  OK this breaks the abstraction, I need to
bring this out of the rule generator... I've updated the code from the
above.


I was hoping the compiler would optimise out passing through `Nothing` and
bail as soon as the accumulator becomes `Nothing`, then this could be done
to fold over an infinite list... sadly not:

```bash
$ stack ghci
...
*Main Lib> foldl (generalFoldFunction (\x y-> x*y > 2500)) (Just 0) [1..100]
Nothing
*Main Lib> foldl (generalFoldFunction (\x y-> x*y > 2500)) (Just 0) [1..40]
Just 40
*Main Lib> foldl (generalFoldFunction (\x y-> x*y > 2500)) (Just 0) [1..]
^CInterrupted.
*Main Lib>
```
