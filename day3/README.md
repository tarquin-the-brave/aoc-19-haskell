# day3

initial attempt worked but was very slow:

```bash
$ time stack run
1983

real    8m17.412s
user    8m29.678s
sys     0m17.977s

```

This is because of the approach of comparing every point on one wire with
every point on the other to see where they meet.

```haskell
wireCrossings w1 w2 = filter (\(x, y) -> cross x y) ((\x y-> (x, y)) <$> w1 <*> w2)
```

For wires with `n`, and `m` points in them respectively, we get `n * m` possible
crossings to analyse.

For my inputs:

```bash
$ wc -l input.txt
2 input.txt
$ cat input.txt | tr ',' ' ' | wc -w
602
$ cat input.txt | head -1 | tr ',' ' ' | wc -w
301
$ cat input.txt | tail -1 | tr ',' ' ' | wc -w
301
$ echo $(( 301 * 301 ))
90601
$
```

, that's a lot.

## Revisiting This

The order of the points in a wire is not needed.  We can use a HashMap.

```
$ time !!
time stack run
Stack has not been tested with GHC versions above 8.6, and using 8.8.3, this may fail
Stack has not been tested with Cabal versions above 2.4, but version 3.0.1.0 was found, this may fail
1983

real    0m0.815s
user    0m2.643s
sys     0m1.368s
$
```
