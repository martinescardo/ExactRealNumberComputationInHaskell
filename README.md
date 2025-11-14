# Exact real number computation in Haskell

By Martín Escardó, School of Computer Science, University of Birmingham, UK, 1998-2011.

Formatted to markdown 14th November 2025, with minor editions, from [the original](https://martinescardo.github.io/papers/fun2011.lhs) literate Haskell File for **Fun in the Afternoon 2011** held at the School of Computer Science, University of Birmingham, UK.

The file [fun2011.hs](fun2011.hs), which contains the Haskell code below without comments, should not be edited directly. Instead edit [this](README.md) file and run
```text
 $ runhaskell mdtohs.hs < README.md > fun2011.hs
```
to update it.

## About

Exact computation with real numbers represented as infinite sequences of signed binary digits. Some references are given as clickable links.

## Running the examples

There are several examples to choose from (search the file for the
keyword `example`):

```haskell
{-# LANGUAGE NPlusKPatterns #-}
{-# OPTIONS_GHC -Wno-x-partial #-}

import System.IO

main = do
        hSetBuffering stdout NoBuffering
        putStr ((show (take 500 example4)) ++ "\n")
```
You may wish to compile the file with
```text
 $ ghc --make -O2 fun2011.hs
```
and then run
```text
 $ ./fun2011
```

## Decimal notation is problematic

[Brouwer](https://en.wikipedia.org/wiki/L._E._J._Brouwer) (1920's) observed that decimal notation is not suitable for
computation with infinitely many digits.

Consider
```text
   3 × 0.33333⋯,
```
where we don't know yet what the next digit is.  What should be the
first digit of the result?

If we eventually see a digit `>3`, the output must be
```text
   1.0⋯
```
If we eventually see a digit `<3`, the output must be
```text
   0.9⋯
```
Hence while we read a digit `=3`, we can't figure out the first digit of
the result.  We need a crystal ball to compute the first digit.

[Continuity is violated](https://www.sciencedirect.com/science/article/pii/S1571066104051357).  Continuity says that finitely many digits of output
depend only on finitely many digits of input.  To
regain continuity, alternative representations of real numbers are
used (see e.g. Plume's (1997) [4th-year project report](https://www.dcs.ed.ac.uk/home/mhe/plume/)).

## Our representation of the unit interval

We work with the signed unit interval `I=[-1,1]` of numbers `-1 ≤ x ≤ 1`, using binary notation
with signed digits.

Alphabet of digits `-1,0,1`:
```haskell
type Digit = Int
```
Representation of the space `I=[-1,1]`:
```haskell
type I  = [Digit]
```
An infinite sequence `ds = [d₀, d₁, ⋯, dₙ, ⋯]` represents the number
```text
    x = d₀ / 2 + d₁ / 4 + d₂ / 8 + ⋯ + dₙ / 2ⁿ⁺¹ + ⋯
```
Soon we will actually use a wider variety of digits, but not in the
type `I`.

## Some constants

```haskell
one, zero, minusOne, half,  minusHalf :: I
minusOne  = repeat (-1)
minusHalf = -1 : repeat 0
zero      = repeat 0
half      = 1 : zero
one       = repeat 1
```

## Auxiliary representations of real numbers

Although the main representation we consider is the above, it is
convenient to use auxiliary intermediate representations to simplify
the algorithms.

We still use base `2`, but allow more digits, and then convert back to
the three digits `-1,0,1`.

Type of digits `-2,-1,0,1,2`:

```haskell
type Digit2  = Int
```
Type of digits `-4,-3,-2,-1,0,1,2,3,4`:
```haskell
type Digit4  = Int
```
More generally, type of digits `|d|<=n`.
```haskell
type Digitn = Int
```
Type of interval spaces `[-2,2]`, `[-4,4]`, and `[-n,n]`:
```haskell
type I2 = [Digit2]
type I4 = [Digit4]
type In = [Digitn]
```
Dependent types would be handy! Maybe the next version will be in
Agda. (Added 14th November 2025. [Todd Ambridge](https://www.birmingham.ac.uk/staff/profiles/computer-science/academic-staff/ambridge-todd-waugh) did this in his [PhD Thesis](https://research.birmingham.ac.uk/en/publications/exact-real-search-formalised-optimisation-and-regression-in-const/).)

The following converts back to our standard representation.  It
amounts to division by `n` as a function `[-n,n] → [-1,1]` for `n>=2`.
Dependent types again would be handy here. We use `n = 2` and `n = 4`.

The first case of the following definition can be omitted. It is an
optimization that makes a significant difference, by reducing the
lookahead complexity.
```haskell
divideBy :: Int -> In -> I
divideBy n (0:x) = 0 : divideBy n x -- Added 5 Feb 2015. Makes everything way faster, but is not needed.
divideBy n (a:x) | abs a == n
  = (if a < 0 then -1 else 1) : divideBy n x
divideBy n (a:b:x) =
  let d = 2 * a + b
  in if      d < -n then -1 : divideBy n (d+2*n:x)
     else if d >  n then  1 : divideBy n (d-2*n:x)
                    else  0 : divideBy n (d:x)

divideBy2 :: I2 -> I -- Added 5 Feb 2015 again to make things faster.
divideBy2 ( 0:x) =  0 : divideBy2 x
divideBy2 ( 2:x) =  1 : divideBy2 x
divideBy2 (-2:x) = -1 : divideBy2 x
divideBy2 (a:b:x) =
  let d = 2*a+b
  in if      d < -2 then -1 : divideBy2 (d+4:x)
     else if d >  2 then  1 : divideBy2 (d-4:x)
                    else  0 : divideBy2 (d:x)
```

## Some very basic operations on `I=[-1,1]`

Addition as a function `[-1,1] × [-1,1] → [-2,2]`. This is our first
example where we use an auxiliary representation:

```haskell
add2 :: I -> I -> I2
add2 = zipWith (+)
```
Midpoint `(x+y)/2` as a function `[-1,1] × [-1,1] → [-1,1]`:
```haskell
mid :: I -> I -> I
mid x y = divideBy2 (add2 x y)
```
Proceeding in this way, we have avoided a myriad of cases in the
definition of addition and mid points that one often sees in the
literature, while still being time and space efficient.

(There are in fact `3⁴` cases, because we need to look at two digits of
each argument to produce one output digit.)

Complement `x ↦ -x` as a function `[-1,1] → [-1,1]`:
```haskell
compl :: I -> I
compl = map (\d -> -d)
```
Division by a positive integer:
```haskell
divByInt :: I -> Int -> I
divByInt x n = f x 0
  where
   f (a:x) s = let t = a + 2 * s
               in if t >=  n then  1 : f x (t - n)
             else if t <= -n then -1 : f x (t + n)
                             else  0 : f x t
```

## Infinitary operations

Given a sequence `x₀, x₁, x₂, ⋯, xₙ, ⋯` of points of `[-1,1]`,
compute
```text
     Mᵢ xᵢ = x₀ / 2 + x1 / 4 + x2 / 8 + ⋯ +  xn / 2^(n+1) + ⋯
           = Σₙ xₙ / 2ⁿ⁻¹
           = mid(x₀, mid(x₁, mid(x₂, ⋯)))
           = bigMid x
```
This infinitary operation is proposed by Escardó and Simpson
(LICS'2001, [A universal characterization of the closed Euclidean
interval](https://martinescardo.github.io/papers/interval.pdf)).

The definition
```text
  (*) bigMid (x:xs) = mid x (bigMid xs)
```
doesn't work, because mid needs two digits of input from each argument
to produce one digit of output, and hence (*) cannot produce any
digit.

We use Scriven's algorithm (in his 2008 MSc thesis ([MFPS'2009](http://dx.doi.org/10.1016/j.entcs.2008.10.020)) supervised by me).
It uses the auxiliary representation I4 to compute
`bigMid' :: [I] -> [I4]`, and then converts back to `I` by division by `4`:
```haskell
bigMid :: [I] -> I
bigMid = divideBy 4 . bigMid'
 where bigMid'((a:b:x):(c:y):zs) = 2*a + b + c : bigMid'((mid x y):zs)
```
Although `bigMid` cannot be defined using (*), it does satisfy (*).

## Truncated operations

Some truncated operations are also useful. The truncation retraction
truncate: `ℝ → [-1,1]` is mathematically defined as:
```text
      truncate(x) = max(-1,min(x,1))

                     /
                     |    -1 if x <= -1,
                  = <      x if      -1 <= x <= 1,
                     |     1 if                 1 <= x.
                     \

```
Truncated `x+1` as a function `[-1,1] → [-1,1]`, that is, `x ↦ min(x+1,1)`:
```haskell
addOne :: I -> I
addOne ( 1 : x) = one
addOne ( 0 : x) = 1 : addOne x
addOne (-1 : x) = 1 : x
```
Truncated `x-1` as a function `[-1,1] → [-1,1]`, that is, `x ↦ max(-1,x-1)`:
```haskell
subOne :: I -> I
subOne ( 1 : x) = -1 : x
subOne ( 0 : x) = -1 : subOne x
subOne (-1 : x) = minusOne
```
Truncated `x ↦ 1-x` as a function `[-1,1] → [-1,1]`:
```haskell
oneMinus :: I -> I
oneMinus = addOne.compl
```
Truncated multiplication by 2 as a function `[-1,1] → [-1,1]`, that is,
`x ↦ max(-1,min(2x,1))`:
```haskell
mulBy2 :: I -> I
mulBy2 ( 1 : x)  = addOne x
mulBy2 ( 0 : x)  = x
mulBy2 (-1 : x)  = subOne x
```
Truncated multiplication by 4:
```haskell
mulBy4 :: I -> I
mulBy4 = mulBy2.mulBy2
```
Truncated multiplication of a number by non-negative integer:
```haskell
tMulByInt :: I -> Int -> I
tMulByInt x 0 = zero
tMulByInt x 1 = x
tMulByInt x n = if even n
                then mulBy2(tMulByInt x (n `div` 2))
                else tadd x (mulBy2(tMulByInt x (n `div` 2)))
```
Truncated addition as a function `[-1,1] × [-1,1] → [-1,1]`,
that is, `(x,y) ↦ max(-1,min(x+y,1))`:
```haskell
tadd :: I -> I -> I
tadd x y = mulBy2(mid x y)
```

## Summary so far

We have Haskell functions
```text
   one, zero, minusOne, half, minusHalf :: I
   divideBy :: Int -> In -> I
   add2 :: I -> I -> I2
   mid :: I -> I -> I
   bigMid :: [I] -> I
   compl :: I -> I
   divByInt :: I -> Int -> I
   addOne :: I -> I
   subOne :: I -> I
   oneMinus :: I -> I
   tadd :: I -> I -> I
   mulBy2 :: I -> I
   mulBy4 :: I -> I
   tMulByInt :: I -> Int -> I
```

## Computation of the constant π

Using [Bailey, Borwein & Plouffe 1997](https://www.davidhbailey.com/dhbpapers/digits.pdf), we get:
```text
  π/8 = bigMidₖ 8⁻ᵏ (1/(8k+1) - 1/2(8k+4) - 1/4(8k+5) - 1/4(8k+6)).
```
See also [Wikipedia](http://en.wikipedia.org/wiki/Bailey%E2%80%93Borwein%E2%80%93Plouffe_formula).
```haskell
piDividedBy32 :: I
piDividedBy32 =
 bigMid
  [f k (mid (mid (g1 k) (g2 k))(mid (g3 k) (g4 k))) | k <- [0..]]
 where f k x = if k == 0 then x else 0:0:0: f (k-1) x
       g1 k = divByInt (repeat  1)      (8*k+1)
       g2 k = divByInt (-1 : zero)      (8*k+4)
       g3 k = divByInt ( 0 : -1 : zero) (8*k+5)
       g4 k = divByInt ( 0 : -1 : zero) (8*k+6)

example1 = piDividedBy32
```
This is an unusual example of a situation of when things go rather
well regarding the trade-off elegance-versus-efficiency. Of course,
there are much better ways of computing π. But this is not the worst.

## Conversion from and to `Double`

We now want to print the results in human-readable form. The easiest
kind of conversion is to/from `Double`. This can be done without
round-off errors, but of course the conversion to `Double` will involve
a truncation error.
```haskell
toDouble :: I -> Double
toDouble = f 55
 where f 0 x = 0.0
       f k (-1 : x) = (-1.0 + f (k-1) x)/2.0
       f k ( 0 : x) = (       f (k-1) x)/2.0
       f k ( 1 : x) = ( 1.0 + f (k-1) x)/2.0

fromDouble :: Double -> I
fromDouble = f 55
 where f 0 x   = zero
       f k x   = if x  < 0.0 then -1 : f (k-1) (2.0 * x + 1)
                             else  1 : f (k-1) (2.0 * x - 1)

example2 = 32 * toDouble piDividedBy32
example3 = example2 - pi
```
The last example gives `0.0`, which confirms that Haskell correctly
computes π in `Double`. Or that our algorithms so far are not
completely wrong.

## Multiplication by an integer

We multiply a number in `[-1,1]` by positive integer, producing integer
and fractional parts. In particular we will be able to multiply by `10`,
and hence convert to decimal (with a caveat - see below).
```haskell
mulByInt :: I -> Int -> (Int,I)
mulByInt x n = f n
 where f 1 = (0, x)
       f n = let (a,u) = f (n `div` 2)
                 d:y = u
                 b = 2*a+d
             in if even n
                then (b,y)
                else let e:t = (mid x y)
                     in (b+e,t)
```

## Conversion to decimal

Cannot be done without crystal balls, for continuity reasons. However,
there is a conversion to decimal that diverges if the input is of the
form `m/10ⁿ` with `m` and `n` integers (`10`-adic numbers). This is the best
that can be done without violating continuity, and hence
computability, requirements.

We consider conversion using negative decimal digits first. The
alphabet of positive and negative (and zero) decimal digits is:
```haskell
type Decimal = [Int]
```
The conversion allowing negative decimal digits is defined by:
```haskell
signed2Decimal :: I -> Decimal
signed2Decimal x = let (d,y) = mulByInt x 10
                   in d : signed2Decimal y
```
We now get rid of negative decimal digits. Only works for positive,
non-10-adic numbers, as discussed above. We use a finite state machine
with stages `f` and `g`, using a weak positiveness test as an
auxiliary function:
```haskell
normalize :: Decimal -> Decimal
normalize x = f x
 where f(d:x) = if wpositive x
                then d:f x
                else (d-1): g x
       g(0:x) = 9: g(x)
       g(d:x) = if wpositive x
                then (10+d) : f x
                else (10+d-1) : g x
       wpositive (d:x) =
                if d > 0 then True
           else if d < 0 then False
                         else wpositive x
```
We now convert a positive, non 10-adic, number to decimal
notation using only non-negative digits:
```haskell
decimal :: I -> Decimal
decimal = normalize.signed2Decimal

decimalString :: I -> String
decimalString = concat . map show . decimal
```
The decimal expansion of π can be computed as follows:
```haskell
example4 = let (m,x) = mulByInt piDividedBy32 32
           in show m ++ "." ++ decimalString x
```

## Full multiplication algorithms

We start in an elegant way when the only requirement is correctness,
but quickly get ugly when we aim for efficiency.

We will consider several algorithms, starting from the elegant
ones. You have to choose the one you want:
```haskell
mul :: I -> I -> I
mul = mul_version2
```
All of them require multiplication of a signed digit by a number
in `[-1,1]`:
```haskell
digitMul :: Digit -> I -> I
digitMul (-1) x = compl x
digitMul   0  x = zero
digitMul   1  x = x
```
The easiest algorithm uses the `bigMid` function, and we could have
presented it much earlier. But we wanted to show that it is possible
to compute π without knowing how to perform full multiplication.
```haskell
mul_version0 :: I -> I -> I
mul_version0 x y = bigMid (zipWith digitMul x (repeat y))
```
I don't have the time, at the time of writing, to fully explain why this is not as efficient as it can be. But it is pleasant that we can get an easy, self-explanatory algorithm (which works in the same way as we do
computations by hand in the case of finitely many digits, thanks to the `bigMid` operation).
At this point I want to save time and refer you to the beautiful [work](http://www.dcs.ed.ac.uk/home/mhe/plume/index.html)
by David Plume at Edinburgh, supervised by Alex Simpson and myself in
1997: He was an undergrad student at that time!

Here is Plume's multiplication algorithm (at that time `mul_version0` was
not known as far as I am aware):
```haskell
mul_version1 :: I -> I -> I
mul_version1 (a0 : a1 : x) (b0 : b1 : y) = mid p q
 where p  = mid p' p''
       p' = (a0*b1): mid (digitMul b1 x) (digitMul a1 y)
       p''= mid (digitMul b0 x) (digitMul a0 y)
       q = (a0*b0) : (a1*b0) : (a1*b1) : mul_version1 x y
```
Soon after that (still last millenium), I came up with the following
way-faster algorithm, which arises by (1) adding particular cases
(first two equations) and (2) considering special cases of the above
algorithm and making mathematical simplifications:
```haskell
mul_version2 :: I -> I -> I
mul_version2 (0:x) y = 0 : mul_version2 x y
mul_version2 x (0:y) = 0 : mul_version2 x y
mul_version2 (a0 : 0 : x) (b0 : 0 : y) = mid p q
  where p  = 0 : mid (digitMul b0 x) (digitMul a0 y)
        q = (a0*b0) : 0 : 0 : mul_version2 x y
mul_version2 (a0 : 0 : x) (b0 : 1 : y) = mid p q
  where p  = mid p' p''
        p' = 0 : 0 : x
        p''= mid (digitMul b0 x) (digitMul a0 y)
        q = (a0*b0) : 0 : 0 : mul_version2 x y
mul_version2 (a0 : 0 : x) (b0 : b1 : y) = mid p q
  where p  = mid p' p''
        p' = (a0*b1): 0 : digitMul b1 x
        p''= mid (digitMul b0 x) (digitMul a0 y)
        q = (a0*b0) : 0 : 0 : mul_version2 x y
mul_version2 (a0 : a1 : x) (b0 : 0 : y) = mid p q
  where p  = mid p' p''
        p' = 0 : 0 : digitMul a1 y
        p''= mid (digitMul b0 x) (digitMul a0 y)
        q = (a0*b0) : (a1*b0) : 0 : mul_version2 x y
mul_version2 (a0 : a1 : x) (b0 : b1 : y) = mid p q
  where p  = mid p' p''
        p' = (a0*b1): mid (digitMul b1 x) (digitMul a1 y)
        p''= mid (digitMul b0 x) (digitMul a0 y)
        q = (a0*b0) : (a1*b0) : (a1*b1) : mul_version2 x y
```
This is not beautiful at this point, but it is the price you have to
pay to be fast. I don't think the last word has been said about
efficient multiplication in the infinite case (in practice, or in
theory).

The previous algorithm can be further simplified when we want to
compute `mul x x`, that is, squares, and this gives a further speed
up.
```haskell
sqr :: I -> I
sqr (0:x) = 0 : 0 : sqr x
sqr (a0 : 0 : x) = mid p q
 where p  = 0 : digitMul a0 x
       q = (a0*a0) : 0 : 0 : sqr x
sqr (a0 : a1 : x) = mid p q
 where p  = mid p' p''
       p' = (a0*a1): digitMul a1 x
       p''= digitMul a0 x
       q  = (a0*a0) : (a1*a0) : (a1*a1) : sqr x
```

## Wrong results very fast, and right results not so fast

A computation which is well-known to go wrong if care is not taken is
the orbit of the [logistic map](https://en.wikipedia.org/wiki/Logistic_map)
```text
   f(x) = ax(1-x).
```
The orbit is the sequence `x₀, f(x₀), f(f(x₀)), ⋯, fⁿ(x₀), ⋯` starting from a chosen `x₀`. A
particularly bad case takes place when `a=4`. In this case we have a
nice map `f:[0,1] → [0,1]` with a very nasty orbit.

The point is that the map looks innocent, but its orbit plays tricks.

This map was introduced in the 1800's as a model of population
evolution (let's say fish in a lake, where 1 is the maximum capacity
of the lake, and 0 is extinction). Of course, this is probably not a
very good model. The point is that, whether or not the model is good,
it gives us computational (and indeed mathematical) trouble.

Before writing Haskell code, let's see what happens in `C` with float and
doubles:
```text
  void main() {
    float const  a = 4.0;
    float const x0 = 0.671875;  float   xs = x0;
    float const  n = 60;        double  xd = x0;

    for (int i = 0; i <= n; i++) {
         xs = a * xs * (1.0 - xs);
         xd = a * xd * (1.0 - xd);

    printf("xs = %f     xd = %f \n", xs, xd);
```
We get:
```text
     xs = 0.934518     xd = 0.928604
```
Maybe we can be confident that the correct result rounded to 2 digits
would be $0.93$?

Let's see. Make the program print intermediate results:
```text
 void main() {
  float const  a = 4.0;
  float const x0 = 0.671875;  float   xs = x0;
  float const  n = 60;        double  xd = x0;

  for (int i = 0; i <= n; i++) {
    xs = a * xs * (1.0 - xs);
    xd = a * xd * (1.0 - xd);
    printf("%d \t %f \t %f \n", i, xs, xd);
```
We get:
```text
 i    xs            xd
-----------------------------
 0    0.671875      0.671875
 5    0.384327      0.384327
10    0.313034      0.313037
15    0.022702      0.022736
20    0.983813      0.982892
25    0.652837      0.757549 <------
30    0.934927      0.481445
35    0.848153      0.313159
40    0.057696      0.024009
45    0.991613      0.930892
50    0.042174      0.625693
55    0.108415      0.637033
60    0.934518      0.928604
```
We now use a different, equivalent formula:
```text
   f(x) = 4x(1-x) = 1-(2x-1)²

 n    exact       double 1    double 2
----------------------------------------
 0    0.671875    0.671875    0.671875
 5    0.384327    0.384327    0.384327
10    0.313037    0.313035    0.313033
15    0.022735    0.022720    0.022700
20    0.982892    0.983326    0.983864
25    0.757549    0.709991    0.646726 <--
30    0.481445    0.367818    0.997196
35    0.313159    0.824940    0.984603
40    0.024009    0.899100    0.553930
45    0.930881    0.632135    0.975145
50    0.625028    0.823770    0.880008
55    0.615752    0.926760    0.898787
60    0.315445    0.371371    0.648129
```
Let's briefly discuss how the exact entry was *not* computed.
Notice that if `a` and `x` are rational, then so is
```text
     f(x) = ax(1-x).
```
So we could in principle use rational arithmetic with arbitrary
precision for the numerators and denominators (as in Haskell).  But
the computation of $xₙ$ runs out of memory for `n > 30` or so (2Gb of
memory).  Numerators and denominators quickly grow to large,
relatively prime integers.

We move back to infinite precision arithmetic in Haskell.  We consider
a slow and a fast way of computing the logistic map (you can also
play with the several ways of defining multiplication, and I have):
```haskell
logistic, logistic' :: I -> I

logistic  x = mulBy4 (mul x (oneMinus x))
```
The following is more efficient (faster) when it is iterated:
```haskell
logistic' x = oneMinus (sqr(g x))       -- 1-(2x-1)^2
      where g ( 1 : x) = x              -- g(x)= max(-1,2x-1)
            g ( 0 : x) = subOne x
            g (-1 : x) = minusOne
```
Here is an experiment, where I work with the fast one:

Firstly, `x₀` has an exact binary representation:
```haskell
x0 = 0.671875
```
It is
```haskell
d0 :: I
d0 = [1,0,1,0,1,1] ++ zero

logistics = map toDouble (iterate logistic' d0)

dlogistic' :: Double -> Double
dlogistic' x = 1.0-(2.0 * x - 1.0)^2

logisticsDouble = iterate dlogistic' x0

logisticsError = zipWith (-) logistics logisticsDouble

example5 = logisticsError
```

## Power series

Consider an analytic function
```text
    f(x) = Σₙ aₙ xⁿ.
```
Now define
```text
    g(x) = 1/2 f(x/2).
```
We calculate
```text
    g(x) = 1/2 sumₙ aₙ xⁿ 1/2ⁿ
         = Σₙ aₙ xⁿ 1/2ⁿ⁻¹
         = Mₙ aₙ xₙ.
```
Hence we can compute `g` using `bigMid`. To compute the elementary
functions, we can apply the usual Taylor series from calculus.

## Exponential function `1/2 e^(x/2)`

```haskell
mexp :: I -> I
mexp x = bigMid (series one 1)
  where series y n = y : series (divByInt (mul x y) n) (n+1)
```

## Trigonometric function `1/2 sin(x/2)`

```haskell
msin :: I -> I
msin x = bigMid (series x 2)
 where x2 = compl(sqr x)
       series y n = zero : y : series(divByInt(mul x2 y)(n*(n+1)))(n+2)
```

## Trigonometric function `1/2 cos(x/2)`

```haskell
mcos :: I -> I
mcos x = bigMid (series one 1)
 where x2 = compl(sqr x)
       series y n = y : zero : series(divByInt(mul x2 y)(n*(n+1)))(n+2)
```

## Function `1/2 arctan(x/2)`

```haskell
marctan :: I -> I
marctan x = bigMid (series x 1)
 where x2 = compl(sqr x)
       series y n = zero : divByInt y n : series (mul x2 y) (n+2)
```

## Number π again

We use [K. Takano 1982](https://en.wikipedia.org/wiki/Approximations_of_%CF%80):
```text
   pi/4 = 12 arctan(1/49)
        + 32 arctan1/57)
        -  5 arctan(1/239)
        + 12 arctan(1/110443)
```
```haskell
piDividedBy4 :: I
piDividedBy4 = let inverse n = divByInt one n
                   arctan = mulBy2.marctan.mulBy2
                   y1 = tMulByInt (arctan (inverse 49)) 12
                   y2 = tMulByInt (arctan (inverse 57)) 32
                   y3 = compl(tMulByInt (arctan (inverse 239)) 5)
                   y4 = tMulByInt (arctan (inverse 110443)) 12
               in tadd (tadd y1 y2) (tadd y3 y4)

example13 = let (m,x) = mulByInt piDividedBy4 4
            in show m ++ "." ++ decimalString x
```

## Trigonometric function `1/2 arcsin(x/2)/2`

```haskell
marcsin :: I -> I
marcsin x = bigMid (series x 1)
 where x2 = sqr x
       series y n = zero : divByInt y n :
                    series (tMulByInt (divByInt (mul x2 y) (n+1)) n) (n+2)
```

## Logarithmic function `ln(1+x/2)/x`

When `x = 0` we get the limit, `namely 1/2`.
```haskell
mlni :: I -> I

mlni x = bigMid (series one 1)
 where x2 = compl x
       series y n = divByInt y n : series (mul x2 y) (n+1)
```


## Truncated logarithmic function `ln(1+x/2)`

```haskell
mln :: I -> I
mln x = mul x (mlni x)
```

## Division

The inverse function 1 / (2 - x) using power series:

```haskell
inv :: I -> I
inv x = bigMid (series one)
 where series y = y : series (mul x y)
```

## Affine maps

The expression `affine a b`, defined below, gives the unique affine
map
```text
    f(x)=px+q
```
with
```text
    f(-1)=a,
    f( 1)=b.
```
That is, with
```text
    p = (b-a)/2,
    q = (b+a)/2.
```
Notice that
```text
    f(0) = (a+b)/2
```
and more generally
```text
    f((x+y)/2) =(f(x) + f(y)) /2,
```
and even more generally
```text
    f(Mᵢ xᵢ) = Mᵢ f(xᵢ).
```
That is, f is a (big) midpoint homomorphism.
See [Escardó and Simpson (2001)](https://martinescardo.github.io/papers/interval.pdf).
```haskell
affine :: I -> I -> I -> I
affine a b x = bigMid (map h x)
  where h (-1) = a
        h   0  = mid a b
        h   1  = b
```
Notice some particular cases. Multiplication is given by
```haskell
mul_version3 :: I -> I -> I
mul_version3 y = affine (compl y) y
```
Complement is given by
```haskell
complAffine :: I -> I
complAffine = affine one minusOne
```
The identity function is given by
```haskell
idAffine :: I -> I
idAffine = affine minusOne one
```

## Quantification over the unit interval

This is essentially Berger's 1990 algorithm in his PhD thesis. (See also [Seemingly impossible functional programs](https://math.andrej.com/2007/09/28/seemingly-impossible-functional-programs/) and/or page 15, Remark 4.8 of [this paper](https://lmcs.episciences.org/693/pdf).) We exploit that every
number in `I=[-1,1]` can be represented using digits `-1` and `1`
only. Hence we only check such sequences of digits.
```haskell
findI :: (I -> Bool) -> I
findI p = if p left then left else right
 where left  = -1 : findI(\x -> p(-1:x))
       right =  1 : findI(\x -> p( 1:x))

forEveryI, forSomeI :: (I -> Bool) -> Bool
forSomeI p = p(findI p)
forEveryI p = not(forSomeI(not.p))
```
Several applications are given in the following sections.

But we continue a bit in this section, because one application towards
the end of this file (program verification), requires checking all
sequences, not just the ones singled out above.
```haskell
findI' :: (I -> Bool) -> I

findI' p | p(left)   = left
         | p(centre) = centre
         | otherwise = right
 where left   = -1 : findI'(\x -> p(-1:x))
       centre =  0 : findI'(\x -> p( 0:x))
       right  =  1 : findI'(\x -> p( 1:x))

forEveryI', forSomeI' :: (I -> Bool) -> Bool
forSomeI' p = p(findI' p)
forEveryI' p = not(forSomeI'(not.p))
```

## Modulus of continuity

Now the modulus of continuity. How many correct digits of the input
are needed to correctly compute the output with a given precision?
The first algorithm for this is due to Berger (1990), and the following is a different version using the above function `forEveryI`:
```haskell
modulus :: (I -> I) -> (Int -> Int)
modulus f 0 = 0
modulus f n = if forEveryI(\x -> head(f x) == head (f zero))
              then modulus (tail.f) (n-1)
              else 1 + max (modulus (f.((-1):)) n) (modulus (f.(1:)) n)
```
This is very close to the lookahead complexity of the function (also
known as its intensional modulus of continuity (see e.g. [Simpson
(1998)](https://www.research.ed.ac.uk/files/12417914/download_2.pdf)).

How many digits of `x` are needed to get two correct digits of `x²`?
```haskell
example11 = modulus sqr 2
```
The answer is `5`.

## Supremum

We use [Simpson's (1998) algorithm](https://www.research.ed.ac.uk/files/12417914/download_2.pdf).
```haskell
supremum :: (I -> I) -> I

supremum f =
 let h = head(f zero)
 in if forEveryI(\x -> head(f x) == h)
    then h : supremum(tail.f)
    else imax (supremum(f.((-1):))) (supremum(f.(1:)))
```
Similarly,
```haskell
infimum :: (I -> I) -> I
infimum f =
  let h = head(f zero)
 in if forEveryI(\x -> head(f x) == h)
    then h : infimum(tail.f)
    else imin (infimum(f.((-1):))) (infimum(f.(1:)))
```
But we need to define `imax` and `imin`. This has to be done so that their
lookahead complexities are 1. Otherwise the algorithms diverge. See the next section.
```haskell
example12 = supremum (\x -> mid (0:x) (sqr x))
```

## Maximum, minimum and absolute value

We can't define `max(x,y) = if x < y then y else x`, because the
less-than relation is undecidable (violation of continuity, need for
crystal balls).
```haskell
imin :: I -> I -> I
imin ( a : x) ( b : y) | a == b = a : imin x y
imin (-1 : x) ( 1 : y) = -1 : x
imin ( 1 : x) (-1 : y) = -1 : y
imin (-1 : x) ( 0 : y) = -1 : imin           x     (oneMinus y)
imin ( 0 : x) (-1 : y) = -1 : imin (oneMinus x)              y
imin ( 1 : x) ( 0 : y) =  0 : imin (addOne   x)              y
imin ( 0 : x) ( 1 : y) =  0 : imin           x       (addOne y)
```
The maximum function by "cheating"
```haskell
imax :: I -> I -> I
imax x y = compl (imin (compl x) (compl y))
```
The absolute-value function
```haskell
iabs :: I -> I
iabs x = imax (compl x) x
```

## Integration

We use [Alex Simpson's (1998) algorithm](https://www.research.ed.ac.uk/files/12417914/download_2.pdf), but changing his intermediate
representation to make it more efficient. We represent a number `x` in `[-1,1]` by a sequence a in
`[I]` such that `x = bigMid a`. What we need is any representation where
average can be computed with lookahead `1` (and that can be converted
back to our chosen representation).
```haskell
average :: [I] -> [I] -> [I]
average = zipWith mid
```
Compute integral of `f` from `-1` to `1` divided by `2`, first using the above
representation for the output:
```haskell
halfIntegral' :: (I -> I) -> [I]
halfIntegral' f =
 let h = head(f zero)
 in if forEveryI(\x -> head(f x) == h)
    then (repeat h) : halfIntegral'(tail.f)
    else average (halfIntegral'(f.((-1):))) (halfIntegral'(f.(1:)))
```
Now we go back to our chosen representation:
```haskell
halfIntegral :: (I -> I) -> I
halfIntegral f = bigMid (halfIntegral' f)
```
Let's integrate the absolute value function:
```haskell
example10 = halfIntegral iabs
```

## Zero normalization

The number zero has countably many distinct representations. The
following procedure "normalizes towards zero":

The sequence `y = znorm x` has the following properties:
```text
  1. y denotes the same number as x, and y = norm y.

  2. If x denotes zero, then y = repeat 0

  3. If x doesn't denote zero, then we can read off the
     the sign of the denoted number by looking at the
     the first non-zero digit of y.
```
We exploit the rewrite rules
```
  (-1)1 ↦ 0(-1)
  1(-1) ↦ 0( 1),
```
which preserve denotation:
```haskell
znorm :: I -> I
znorm (0:x)      = 0:znorm x
znorm (-1:  1:x) = 0:znorm (-1:x)
znorm ( 1: -1:x) = 0:znorm ( 1:x)
znorm x          = x
```
See also my 1998 paper [Effective and sequential definition by cases
on the reals via infinite signed-digit numerals](https://www.sciencedirect.com/science/article/pii/S1571066105802142).

The following gives `True` if `x < 0`, `False` if `x > 0`, and an infinite, unproductive computation if `x = 0`.
```haskell
negative :: I -> Bool
negative x = f(znorm x)
 where f( 0 : x) = f x
       f(-1 : x) = True
       f( 1 : x) = False
```
This is not used anywhere else:
```haskell
smaller :: I -> I -> Bool
smaller x y = negative(mid x (compl y))
```

## Roots

The following works only if `f` is strictly increasing, `f(-1) < f(1)`,
and the unique root of `f` is not dyadic (of the form `m/2ⁿ` with `m,n`
integer). In other case it usually diverges.
```haskell
bisection :: (I -> I) -> I
bisection f =
 if negative(f zero)
 then  1 : bisection(f.( 1:))
 else -1 : bisection(f.(-1:))
```
Divergence can be avoided using trisection (an old algorithm in
constructive mathematics with a tendency to be rediscovered again and
again).

Assuming either `x < 0` or `0 < y`, we tell which one holds, where `True` is
for `x` and `False` for `y`, and willl diverge if `x=y=0`. This is a particular
case of the trichotomy law `x < a` or `a < y`, where `a=0`.
```haskell
ztrichot :: I -> I -> Bool
ztrichot x y = f (znorm x) (znorm y)
 where f (0  : x) (0 : y) = f x y
       f (-1 : x)      y  = True
       f       x       y  = False
```
Assume that `f` is strictly increasing and that `f(-1) < f(1)`.
```haskell
trisection :: (I -> I) -> I
trisection f =
 let l = f minusHalf
     c = f zero
     r = f half
 in if (ztrichot c r)
    then 1 : trisection(f.(1:))       -- f(0) < 0,      so root in [0,1]
    else if (ztrichot l c)            -- 0 < f(1/2),    so root in [-1,  1/2]
         then  0: trisection(f.( 0:)) --    f(-1/2) < 0,so root in [-1/2,1/2]
         else -1: trisection(f.(-1:)) --    0 < f(0),   so root in [-1,0]
```
A bijection `I → I` is therefore invertible:
```haskell
inverse :: (I -> I) -> (I -> I)
inverse f y = trisection(\x -> mid (f x) (compl y))
```
The function `x ↦ x²` is not a bijection, but it is a bijection on `[0,1]`,
and the above can be applied. But don't use it for negative numbers,
unless you want to get unproductive loops.
```haskell
squareRoot :: I -> I
squareRoot = inverse sqr

example6 = toDouble(squareRoot half)
```
Perhaps counter-intuitively, the closer we get the root,
longer the algorithm takes to produce the next digit. A related example is this:
```haskell
example7 = trisection f
 where tiny n = if n == 0 then one else 0 : tiny(n-1)
       epsilon = tiny (10^3)
       f = affine (compl epsilon) epsilon
```
If we want a root of `f` in a given interval `[a,b]`, we find the unique
affine map `g` such that `g(-1)=a` and `g(1)=b`. We then solve `h(x)=0`, where
`h(x)=f(g(x))`, and the root of `f` is `g(x)`.
```haskell
trisectionInterval :: I -> I -> (I -> I) -> I
trisectionInterval a b f = g(trisection (f.g))
 where g = affine a b
```
The particular case where `a=0` and `b=1` is often useful, and in this case
`g(x) = 1 : x`.
```haskell
trisection01 :: (I -> I) -> I
trisection01 f = 1 : trisection(f.(1:))
```
In fact, this is what is used in the above recursive definition of
`trisection`!

## Definition by cases

As mentioned above, (in)equality of real numbers is not decible
(continuity fails, crystal balls would be needed). However this is not
as bad as it seems at a first sight.

The following is a partial function defined on pairs
```text
  (x,y) such that if x=0 then y=0,
```
that is, undefined on
```text
  (0,y) with y /= 0.
```
The classical mathematical definition is
```text
   zppif x y = if x < 0 then 0 else y.
```
A constructive definition is
```haskell
zppif :: I -> I -> I
zppif x y = c (znorm x) (znorm y)
  where c (0:x) (0:y) = 0 : c x y
        c (0:x)    y  = c x y
        c (-1:x)   _  = zero
        c ( 1:x)   y  = y
```
If `x=0` and `y≠0` is close to zero, the answer is a partial number close
to zero.

Although this function is partial, it can be used to define total
functions, e.g.
```text
    f(x) = zppif x x = if x < 0 then 0 else x.
```
Application: we can define
```text
  (ppif x < y then u else v) = u + if (x-y) < 0 then 0 else v-u.
```
Except that `+` and `-` are not available. Hence we need a little bit
more work.
```haskell
ppif :: I -> I -> I -> I -> I
ppif x y u v =
   mulBy4(mid (0:u) (zppif (mid x (compl y)) (mid (compl u) v)))
```
We have that
```text
  ppif x y u v = u      if x < y,
  ppif x y u v = v      if x > y,
  ppif x y w w = w.
```
Actually the last equation doesn't need the last arguments to be
equal: it is enough they represent the same number, and the result
will be (a third) representation of that number.

I called this the "pseudo-parallel conditional" in [a 1998 publication](https://www.sciencedirect.com/science/article/pii/S1571066105802142).

A particular case of interest is y = 0, for which we get a more
efficient definition:
```haskell
ppifz :: I -> I -> I -> I
ppifz x u v = mulBy4(mid (0:u) (zppif (0:x) (mid (compl u) v)))
```
These partial functions can be used to define total functions.
Examples: absolute value, maximum.
```haskell
pabs :: I -> I
pabs x = ppifz x (compl x) x

pmax :: I -> I -> I
pmax x y = ppif x y y x
```

## Finding bugs automatically

Time to do some automatic program verification. Some years ago I
manually introduced a bug in the multiplication algorithm, and now
truly I don't remember what it was. But we are going to be able to see
that there is indeed a bug, and compute the offending input.
```haskell
buggyMul (0:x) y = 0 : buggyMul x y
buggyMul x (0:y) = 0 : buggyMul x y
buggyMul (a0 : 0 : x) (b0 : 0 : y) = mid p q
  where p  = 0 : mid (digitMul b0 x) (digitMul a0 y)
        q = (a0*b0) : 0 : 0 : buggyMul x y
buggyMul (a0 : 0 : x) (b0 : 1 : y) = mid p q
  where p  = mid p' p''
        p' = 0 : 0 : x
        p''= mid (digitMul b0 x) (digitMul a0 y)
        q = (a0*b0) : 0 : 0 : buggyMul x y
buggyMul (a0 : 0 : x) (b0 : b1 : y) = mid p q
  where p  = mid p' p''
        p' = (a0*b1): 0 : digitMul b1 x
        p''= mid (digitMul b0 x) (digitMul a0 y)
        q = (a0*b0) : 0 : 0 : buggyMul x y
buggyMul (a0 : a1 : x) (b0 : 0 : y) = mid p q
  where p  = mid p' p''
        p' = 0 : 0 : digitMul a1 y
        p''= mid (digitMul b0 x) (digitMul a0 y)
        q = (a0*b0) : (a1*b0) : 0 : buggyMul x y
buggyMul (a0 : a1 : x) (b0 : b1 : y) = mid p q
  where p  = mid p' p''
        p' = (a0*b1): mid (digitMul b1 x) (digitMul a1 y)
        p''= mid (digitMul b0 x) (digitMul a0 y)
        q = (a0*b0) : (a1*b0) : (a1*a1) : buggyMul x y
```
How do we know it is wrong, and what is the mistake?

We first check when two numbers x and y are close up to precision n.
```haskell
close :: Int -> I -> I -> Bool
close n x y = closez n (mid x (compl y))
```
This uses closeness to zero:
```haskell
closez :: Int -> I -> Bool
closez n x = all (==0) (take n (znorm x))
```
To catch all bugs, we should use `forEveryI'`. But the non-primed
version is faster, and any bug caught by it will be a bug:
```haskell
example8 = forEveryI(\x -> forEveryI(\y -> close 4 (mul x y) (buggyMul x y)))
```
This evaluates to `False` (and to `True` for closeness smaller than `4`). We
now find a counter-example:
```haskell
example9 = (take 5 x, take 5 y)
 where x = findI(\x -> forSomeI(\y -> not(close 4 (mul x y) (buggyMul x y))))
       y = findI(\y -> not(close 4 (mul x y) (buggyMul x y)))
```
This gives `([-1,-1,-1,-1,-1],[-1,1,-1,-1,-1])` as an example for which
`buggyMul` has a bug (assuming `mul` is correct). These two examples run
in under a second each, compiling this file as
```text
  $ ghc -O2 --make fun2011.lhs
```
This is a kind of model-free [model checking](https://en.wikipedia.org/wiki/Model_checking).

## General real numbers

General real numbers can be represented by a mantissa in I and an
integer exponent:
```haskell
type R = (I,Integer)
```
Using the above development for `I`, it is now fairly easy to implement
operations on R. But laborious too, and hence I stop here.

## Further implementations of exact real number computation

Many people around the world have produced (better and faster)
implementations of real numbers, using a variety of real number
representations (and programming languages). They include, in random
order, Norbert Muller, Branimir Lambov, Andrej Bauer, Russel O'Connor,
Valery Menissier-Morain, Pietro di Gianantonio, Boehm & Cartwright,
Peter Potts, David Lester, Vuilimin, and lots of people whose names
don't come immediately to my mind as I write this sentence.
