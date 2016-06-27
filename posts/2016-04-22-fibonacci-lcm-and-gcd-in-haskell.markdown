---
title: Fibonacci, LCM and GCD in Haskell
jumbotron_image: /images/2016-04-22-fibonacci-lcm-and-gcd-in-haskell/jumbotron_image.jpg
preview_image: /images/2016-04-22-fibonacci-lcm-and-gcd-in-haskell/preview_image.jpg
author: David Lettier
---
<!--https://pixabay.com/en/abstract-fractal-formula-math-1022198/-->

The following three problems: the Fibonacci sequence, Least Common Multiple, and the Greatest Common Divisor are potential problems one
may be asked to solve during a technical interview.
All of the main headers link to a larger collection of interview questions collected over the years.

## [Fibonacci](https://github.com/lettier/interviewquestions/blob/master/recursion/fibonacci.hs)

### Overview

Using multiple recursion (vs single recursion) we output the i<sup>th</sup> number of the sequence:

```javascript
   Index: 0 1 2 3 4 5 6  7  8 ...
Sequence: 0 1 1 2 3 5 8 13 21 ...
```

which has the pattern `0 + 1 = 1`, `1 + 1 = 2`, `1 + 2 = 3`, and so on and so forth...

```haskell
 1) {-
 2)   (C) David Lettier 2016
 3)   http://www.lettier.com/
 4) -}
 5)
 6) import System.Environment (getArgs)
 7)
 8) fib :: Int -> Int
 9) fib x | x <= 0 = 0
10) fib 1 = 1
11) fib x = (+) (fib y) (fib z)
12)   where y = x - 1
13)         z = x - 2
14)
15) main :: IO ()
16) main = do
17)   args <- getArgs
18)   let args' = map (\ x -> read x :: Int) args
19)   let x = case args' of
20)         [] -> Nothing
21)         (x:y) -> Just x
22)   let result = case x of
23)         Nothing -> "Error."
24)         Just x -> show $ fib x
25)   putStrLn result
26)   return ()
```

### Input

Line 17 gets the command line arguments we need for our input into `fib`.
Before we can just pass it off however, we need to consider a few cases.
If empty then we having `Nothing` otherwise we have `[something,...]` and we take `Just x`.
Now if we have `Nothing` then we will output `"Error."` otherwise we will pass the integer to
`fib`.

### Fib

Defined on lines eight through 13, `fib` starts out with pattern matching.
If `x` is negative just output `0`. However if `x` is `1` then just return `1`.
Line 11 is the multiple recursive call which falls inline with the pattern that our
current number is the sum of the previous two numbers in the sequence.
Notice that `y` is `x - 1` while `z` is `x - 2`.

## [LCM](https://github.com/lettier/interviewquestions/blob/master/number_theory/lcm.hs)

### Overview

The LCM is the smallest product found in all of the products of each number under review.
This solution only works with two integers but you could expand it to work with more.

Say our numbers are two, three, and four:

```javascript
   1 2 3 4 5 6 7 8 9 10 11 12 13 ...
2:   2   4   6   8   10    12    ...
3:     3     6     9       12    ...
4:       4       8         12    ...
                            ^
```

then the LCM would be `12`.

```haskell
 1) {-
 2)   (C) David Lettier 2016
 3)   http://www.lettier.com/
 4) -}
 5)
 6) import System.Environment (getArgs)
 7)
 8) lcm' :: Int -> Int -> Maybe Int
 9) lcm' 0 0 = Just 0
10) lcm' 0 _ = Nothing
11) lcm' _ 0 = Nothing
12) lcm' x y = Just $ head $ filter (\ z -> (mod z b) == 0) multiples_a
13)   where multiples_a = multiples a
14)         a = abs x
15)         b = abs y
16)
17) multiples :: Int -> [Int]
18) multiples x = map (* x) [1..]
19)
20) main :: IO ()
21) main = do
22)   args <- getArgs
23)   let args' = map (\ x -> read x :: Int) args
24)   let [x, y] = case args' of
25)         []      -> [0, 1]
26)         [x]     -> [0, x]
27)         (x:y:z) -> [x, y]
28)   let result = case lcm' x y of
29)         Nothing -> "Error."
30)         Just x -> show x
31)   putStrLn result
32)   return ()
```

### Input

Our input procedure is largely the same as the Fibonacci one however we need an input of two integers on the command line.
If we do not get two integers, we will fill the input with `0`s.
Depending of the other input, `lcm'` will catch this and may return `Nothing`.

### LCM'

Defined on line 9, `lcm'` makes an exception for `0 0` such that `0` is the LCM for both zero and zero.
Otherwise if one input is zero and the other is not we return `Nothing`.
If we allowed zero to be a possible common multiple then zero
would be the LCM for any combination of integers such that zero is a multiple of every number.

The last case does not deal with zero and proceeds to return the `head` of the common multiples between `x` and `y`.
`multiples` multiplies `x` with every number in the range `[1,inf)`.
Note that the program does not actually attempt to multiply a infinite number of integers due to
[lazy evaluation](https://en.wikipedia.org/wiki/Lazy_evaluation).
On line 12 these multiples are filtered by the multiples that are evenly divided by `y`.

```haskell
Prelude> let multiples x = map (* x) [1..]
Prelude> let common_multiples x y = filter (\ z -> (mod z y) == 0) (multiples x)
Prelude> take 10 $ common_multiples  6 12
[12,24,36,48,60,72,84,96,108,120]
Prelude> take 10 $ common_multiples 5 13
[65,130,195,260,325,390,455,520,585,650]
Prelude> take 10 $ common_multiples 12 36
[36,72,108,144,180,216,252,288,324,360]
```

Since we started at `1`, the least common multiple is at the `head` of the list/array of common multiples.

## [GCD](https://github.com/lettier/interviewquestions/blob/master/number_theory/gcd.hs)

### Overview

<i>Somewhat</i> similar to the spirit of LCM, the GCD finds the largest number that evenly divides all of the numbers being considered.
Looking at two, three, and four again:

```javascript
   1 2 3 4 ...
2: 1 2     ...
3: 1   3   ...
4: 1 2   4 ...
   ^
```

one is the GCD.

```haskell
 1) {-
 2)   (C) David Lettier 2016
 3)   http://www.lettier.com/
 4) -}
 5)
 6) import System.Environment (getArgs)
 7)
 8) gcd' :: Int -> Int -> Int
 9) gcd' 0 y = abs y
10) gcd' x 0 = abs x
11) gcd' x y = case divisors a b of
12)       [] -> -1
13)       (x:y) -> x
14)   where a = abs x
15)         b = abs y
16)
17) divisors :: Int -> Int -> [Int]
18) divisors x y = filter (\ z -> ((mod x z) + (mod y z)) == 0)  [a, b..1]
19)   where a = min x y
20)         b = if (a - 1) < 0 then 0 else (a - 1)
21)
22) -- Euclid's GCD Algorithm
23)
24) gcd'' :: Int -> Int -> Int
25) gcd'' x 0 = abs x
26) gcd'' x y = gcd'' b (mod a b)
27)   where a = abs x
28)         b = abs y
29)
30) main :: IO ()
31) main = do
32)   args <- getArgs
33)   let args' = map (\ x -> read x :: Int) args
34)   let [x, y] = case args' of
35)         [] -> [0, 0]
36)         [x] -> [0, x]
37)         (x:y:z) -> [x, y]
38)   let result = assemble_output (gcd' x y) (gcd'' x y)
39)   putStrLn result
40)   return ()
41)
42) assemble_output :: Int -> Int -> String
43) assemble_output x y = (show x) ++ " " ++ (show y)
```

### Input

As with the LCM, the GCD input procedure pattern matches in an attempt to assemble two input integers.
If it cannot assemble two inputs it will fill the inputs in with zero.

### Output

As you will later read, there are two methods defined for computing the greatest common divisor of two integers.
`assemble_output` takes takes both the output of `gcd'` and `gcd''` and outputs them concatenated as a string.

### GCD'

`gcd'` is defined on lines eight through 15.
It takes two integers and outputs an integer.
If one or more inputs are zero then the GCD is just the other (possibly) non-zero variable.
If both are zero then the GCD is zero.
Negative inputs are just converted to their absolute values.
This is the same behavior as `gcd` found in the `Prelude`.

`divisors` takes two integers and outputs a list of integers such that every integer in the list evenly divides
both `x` and `y`. Note that `divisors` goes from greatest to least `[a, b..1]`.
`gcd'` uses this list and returns the head/first integer found in the list since this is indeed the greatest
common divisor since the list is descending.
We could have found all of the common divisors ascending and then returned the maximum or reversed the list and took the head
but then we would have to go through the list twice.

### GCD'' (Euclid's Algorithm)

`gcd''` is Euclid's or the <i>Euclidean Algorithm</i>.

<blockquote>
The Euclidean Algorithm makes use of these properties by rapidly reducing the problem into easier and easier problems,
using the third property,  until it is easily solved by using one of the first two properties.

<footer>[Khan Academy](https://www.khanacademy.org/computing/computer-science/cryptography/modarithmetic/a/the-euclidean-algorithm)</footer>
</blockquote>

If the second parameter is zero, we just output the first absolute value of the first parameter.
Otherwise we recursively call `gcd'` where the first parameter was the second parameter and the second parameter
is the remainder of `a` divided by `b`.

## Wrap-up

Solving whiteboard problems every now and then can never hurt. We discussed the Fibonacci sequence, LCM and GCD.
All solutions were written in Haskell but the algorithms easily translate to other languages.
We discussed pattern matching, the `Maybe Monad`, `filter`, `map` and `head`. GCD was defined two ways.
One way took an iterative approach while the second way, Euclid's Algorithm, used a simple recursive method.
