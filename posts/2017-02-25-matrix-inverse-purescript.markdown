---
title: Let's make a Matrix Inverse Calculator with PureScript
jumbotron_image: /images/2017-02-25-matrix-inverse-purescript/jumbotron_image.jpg
preview_image: /images/2017-02-25-matrix-inverse-purescript/preview_image.jpg
description: Using PureScript and the Halogen library, we build a matrix inverse calculator from the ground up.
author: David Lettier
---
<!--https://pixabay.com/en/storage-sort-sorting-container-1209606/-->

## The Code and final build

![](/images/2017-02-25-matrix-inverse-purescript/ui.gif){.post-img .post-img-small .post-img-limit}

You can try out the final build of the matrix inverse calculator at [lettier.com/matrix-inverse](https://lettier.com/matrix-inverse/).
All of the code for the project is hosted on [GitHub](https://github.com/lettier/matrix-inverse-calculator).

## Who this is for

* Haskell programmers looking to solve [The JavaScript Problem](https://wiki.haskell.org/The_JavaScript_Problem)
* Programmers interested in functional programming
* JavaScript programmers looking to try out PureScript
* Web developers searching for a new approach
* Matrix inverters

## What we'll cover

* Using Yarn and NPM scripts
* Building Sass
* Setting up PureScript
* Edge cases for inverting a matrix
* Gauss-Jordan elimination method
* Reduced row echelon from
* Functional programming
* Using the Halogen library to build our UI

## Project setup

Before we begin developing, we need to set up our project with all of its files and dependencies.

### File structure

```bash
matrix-inverse-calculator/
  dist/
    app.js
    index.css
    index.html
  src/
    Main.purs
    UI.purs
    MatrixInverse.purs
    Utils.purs
  static/
    html/
      index.html
    scss/
      index.scss
  .nvmrc
  bower.json
  package.json
```

This will be layout for our project. Go ahead and run the following commands.

```bash
mkdir -p matrix-inverse-calculator
cd matrix-inverse-calculator
mkdir -p dist src static static/html static/scss
touch .nvmrc bower.json package.json
cd src
touch Main.purs UI.purs MatrixInverse.purs Utils.purs
cd ..
cd static
touch html/index.html scss/index.scss
cd ..
```

## NVM

We will need to install [Node.js](https://nodejs.org/en/) using [NVM](https://github.com/creationix/nvm).
NVM allows us to easily switch between different versions of Node.

```bash
cd ~/Downloads
wget -qO- https://raw.githubusercontent.com/creationix/nvm/v0.33.1/install.sh | bash
cd ~/matrix-inverse-calculator
echo 'v5.5.0' > .nvmrc
nvm use
```

### Yarn

To speed up the downloading and installation of our dependencies, will we use [Yarn](https://yarnpkg.com/).

``` bash
cd ~/Downloads
curl -o- -L https://yarnpkg.com/install.sh | bash
```

### NPM

Open up your favorite text editor and copy this into the `package.json` file.

```javascript
{
    "name": "matrix-inverse-calculator",
    "description": "Matrix Inverse Calculator",
    "homepage": "http://lettier.com/matrix-inverse/",
    "license": "Apache-2.0",
    "author": "David Lettier",
    "private": true,
    "scripts": {
        "installPackages": "yarn && bower install",
        "buildSrc": "pulp build",
        "buildDist": "mkdir -p dist && pulp browserify --to dist/app.js && node-sass static/scss/index.scss dist/index.css && cp -R static/images/. dist/ && cp -R static/html/. dist/",
        "watchBuildDist": "onchange './static/**/*' './src/*' -i -- yarn buildDist"
    },
    "dependencies": {
        "onchange": "^3.2.1",
        "virtual-dom": "^2.1.1"
    },
    "devDependencies": {
        "node-sass": "4.3.0",
        "onchange": "3.2.1",
        "pulp": "10.0.0",
        "purescript": "0.10.2"
    }
}
```

With this file we have four NPM scripts we can run with Yarn.
`watchBuildDist` will be really convenient when we start developing as it will build the project into `dist/`
each time we make a change to any of the project files. Once the project is built, we can just refresh our browser.

### Bower

Open up your favorite editor and copy this into the `bower.json` file.

```javascript
{
  "name": "matrix-inverse-calculator",
  "description": "Matrix Inverse Calculator",
  "homepage": "http://lettier.com/matrix-inverse/",
  "license": "Apache-2.0",
  "authors": [
    "David Lettier"
  ],
  "private": true,
  "ignore": [
    "**/.*",
    "node_modules",
    "bower_components",
    "output"
  ],
  "dependencies": {
    "purescript-arrays": "3.1.0",
    "purescript-console": "2.0.0",
    "purescript-either": "2.1.0",
    "purescript-foldable-traversable": "2.0.0",
    "purescript-globals": "2.0.0",
    "purescript-halogen": "*",
    "purescript-integers": "2.1.0",
    "purescript-math": "2.0.0",
    "purescript-maybe": "2.0.1",
    "purescript-prelude": "2.3.0"
  },
  "devDependencies": {
    "purescript-psci-support": "2.0.0"
  }
}
```

We can now go ahead and install all of our dependencies so make sure to run the following commands.

```bash
cd matrix-inverse-calculator
yarn run installPackages
```

## Inverting a matrix

There is an argument to
[never invert a matrix](https://news.ycombinator.com/item?id=11681893)---reasons
being computational space time efficiency and numerical instability.
However, inverting for us is an end-in-itself.

### The identity matrix

With matrices we cannot divide a matrix like say you would two numbers `3 / 2 = 3 * 2^(-1) = 3 * (1 / 2) = 3 / 2`.
We can however multiply a matrix by the inverse of some matrix `[[...], ...] * [[...], ...]^(-1)` provided the inverse exists.
What happens if you take a matrix `A` and times it by its inverse `A^(-1)`?
You get the identity matrix `I = A * A^(-1)`.
Multiply a matrix by the identity matrix and you get back the same matrix `A * I = A = A * (A * A^(-1)) = A * I`.

```haskell
[ 1 0 0 0 ]   [ 2 0 0 0 ]   [ 2 0 0 0 ] ^ -1   [ 2 0 0 0 ]   [ 0.5 0   0   0   ]   [ 1 0 0 0 ]
[ 0 1 0 0 ] = [ 0 2 0 0 ] * [ 0 2 0 0 ]      = [ 0 2 0 0 ] * [ 0   0.5 0   0   ] = [ 0 1 0 0 ]
[ 0 0 1 0 ]   [ 0 0 2 0 ]   [ 0 0 2 0 ]        [ 0 0 2 0 ]   [ 0   0   0.5 0   ]   [ 0 0 1 0 ]
[ 0 0 0 1 ]   [ 0 0 0 2 ]   [ 0 0 0 2 ]        [ 0 0 0 2 ]   [ 0   0   0   0.5 ]   [ 0 0 0 1 ]
```

Notice the ones going down the diagonal.
For every matrix value where the row number equals the column number notice the following.

```javascript
2 * 0.5 + 0 + 0 + 0 = 2 * (1 / 2) + 0 + 0 + 0 = 1
```

What if we took our matrix `A`,
the identity matrix `I`,
performed some operations on `A` to make it look just like `I`,
and did those same operations on `I`?
What would `I` turn into?
`I` would turn into the inverse `A^(-1)` and `A` would turn into `I`.

### Inverse criteria

Before we can find the inverse, we must satisfy the following criteria.

* `A` must be square such that it has as many rows as it does columns
    * `I` is always square
* `A` cannot have a row that is all zeros and/or a column that is all zeros
    * `I` has no rows and/or columns with all zeros
* `A` must have as many [pivot positions](https://en.wikipedia.org/wiki/Pivot_element#Pivot_position) as it does rows
    * `I` has as many pivot positions as it does rows

If we notice an all zero row or column, while we turn `A` into `I`, we have to stop.
Similarly, if find ourselves without a pivot position, as we traverse the rows, we have to stop.
We'll have to report back to the user that the matrix could not be inverted if we cannot satisfy all of the criteria.

### Elementary row operations

So what operations can we perform to turn `A` into `I`?

* Swap two rows
* Multiply a row by some non zero number
* Take a row, multiply it by some number, and then add that result to another row

<blockquote>
Elementary row operations are used in Gaussian elimination to reduce a matrix to row echelon form.
They are also used in Gauss-Jordan elimination to further reduce the matrix to reduced row echelon form.
<footer>[Elementary Matrix - Wikipedia](https://en.wikipedia.org/wiki/Elementary_matrix#Operations)</footer>
</blockquote>

### RREF

Using these operations our goal is to turn every diagonal value in `A` into a one and every other value into a zero.
This is known as reduced row echelon form (RREF).

To be in RREF, your matrix must satisfy the following.

* The ones (also known as the "pivots") have to be the only non-zero number in their column
* The pivots have to be in column order
    * From top to bottom, pivots in lower numbered columns come before pivots in higher numbered columns
* From left to right, all values before a pivot in a row have to be zero
* All zero rows are at the bottom

```haskell

No:

[ 0 0 1 0 ]
[ 0 1 0 0 ]

Yes:

[ 0 1 0 0 ]
[ 0 0 1 0 ]

No:

[ 3 1 0 0 ]
[ 0 0 1 0 ]

Yes:

[ 0 1 0 0 ]
[ 0 0 1 0 ]

No:

[ 0 0 0 0 ]
[ 0 0 1 0 ]

Yes:

[ 0 0 1 0 ]
[ 0 0 0 0 ]

No:

[ 1 0 3 0 ]
[ 0 0 1 0 ]

Yes:

[ 1 0 0 0 ]
[ 0 0 1 0 ]
```

### Gauss-Jordan elimination

Our algorithm for turning `A` into `I` follows the following pseudo code.

```ruby
do

  let A be the input matrix
  let I be the identity matrix

  if A is not square do
    return error

  while A is not in RREF do

    if A contains a zero row or column do
      return error

    in A and for each diagonal value do

      in A in current column and in rows at or below current row, find the row with a one or the largest absolute value
      in A swap the found row with the current row
      in I swap the same row numbers

      in A get current diagonal value

      if diagonal value is zero do
        return error

      in A multiply pivot row by (1 / diagonal value)
      in A overwrite pivot row with this new row

      in I multiply pivot row by (1 / diagonal value found in A)
      in I overwrite pivot row with this new row

      in A and from top to bottom row in current column do

        if current row is the pivot row
          skip

        in A get current row column value
        in A get current pivot

        in A subtract do
          in A multiply pivot row by current row column value
        from do
          in A multiply current row by pivot
        and store this new row in current row

        in I subtract do
          in I multiply current row by pivot found in A
        from do
          in I multiply pivot row by current row column value found in A
        and store this new row in current row

  return I
```

Overall our goal is to visit each diagonal value (row number equals column number),
turn that value into a one (by dividing the whole row by the diagonal value) and
zero out every other value in the column
(by subtracting the pivot row multiplied by the target value from the target row multiplied by the pivot value)
while also mirroring the same operations in _what was initially_ the identity matrix.
Note that we reuse the numbers we find in the input matrix when we perform the mirrored operations in the identity matrix.
For example, if we are dividing the current row by 11 (because the diagonal was 11) in the input matrix then we will
divide the same row number by 11 in the identity matrix.

Typically one will generate an augmented matrix by appending the identity matrix to the input matrix and then perform the
elementary operations on this augmented matrix.
By doing it this way you perform the mirrored operations implicitly.
For our implementation though we will keep the two matrices separate while we perform the inversion calculation.

To make it concrete lets run through a small example.

```haskell
Begin

A = [ 0 2 ]
    [ 2 1 ]

I = [ 1 0 ]
    [ 0 1 ]

Search for a 1 or the largest number in C1 at or below R1
Found 2 in R2 C1
Swap R1 with R2 in A
Swap R1 with R2 in I

A = [ 2 1 ]
    [ 0 2 ]

I = [ 0 1 ]
    [ 1 0 ]

R1 C1 is not 1
Multiply R1 by (1 / 2) in A
Multiply R1 by (1 / 2) in I

A = [ 1 0.5 ]
    [ 0 2   ]

I = [ 0 0.5 ]
    [ 1 0   ]

All other column values in C1 are zero so move to the next diagonal

A = [ 1 0.5 ]
    [ 0 2   ]

I = [ 0 0.5 ]
    [ 1 0   ]

R2 C2 is not 1
Multiply R2 by (1 / 2) in A
Multiply R2 by (1 / 2) in I

A = [ 1 0.5 ]
    [ 0 1   ]

I = [ 0   0.5 ]
    [ 0.5 0   ]

R1 C2 is not zero
Multiply R1 by 1 in A
Multiply R2 by 0.5 in A
Subtract and overwrite R1 in A
R1 = 1 * R1 - 0.5 * R2
R1 = 1 * [ 1 0.5 ] - 0.5 [ 0 1 ]
R1 = [1 0.5 ] - [ 0 0.5 ]
R1 = [ (1 - 0) (0.5 - 0.5) ]
R1 = [ 1 0 ]
Multiply R1 by 1 in I
Multiply R2 by 0.5 in I
Subtract and overwrite R1 in I
R1 = 1 * R1 - 0.5 * R2
R1 = 1 * [ 0 0.5 ] - 0.5 [ 0.5 0 ]
R1 = [ 0 0.5 ] - [ 0.25 0 ]
R1 = [ (0 - 0.25) (0.5 - 0) ]
R1 = [ -0.25 0.5 ]

A = [ 1 0 ]
    [ 0 1 ]

I = [ -0.25 0.5 ]
    [  0.5  0   ]

All other non-pivot column values in C2 are zero so move to the next diagonal
No other diagonals
A is now the identity matrix
Stop
Test

[ 0 2 ] * [ -0.25 0.5 ] = [ (0 * -0.25 + 2 * 0.5) (0 * 0.5 + 2 * 0) ] = [ 1 0 ]
[ 2 1 ]   [  0.5  0   ]   [ (2 * -0.25 + 1 * 0.5) (2 * 0.5 + 1 * 0) ]   [ 0 1 ]
```

## Application code

Our project is setup and we have our basic algorithm ready.
We can now begin developing our application in PureScript, HTML, and Sass.

### HTML

While we will define some of our HTML in PureScript, we can generate the page shell by hand.

Make sure to open up your favorite text editor and replace `static/html/index.html` with the following code.

```html
<!DOCTYPE html>
<!--(C) 2017 David Lettier-->
<html>
  <head>
    <title>Matrix Inverse Calculator | Lettier.com</title>
    <link rel="stylesheet" href="index.css">
  </head>
  <body>
    <div id="pageContainer">
      <div id="instructionsContainer">
        <h1>Matrix Inverse Calculator</h1>
      </div>
      <div class="row">
        <div id="uiContainer">
        </div>
      </div>
    </div>
    <script src="app.js"></script>
  </body>
</html>
```

Our generated JavaScript from PureScript will be in the `app.js` file.

### Sass

We will use the CSS pre-processor, Sass, to give our calculator some style.

Make sure to open up your favorite text editor and replace `static/scss/index.scss` with the following code.

```css
/*
  (C) 2017 David Lettier
  lettier.com
*/

$laptopLWidth: 1440px;
$laptopWidth: 1024px;
$tabletWidth: 768px;

$color1: rgba(51, 55, 69, 1);
$color2: rgba(245, 47, 87, 1);
$color3: rgba(76, 75, 99, 1);
$color4: rgba(175, 190, 209, 1);
$color5: rgba(237, 237, 244, 1);

$matrixValueInputSize: 45px;

@mixin laptopL {
  @media (max-width: #{$laptopLWidth}) {
    @content;
  }
}

@mixin laptop {
  @media (max-width: #{$laptopWidth}) {
    @content;
  }
}

@mixin tablet {
  @media (max-width: #{$tabletWidth}) {
    @content;
  }
}

body {
  font-family: sans-serif;
  margin-top:   25px;
  margin-left: 25px;
  background-color: #343438;
  color: white;
  height: 100%;
  line-height: 2;
}
input {
  margin-right: 4px;
  outline: none;
  border: none;
  border-radius: 1px;
  padding: 2px;
  font-size: 20px;
  background-color: whitesmoke;
  color: #222;
}
li {
  list-style: none;
  margin-top: 3px;
}
button {
  background-color: #6f6cb9;
  color: white;
  font-weight: bold;
  margin: 2px;
  border-style: none;
  border-radius: 1px;
  outline: none;
  font-size: 20px;
  cursor: pointer;
  -webkit-box-shadow: 0px 3px 3px 0px #190d25;
  -moz-box-shadow:    0px 3px 3px 0px #190d25;
  box-shadow:         0px 3px 3px 0px #190d25;
}
#pageContainer {
  height: 100%;
}
#chartContainer {
  margin-right: 10px;
}
#instructionsContainer {
  margin-bottom: 20px;
}
#uiContainer {
  margin-left: 10px;
}
.status {
  display: block;
}
.matrixSizeInput {
  width: 300px;
}
.matricesContainer {
  display: flex;
  margin-top: 10px;
}
.matrixAContainer input {
}
.matrixBContainer {
  margin-left: 10px;
  padding-left: 10px;
  border-left: 1px solid white;
}
.identityContainer input {
}
.matrixRow {
  display: flex;
}
.matrixValueInput {
  width:  $matrixValueInputSize;
  height: $matrixValueInputSize;
  margin: 1px;
  text-align: center;
}
.runButton {
  display: inline-block;
  background-color: #00c17c;
  margin-left: 10px;
}
.removeButton {
  background-color: #f52f57;
}
.defaultCursor {
  cursor: default;
}
.row {
  display: flex;
  flex-direction: row;
}
.blue {
  background-color: #6b89e0;
  color: white;
}
.lightBlue {
  background-color: #99b3ff;
}
.red {
  background-color: #dc5c90;
  color: white;
}
.lightRed {
  background-color: #ff86b7;
}
```

The `node-sass static/scss/index.scss dist/index.css` command in `package.json` will convert our Sass to
CSS.

### PureScript

We will be using PureScript to program the logic of our application.

<blockquote>
PureScript is a small strongly typed programming language that compiles to JavaScript.
<footer>[PureScript.org](http://purescript.org)</footer>
</blockquote>

The following files make up all of the PureScript code for our matrix inverse calculator.

#### MatrixInverse.purs

We will start with `MatrixInverse.purs`.
This file or module contains all of our matrix related functions for inverting a matrix.

```haskell
module MatrixInverse where

import Prelude

import Data.Ord (abs)
import Data.Foldable (foldl)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Array (head, length, null, range, slice, tail, updateAt, zip, (!!))
import Data.Either (Either(..))
import Data.Maybe (Maybe(Just, Nothing), fromMaybe, isNothing)
```

The top section defines the module and imports all of the necessary dependencies.
The PureScript compiler will warn us if we are importing something we don't need.
It will also throw an error if we are using something we didn't explicitly import.
This can get tedious as PureScript's package ecosystem is very granular.

```haskell
defaultMatrix :: Int -> Matrix
defaultMatrix size = buildMatrix size value
  where
    value _ _ = defaultMatrixValue

identityMatrix :: Int -> Matrix
identityMatrix size = buildMatrix size value
  where
    value row col | row == col = 1.0
                  | otherwise   = 0.0

buildMatrix :: Int -> (Int -> Int -> Number) -> Matrix
buildMatrix 0    _ = []
buildMatrix size f = map (\ row -> map (\ col -> f row col) matrixRange) matrixRange
  where
    matrixRange = range 0 (size - 1)

-- ...

defaultMatrixValue :: Number
defaultMatrixValue = 0.0
```

`defaultMatrix` inputs a number and outputs a square matrix of the given size using the `buildMatrix` function.
Every value in the matrix will be `0.0` as given by `defaultMatrixValue`.

The `identityMatrix` function takes a size and outputs a square matrix that is zeros everywhere except when the `row` and `col` numbers
match. You can see that logic in the `where` clause.

`buildMatrix` takes a size, a function that takes two integers and outputs a number, and outputs a matrix.
The input function is used to fill each value in the matrix being built.
We map over the rows and map over the columns and for each row and column we fill the value by passing the row and column number
to the function that was passed to `buildMatrix`.
Since we only care about building square matrices, we use the same range for both rows and columns.

```haskell
containsZeroRowOrCol :: Matrix -> Boolean
containsZeroRowOrCol matrix = containsZeroRow matrix || containsZeroCol matrix

containsZeroCol :: Matrix -> Boolean
containsZeroCol = containsZeroRow <<< transpose

containsZeroRow :: Matrix -> Boolean
containsZeroRow = foldl (\ acc row -> acc || foldl (\ acc' value -> acc' && value == 0.0) true row) false

isSquareMatrix :: Matrix -> Boolean
isSquareMatrix matrix = foldl (\ acc row -> acc && length row == matrixLength) true matrix
  where
    matrixLength = length matrix
```

Here we define some error checking functions.
You'll remember that our input matrix must be square and have no rows and/or columns that contain all zeros.

`isSquareMatrix` checks that every row of the matrix has the same number of elements as the number of rows in the matrix.
If one or more rows are longer or shorter than the total amount of rows, we output false.
Otherwise, if they have all the same size, we output true.

`containsZeroRow` reduces (`foldl` for fold from the left) the input matrix down to a single Boolean (true or false) value.
For every row, we check if all the values contain zero and if so, we return true for that row.
As we reduce each row down to a Boolean, we or (`||`) these together and output true or false.
If one or more rows have all zeros, our output will be true and otherwise it will be false.

`containsZeroCol` transposes the input matrix (turns every column into a row) and then uses `containsZeroRow` to check for
any row (previously a column) that contains all zeros.

So for example, take a look at the following.

```haskell
input

  [  1  2  3 0 ]
  [  5  6  7 0 ]
  [  9 10 11 0 ]
  [ 13 14 15 0 ]

transpose

  [  1  5  9 13 ]
  [  2  6 10 14 ]
  [  3  7 11 15 ]
  [  0  0  0  0 ]

reduce

  [  1  5  9 13 ] False \ False \
  [  2  6 10 14 ] False /         True
  [  3  7 11 15 ] False \ True  /
  [  0  0  0  0 ] True  /
```


Recall that once we visit a new diagonal, we have to find the best row to pivot off of.
The best is a row (in the same column as the diagonal) with a one but if we cannot find that then
we pick the row that has the largest absolute value among the column of values.
Note that the only row column values we can look at are the values at or below the current row number
of the diagonal we are at.
We do not want to swap with any rows above the current since we already pivoted off of those rows.

```haskell
  C1 C2 C3 C4
[ 1  2  3  1 ] R1
[ 0 (0) 1  1 ] R2
[ 0  2  1  1 ] R3
[ 0  1  1  1 ] R4
```

Let's say we are at the R2 C2 diagonal.

In this case the first valid row would be R4 since it contains a one in the same column as the diagonal.
We would end up swapping R2 with R4.

```haskell
  C1 C2 C3 C4
[ 1  2  3  1 ] R1
[ 0 (1) 1  1 ] R2
[ 0  8  1  1 ] R3
[ 0  9  1  1 ] R4
```

In this case the first valid row would be R2 since it contains a one in the same column as the diagonal.
We would end up swapping R2 with R2.

```haskell
  C1 C2 C3 C4
[ 1  2  3  1 ] R1
[ 0 (2) 1  1 ] R2
[ 0 -4  1  1 ] R3
[ 0  3  1  1 ] R4
```

In this case the first valid row would be R3 since it contains a negative four in the same column as the diagonal.
We would end up swapping R2 with R3.

```haskell
firstValidRow :: Matrix -> Int -> Int -> Maybe Int
firstValidRow []     _            _     = Nothing
firstValidRow matrix atRowOrBelow inCol = (foldl folder { index: Nothing, value: Nothing} tuples).index
  where
    matrixLength = length matrix
    lastRow = matrixLength - 1
    rowRange = range atRowOrBelow lastRow
    column = slice atRowOrBelow matrixLength (matrixRow (transpose matrix) inCol)
    tuples = zip rowRange column
    folder :: { index :: Maybe Int, value :: Maybe Number } -> Tuple Int Number -> { index :: Maybe Int, value :: Maybe Number }
    folder { index: Nothing, value: Nothing} (Tuple a b) = { index: (Just a), value: (Just b) }
    folder r@{ index: (Just x), value: (Just y)} (Tuple a b) =
      if abs y == 1.0
        then r
        else
          if abs b >= abs y || abs b == 1.0
            then { index: (Just a), value: (Just b) }
            else r
    folder _ _ = { index: Nothing, value: Nothing }
```

Looking at the function you can see how we go about doing this.
We input our matrix, the row and column we can start searching from, and we `Maybe` return an integer.
The `Maybe` signifies that we may not find any valid row forcing us to return `Nothing`.

This function looks verbose but really the meat of it is in the helper function `folder`.
We scan the values from `atRowOrBelow` in the column `inCol` and search for a one or the
`abs` largest value.
If found, we return `Just` the row number and `Just` the row column value for the first valid row.

```haskell
    folder :: { index :: Maybe Int, value :: Maybe Number } -> Tuple Int Number -> { index :: Maybe Int, value :: Maybe Number }

    folder { index: Nothing, value: Nothing} (Tuple a b) = { index: (Just a), value: (Just b) }

    folder r@{ index: (Just x), value: (Just y)} (Tuple a b) =
      if abs y == 1.0
        then r
        else
          if abs b >= abs y || abs b == 1.0
            then { index: (Just a), value: (Just b) }
            else r

    folder _ _ = { index: Nothing, value: Nothing }
```

Looking closer at `folder`, it takes a record, a tuple containing an integer and a real number, and returns
a record that holds maybe an integer (`index`) and maybe a real number (`value`).
There a three input scenarios `folder` _pattern matches_ on.

* The input record has nothing
    * Return a record containing just what the input tuple had
* The input record has values
    * Return the input record if its value is 1, otherwise
        * return the input record if its value is greater than the value of the input tuple or
        * return the tuple as a record if its value is greater than that of the input record
* For everything else
    * return a record containing `Nothing`

The `foldl` uses `folder` to reduce the extracted tuples, from the matrix column, down to a single record where we
access the index or row number that `Maybe` an integer if `folder` was able to find something.

```haskell
input matrix

    C1  C2  C3 C4
  [ 1   2   3  1 ] R1
  [ 0 [(2)] 1  1 ] R2
  [ 0 [-4 ] 1  1 ] R3
  [ 0 [ 3 ] 1  1 ] R4

extract tuples

  return [(R2, 2), (R3, -4), (R4, 3)]

fold 1 { index: Nothing, value: Nothing }, (R2, 2)

  return { index: Just R2, value: Just 2 }

fold 2 { index: Just R2, value: Just 2 },  (R3, -4)

  return { index: Just R3, value: Just -4 }

fold 3 { index: Just R3, value: Just -4 }, (R4, 3)

  return { index: Just R3, value: Just -4 }

return Just R3
```

Given the input matrix, we extract out the rest of the column into tuples where the first element is the row number and
the second element is the value.
We then reduce these tuples to a single record where we return the index attribute.

```haskell
multiplyRow :: Matrix -> Int -> Number -> Matrix
multiplyRow []     _   _          = []
multiplyRow matrix row multiplier = map (\ row' ->
    if row == row'
      then map (\ value -> value * multiplier) (matrixRow matrix row')
      else matrixRow matrix row'
  ) (matrixToRange matrix)
```

`multiplyRow` takes a matrix, row number, multiplier, and returns the modified matrix.
We will use this function to turn every diagonal value into one.

```haskell
clearValue :: Tuple Matrix Matrix -> Tuple Int Int -> Tuple Int Int -> Tuple Matrix Matrix
clearValue (Tuple aMat bMat) pivot@(Tuple pRow pCol) target@(Tuple tRow tCol) = Tuple aMat' bMat'
  where
    pivotRowA :: MatrixRow
    pivotRowA         = matrixRow aMat pRow
    targetRowA :: MatrixRow
    targetRowA        = matrixRow aMat tRow
    pivotValueA :: Number
    pivotValueA       = rowValue pivotRowA pCol
    targetValueA :: Number
    targetValueA      = rowValue targetRowA tCol
    aMat' :: Matrix
    aMat'             = multiplyAndSubtractRows aMat tRow pRow pivotValueA targetValueA
    bMat' :: Matrix
    bMat'             = multiplyAndSubtractRows bMat tRow pRow pivotValueA targetValueA
    rowValue :: MatrixRow -> Int -> Number
    rowValue row col  = fromMaybe defaultMatrixValue (row  !! col)
    multiplyAndSubtractRows :: Matrix -> Int -> Int -> Number -> Number -> Matrix
    multiplyAndSubtractRows
      matrix
      leftSideRow
      rightSideRow
      leftMultiplier
      rightMultiplier
      = map (\ row -> if row == leftSideRow
                        then leftRowValues'
                        else matrixRow matrix row
        ) (matrixToRange matrix)
      where
        leftRowValues  = matrixRow matrix leftSideRow
        rightRowValues = matrixRow matrix rightSideRow
        leftRowValues' = map (\ (Tuple l r) ->
            leftMultiplier * l - rightMultiplier * r
          ) (zip leftRowValues rightRowValues)

clearColumnExceptPivot :: Tuple Matrix Matrix -> Tuple Int Int -> Tuple Matrix Matrix
clearColumnExceptPivot t@(Tuple aMat bMat) pivot@(Tuple pRow pCol) = t'
  where
    t' :: Tuple Matrix Matrix
    t' = foldl folder t (matrixToRange aMat)
    folder :: Tuple Matrix Matrix -> Int -> Tuple Matrix Matrix
    folder acc@(Tuple aMat' bMat') row =
      if row == pRow
        then acc
        else clearValue acc pivot (Tuple row pCol)
```

`clearColumnExceptPivot` will helps up clear a column to all zeros except the diagonal value.
It works on two matrices at a time mirroring the operations on `aMat` in `bMat` (`mat` short for matrix).
The majority of the work occurs in `clearValue`.

`clearValue` takes a tuple containing two matrices, a tuple of two integers, another tuple of two integers,
and returns a tuple containing two matrices.
While `clearValue` may look complicated at first,
it is simply performing the `Target Row = (Pivot * Target Row) - (Target Value * Pivot Row)` update step.
This occurs in the `multiplyAndSubtractRows` function. The rest is mostly accessing row values.

```haskell
invertMatrix :: Matrix -> Tuple (Either String Matrix) Matrix
invertMatrix matrix
  | null matrix                 = Tuple (Left "Empty matrix")                (identityMatrix (length matrix))
  | not $ isSquareMatrix matrix = Tuple (Left "Not a square matrix")         (identityMatrix (length matrix))
  | containsZeroRowOrCol matrix = Tuple (Left "Contains zero row or column") (identityMatrix (length matrix))
  | otherwise = result
  where
    matrixSize = length matrix
    identity   = identityMatrix matrixSize
    result :: Tuple (Either String Matrix) Matrix
    result = foldl folder (Tuple (Right matrix) identity) (matrixToRange matrix)
    folder :: Tuple (Either String Matrix) Matrix -> Int -> Tuple (Either String Matrix) Matrix
    folder t@(Tuple acc@(Left  _) _) _        = t
    folder t@(Tuple acc@(Right a) b) diagonal =
      if containsZeroRowOrCol a || null a || isNothing maybeDivisorA || divisorA == 0.0
        then (Tuple (Left "Cannot invert") b)
        else (Tuple (Right clearedA) clearedB)
      where
        swapped       = swapWithValidRow (Tuple a b) diagonal
        swappedA      = fst swapped
        swappedB      = snd swapped
        divisorRowA   = fromMaybe [] (swappedA !! diagonal)
        maybeDivisorA = divisorRowA !! diagonal
        divisorA      = fromMaybe defaultMatrixValue maybeDivisorA
        multiplierA   = 1.0 / divisorA
        multipliedA   = multiplyRow swappedA diagonal multiplierA
        multipliedB   = multiplyRow swappedB diagonal multiplierA
        cleared       = clearColumnExceptPivot (Tuple multipliedA multipliedB) (Tuple diagonal diagonal)
        clearedA      = fst cleared
        clearedB      = snd cleared
```

The last major function is `invertMatrix`.

Let us go ahead and break it down.

```haskell
invertMatrix :: Matrix -> Tuple (Either String Matrix) Matrix
```

You can see that it takes the input matrix and returns a tuple.
The first tuple element is either a string or a matrix and the second element is a matrix.

Users of this function can test the first element of the returned tuple.
If it is a string, there was an issue with trying to invert the matrix.
However, if it is a matrix, then the inversion was possible and the first element will be the identity matrix
and the second element will be the inverse of the input matrix.

```haskell
  | null matrix                 = Tuple (Left "Empty matrix")                (identityMatrix (length matrix))
  | not $ isSquareMatrix matrix = Tuple (Left "Not a square matrix")         (identityMatrix (length matrix))
  | containsZeroRowOrCol matrix = Tuple (Left "Contains zero row or column") (identityMatrix (length matrix))
  | otherwise = result
```

These are guard clauses that check if the input matrix satisfies our criteria.
Notice that the first three guard clauses return a tuple containing the error message and the identity matrix.
If the first three scenarios do not apply then we return the result.

```haskell
    result :: Tuple (Either String Matrix) Matrix
    result = foldl folder (Tuple (Right matrix) identity) (matrixToRange matrix)
```

The `result` function reduces the input and identity matrix to either a string
and identity matrix or the identity matrix and the matrix inverse.

```haskell
    folder :: Tuple (Either String Matrix) Matrix -> Int -> Tuple (Either String Matrix) Matrix

    folder t@(Tuple acc@(Left  _) _) _        = t

    folder t@(Tuple acc@(Right a) b) diagonal =
      if containsZeroRowOrCol a || null a || isNothing maybeDivisorA || divisorA == 0.0
        then (Tuple (Left "Cannot invert") b)
        else (Tuple (Right clearedA) clearedB)
```

`folder` pattern matches twice.
The first one matches if the first element of the input (the result of the last fold iteration)
tuple is `Left` (which would be a string).
In this scenario it just returns the tuple it was given. No need to keep trying as there must have been an error.
The last one matches everything else but more specifically that the first element of the input tuple is
`Right` (which would be a matrix).
The last scenario checks four different cases before it proceeds.

```haskell
      if containsZeroRowOrCol a || null a || isNothing maybeDivisorA || divisorA == 0.0
```

If any of these are true then it returns a tuple with an error string explaining that it could not invert the matrix
and whatever was the second element of the input tuple.
However, if they are all false, then it returns a tuple containing the updated matrices.

```haskell
        swapped       = swapWithValidRow (Tuple a b) diagonal
        swappedA      = fst swapped
        swappedB      = snd swapped
        divisorRowA   = fromMaybe [] (swappedA !! diagonal)
        maybeDivisorA = divisorRowA !! diagonal
        divisorA      = fromMaybe defaultMatrixValue maybeDivisorA
        multiplierA   = 1.0 / divisorA
        multipliedA   = multiplyRow swappedA diagonal multiplierA
        multipliedB   = multiplyRow swappedB diagonal multiplierA
        cleared       = clearColumnExceptPivot (Tuple multipliedA multipliedB) (Tuple diagonal diagonal)
        clearedA      = fst cleared
        clearedB      = snd cleared
```

For each iteration of `foldl`, `folder` uses these functions.

You can see the progression from top to bottom.

* Swap current diagonal row with valid row
* Get diagonal value of newly swapped row
* Multiply the diagonal row by one over the diagonal
    * This turns the diagonal into a one or a pivot
* Zero out every value in the diagonal column except the pivot
* Mirror all the operations in the second matrix

Make sure to open up your favorite text editor and replace `src/MatrixInverse.purs` with the following code.

```haskell
{-
  (C) 2017 David Lettier
  lettier.com
-}

module MatrixInverse where

import Prelude

import Data.Ord (abs)
import Data.Foldable (foldl)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Array (head, length, null, range, slice, tail, updateAt, zip, (!!))
import Data.Either (Either(..))
import Data.Maybe (Maybe(Just, Nothing), fromMaybe, isNothing)

type MatrixRow = Array Number
type Matrix = Array MatrixRow

invertMatrix :: Matrix -> Tuple (Either String Matrix) Matrix
invertMatrix matrix
  | null matrix                 = Tuple (Left "Empty matrix")                (identityMatrix (length matrix))
  | not $ isSquareMatrix matrix = Tuple (Left "Not a square matrix")         (identityMatrix (length matrix))
  | containsZeroRowOrCol matrix = Tuple (Left "Contains zero row or column") (identityMatrix (length matrix))
  | otherwise = result
  where
    matrixSize = length matrix
    identity   = identityMatrix matrixSize
    result :: Tuple (Either String Matrix) Matrix
    result = foldl folder (Tuple (Right matrix) identity) (matrixToRange matrix)
    folder :: Tuple (Either String Matrix) Matrix -> Int -> Tuple (Either String Matrix) Matrix
    folder t@(Tuple acc@(Left  _) _) _        = t
    folder t@(Tuple acc@(Right a) b) diagonal =
      if containsZeroRowOrCol a || null a || isNothing maybeDivisorA || divisorA == 0.0
        then (Tuple (Left "Cannot invert") b)
        else (Tuple (Right clearedA) clearedB)
      where
        swapped       = swapWithValidRow (Tuple a b) diagonal
        swappedA      = fst swapped
        swappedB      = snd swapped
        divisorRowA   = fromMaybe [] (swappedA !! diagonal)
        maybeDivisorA = divisorRowA !! diagonal
        divisorA      = fromMaybe defaultMatrixValue maybeDivisorA
        multiplierA   = 1.0 / divisorA
        multipliedA   = multiplyRow swappedA diagonal multiplierA
        multipliedB   = multiplyRow swappedB diagonal multiplierA
        cleared       = clearColumnExceptPivot (Tuple multipliedA multipliedB) (Tuple diagonal diagonal)
        clearedA      = fst cleared
        clearedB      = snd cleared

defaultMatrix :: Int -> Matrix
defaultMatrix size = buildMatrix size value
  where
    value _ _ = defaultMatrixValue

identityMatrix :: Int -> Matrix
identityMatrix size = buildMatrix size value
  where
    value row col | row == col = 1.0
                  | otherwise  = 0.0

buildMatrix :: Int -> (Int -> Int -> Number) -> Matrix
buildMatrix 0    _ = []
buildMatrix size f = map (\ row -> map (\ col -> f row col) matrixRange) matrixRange
  where
    matrixRange = range 0 (size - 1)

matrixSizeValid :: Int -> Boolean
matrixSizeValid size = size <= maxMatrixSize && size >= minMatrixSize

maybeMatrixValue :: Matrix -> Int -> Int -> Maybe Number
maybeMatrixValue matrix row col = case matrix !! row of
                                    Nothing -> Nothing
                                    Just x  -> x !! col

matrixRow :: Matrix -> Int -> MatrixRow
matrixRow matrix i = fromMaybe [] (matrix !! i)

firstValidRow :: Matrix -> Int -> Int -> Maybe Int
firstValidRow []     _            _     = Nothing
firstValidRow matrix atRowOrBelow inCol = (foldl folder { index: Nothing, value: Nothing} tuples).index
  where
    matrixLength = length matrix
    lastRow = matrixLength - 1
    rowRange = range atRowOrBelow lastRow
    column = slice atRowOrBelow matrixLength (matrixRow (transpose matrix) inCol)
    tuples = zip rowRange column
    folder :: { index :: Maybe Int, value :: Maybe Number } -> Tuple Int Number -> { index :: Maybe Int, value :: Maybe Number }
    folder { index: Nothing, value: Nothing} (Tuple a b) = { index: (Just a), value: (Just b) }
    folder r@{ index: (Just x), value: (Just y)} (Tuple a b) =
      if abs y == 1.0
        then r
        else
          if abs b >= abs y || abs b == 1.0
            then { index: (Just a), value: (Just b) }
            else r
    folder _ _ = { index: Nothing, value: Nothing }

swapWithValidRow :: Tuple Matrix Matrix -> Int -> Tuple Matrix Matrix
swapWithValidRow t@(Tuple a b) diagonal = Tuple a' b'
  where
    row = firstValidRow a diagonal diagonal
    yesSwap = case row of
                Nothing -> false
                Just r  -> r /= diagonal
    swapIfYes m =
      if yesSwap
         then swapRows m diagonal (fromMaybe diagonal row)
         else m
    a' = swapIfYes a
    b' = swapIfYes b

transpose :: Matrix -> Matrix
transpose []     = []
transpose matrix = transpose' matrix []
  where
    transpose' :: Matrix -> Matrix -> Matrix
    transpose' old new = if arrayHasNothing maybeHeads then new else transpose' tails (new <> [heads])
      where
        maybeHeads = map head old
        maybeTails = map tail old
        heads = map (fromMaybe 0.0) maybeHeads
        tails = map (fromMaybe []) maybeTails
        arrayHasNothing :: forall a. Array (Maybe a) -> Boolean
        arrayHasNothing array = case array !! 0 of
                                  Nothing  -> true
                                  (Just x) -> isNothing x

swapRows :: Matrix -> Int -> Int -> Matrix
swapRows []     _ _ = []
swapRows matrix a b = fromMaybe [] (updateRow b rowA (updateRow a rowB (Just matrix)))
  where
    rowA :: Maybe MatrixRow
    rowA = matrix !! a
    rowB :: Maybe MatrixRow
    rowB = matrix !! b
    updateRow :: Int -> Maybe MatrixRow -> Maybe Matrix -> Maybe Matrix
    updateRow i (Just row) (Just matrix') = updateAt i row matrix'
    updateRow _ _ _ = Nothing

multiplyRow :: Matrix -> Int -> Number -> Matrix
multiplyRow []     _   _          = []
multiplyRow matrix row multiplier = map (\ row' ->
    if row == row'
      then map (\ value -> value * multiplier) (matrixRow matrix row')
      else matrixRow matrix row'
  ) (matrixToRange matrix)

clearValue :: Tuple Matrix Matrix -> Tuple Int Int -> Tuple Int Int -> Tuple Matrix Matrix
clearValue (Tuple aMat bMat) pivot@(Tuple pRow pCol) target@(Tuple tRow tCol) = Tuple aMat' bMat'
  where
    pivotRowA :: MatrixRow
    pivotRowA         = matrixRow aMat pRow
    targetRowA :: MatrixRow
    targetRowA        = matrixRow aMat tRow
    pivotValueA :: Number
    pivotValueA       = rowValue pivotRowA pCol
    targetValueA :: Number
    targetValueA      = rowValue targetRowA tCol
    aMat' :: Matrix
    aMat'             = multiplyAndSubtractRows aMat tRow pRow pivotValueA targetValueA
    bMat' :: Matrix
    bMat'             = multiplyAndSubtractRows bMat tRow pRow pivotValueA targetValueA
    rowValue :: MatrixRow -> Int -> Number
    rowValue row col  = fromMaybe defaultMatrixValue (row  !! col)
    multiplyAndSubtractRows :: Matrix -> Int -> Int -> Number -> Number -> Matrix
    multiplyAndSubtractRows
      matrix
      leftSideRow
      rightSideRow
      leftMultiplier
      rightMultiplier
      = map (\ row -> if row == leftSideRow
                        then leftRowValues'
                        else matrixRow matrix row
        ) (matrixToRange matrix)
      where
        leftRowValues  = matrixRow matrix leftSideRow
        rightRowValues = matrixRow matrix rightSideRow
        leftRowValues' = map (\ (Tuple l r) ->
            leftMultiplier * l - rightMultiplier * r
          ) (zip leftRowValues rightRowValues)

clearColumnExceptPivot :: Tuple Matrix Matrix -> Tuple Int Int -> Tuple Matrix Matrix
clearColumnExceptPivot t@(Tuple aMat bMat) pivot@(Tuple pRow pCol) = t'
  where
    t' :: Tuple Matrix Matrix
    t' = foldl folder t (matrixToRange aMat)
    folder :: Tuple Matrix Matrix -> Int -> Tuple Matrix Matrix
    folder acc@(Tuple aMat' bMat') row =
      if row == pRow
        then acc
        else clearValue acc pivot (Tuple row pCol)

containsZeroRowOrCol :: Matrix -> Boolean
containsZeroRowOrCol matrix = containsZeroRow matrix || containsZeroCol matrix

containsZeroCol :: Matrix -> Boolean
containsZeroCol = containsZeroRow <<< transpose

containsZeroRow :: Matrix -> Boolean
containsZeroRow = foldl (\ acc row -> acc || foldl (\ acc' value -> acc' && value == 0.0) true row) false

isSquareMatrix :: Matrix -> Boolean
isSquareMatrix matrix = foldl (\ acc row -> acc && length row == matrixLength) true matrix
  where
    matrixLength = length matrix

matrixToRange :: Matrix -> Array Int
matrixToRange []     = []
matrixToRange matrix = (range 0 (length matrix - 1))

maxMatrixSize :: Int
maxMatrixSize = 10

minMatrixSize :: Int
minMatrixSize = 2

defaultMatrixValue :: Number
defaultMatrixValue = 0.0
```

#### UI.purs

Now that we have our matrix inverse library done, we can work on the user interface that will allow us to input and interact
with our matrix inverse calculator.

```haskell
module UI where

import Prelude

import Data.Generic (gShow)
import Data.Int (fromString)
import Data.Tuple (fst, snd)
import Data.Array (null, range, updateAt, (!!))
import Data.Either (Either(..))
import Data.Maybe (fromMaybe, isJust)

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Free (class Affable)
import Control.Monad.Aff.Console (CONSOLE, log)
import Control.Monad.Eff (Eff)

import Halogen as H
import Halogen.HTML.Core (className)
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Properties.Indexed as HP
import Halogen.HTML.Indexed as HH
import Halogen.Util (awaitBody, runHalogenAff, selectElement)

import Utils (
      maybeToString
    , stringToMaybeNumber
  )

import MatrixInverse (
      Matrix
    , invertMatrix
    , matrixSizeValid
    , maybeMatrixValue
    , defaultMatrix
    , identityMatrix
    , defaultMatrixValue
    , minMatrixSize
    , maxMatrixSize
  )
```

Like before we define the `UI` module and we import our dependencies.
Notice how we import our custom `MatrixInverse` library.

```haskell
type State = {
      matrixSize :: Int
    , matrixA :: Matrix
    , matrixB :: Matrix
    , status :: String
    , running :: Boolean
    , finished :: Boolean
  }

initialState :: State
initialState = { matrixSize: 0, matrixA: [], matrixB: [], status: "", running: false, finished: false }
```

Our UI state consists of the size of the matrix, two matrices, a status message, and two Booleans indicating
if the calculator is running and if it finished the calculation after having run.

```haskell
data Query a = UpdateMatrixSize String a | UpdateMatrixValue Int Int String a | Run a
```

Our UI is setup to handle three messages or _queries_.
The first updates the size of the input matrix.
The second updates the input matrix with a new value at the specified row and column.
The third fires off the inversion attempt then updates the UI component's state
once the calculator comes back with a result.

```haskell
      HH.div_
        [
            HH.div_ [
                  HH.div [
                        HP.class_ (className "status")
                    ] [
                        HH.b_ [ HH.text "Status: " ]
                      , HH.text state.status
                    ]
                , HH.input [
                        HP.value ""
                      , HP.placeholder "Input Matrix Size and Press Enter"
                      , HE.onValueChange (HE.input UpdateMatrixSize)
                      , HP.class_ (className "matrixSizeInput")
                    ]
                , HH.button [
                        HE.onClick (HE.input_ Run)
                      , HP.class_ (className "runButton")
                      , HP.title "RUN"
                    ] [
                        HH.text "RUN"
                    ]
              ]
```

Directly in the code we define the HTML.
This part contains the status message, matrix size input box, and the run button.

```haskell
            if matrixSizeValid state.matrixSize
              then [
                    HH.div [
                          HP.class_ (className "matrixAContainer")
                      ] (
                        map (\ row ->
                              HH.div [
                                    HP.class_ (className "matrixRow")
                                ] (
                                  map (\ col ->
                                      HH.div [
                                            HP.classes $ map className ["matrixValue"]
                                        ] [
                                            HH.input [
                                                  HP.value $ maybeToString $ maybeMatrixValue state.matrixA row col
                                                , HP.placeholder (gShow defaultMatrixValue)
                                                , HE.onValueChange (HE.input (UpdateMatrixValue row col))
                                                , HP.classes $ map className [
                                                      "matrixValueInput"
                                                    , if row == col
                                                        then
                                                          if state.finished
                                                            then "lightRed"
                                                            else "lightBlue"
                                                        else
                                                          if state.finished
                                                            then "red"
                                                            else "blue"
                                                  ]
                                                , HP.disabled state.running
                                                , HP.title $ maybeToString $ maybeMatrixValue state.matrixA row col
                                              ]
                                        ]
                                    ) (range 0 (state.matrixSize -1))
                                )
                          ) (range 0 (state.matrixSize - 1))
                      )
```

If the matrix size is valid, we display all of the input boxes that make up the input matrix.
Using CSS, we color the diagonal input boxes slightly different.
Once a run finishes, we swap the colors of the left and right matrix.

```haskell
                  , HH.div [
                          HP.class_ (className "matrixBContainer")
                      ] (
                        map (\ row ->
                              HH.div [
                                    HP.class_ (className "matrixRow")
                                ] (
                                  map (\ col ->
                                      HH.div [
                                            HP.classes $ map className ["matrixValue"]
                                        ] [
                                            HH.input [
                                                  HP.value $ maybeToString $ maybeMatrixValue state.matrixB row col
                                                , HP.placeholder (gShow defaultMatrixValue)
                                                , HP.classes $ map className [
                                                      "matrixValueInput"
                                                    , if row == col
                                                        then
                                                          if state.finished
                                                            then "lightBlue"
                                                            else "lightRed"
                                                        else
                                                          if state.finished
                                                            then "blue"
                                                            else "red"
                                                  ]
                                                , HP.disabled true
                                                , HP.title $ maybeToString $ maybeMatrixValue state.matrixB row col
                                              ]
                                        ]
                                    ) (range 0 (state.matrixSize -1))
                                )
                          ) (range 0 (state.matrixSize - 1))
                      )
                ]
```

Here we update the DOM with what is initially the identity matrix.
After a successful run, it will be the matrix inverse.
Since this is not the input matrix, we disable all of the input fields.

With the dynamic HTML in place, we can create the functions that will handle our three query messages.

```haskell
    eval :: Query ~> H.ComponentDSL State Query (Aff (Effects eff))
    eval (UpdateMatrixSize value next) = do
      let matrixSize = fromMaybe 0 (fromString value)
      if not $ matrixSizeValid matrixSize
        then H.modify (\ state ->
            state {
                  matrixSize = 0
                , matrixA    = []
                , matrixB    = []
                , status     = "Enter a number between " <> (gShow minMatrixSize) <> " and " <> (gShow maxMatrixSize)
                , running    = false
                , finished   = false
              }
          )
        else do
          H.modify (\ state ->
              state {
                    matrixSize = matrixSize
                  , matrixA    = defaultMatrix matrixSize
                  , matrixB    = identityMatrix matrixSize
                  , status     = ""
                  , running    = false
                  , finished   = false
                }
            )
      pure next
```

After the user attempts to update the matrix size, we check to see if the value is valid.
If valid, we go ahead and create a default and identity matrix.
If invalid, we update the status message and clear out both of the state's matrices.

```haskell
    eval (UpdateMatrixValue row col value next) = do
      log' value
      currentState <- H.get
      let maybeNumber = stringToMaybeNumber value
      let matrixRow = fromMaybe [] (currentState.matrixA !! row)
      let number = fromMaybe defaultMatrixValue maybeNumber
      when (not $ null matrixRow) do
        let maybeUpdatedRow = updateAt col number matrixRow
        when (isJust maybeUpdatedRow) do
          let updatedRow = fromMaybe [] maybeUpdatedRow
          let maybeUpdatedMatrix = updateAt row updatedRow currentState.matrixA
          when (isJust maybeUpdatedMatrix) do
            H.modify (\ state ->
                state {
                      matrixA  = (fromMaybe [] maybeUpdatedMatrix)
                    , running  = false
                    , finished = false
                  }
              )
      pure next
```

When the user inputs a matrix value, we perform error checking and if valid,
we update the state's `matrixA` with the new value at the specified row and column number.

```haskell
    eval (Run next) = do
      H.modify (\ state -> state { status = "Running", running = true })
      currentState <- H.get
      let matrix = currentState.matrixA
      log' (gShow matrix)
      let result = invertMatrix matrix
      let fstResult = fst result
      let sndResult = snd result
      case fstResult of
        Left s ->
          H.modify (\ state ->
              state {
                    status  = s
                  , running = false
                  , finished = false
                }
            )
        Right m -> do
          log' (gShow m)
          log' (gShow sndResult)
          H.modify (\ state ->
              state {
                    status   = "Finished"
                  , matrixA  = m
                  , matrixB  = sndResult
                  , running  = false
                  , finished = true
                }
            )
      pure next
```

After clicking the run button, we set the state's `status` and `running` attribute.
Next we get the current input matrix and attempt to invert it.
If there was an error, we update the `status`.
Otherwise we set `matrixA` to the identity matrix and
set `matrixB` to the inverse of the input matrix (as returned by `invertMatrix`).

Make sure to open up your favorite text editor and replace `src/UI.purs` with the following code.

```haskell
{-
  (C) 2017 David Lettier
  lettier.com
-}

module UI where

import Prelude

import Data.Generic (gShow)
import Data.Int (fromString)
import Data.Tuple (fst, snd)
import Data.Array (null, range, updateAt, (!!))
import Data.Either (Either(..))
import Data.Maybe (fromMaybe, isJust)

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Free (class Affable)
import Control.Monad.Aff.Console (CONSOLE, log)
import Control.Monad.Eff (Eff)

import Halogen as H
import Halogen.HTML.Core (className)
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Properties.Indexed as HP
import Halogen.HTML.Indexed as HH
import Halogen.Util (awaitBody, runHalogenAff, selectElement)

import Utils (
      maybeToString
    , stringToMaybeNumber
  )

import MatrixInverse (
      Matrix
    , invertMatrix
    , matrixSizeValid
    , maybeMatrixValue
    , defaultMatrix
    , identityMatrix
    , defaultMatrixValue
    , minMatrixSize
    , maxMatrixSize
  )

type Effects eff = H.HalogenEffects (console :: CONSOLE | eff)

data Query a = UpdateMatrixSize String a | UpdateMatrixValue Int Int String a | Run a

type State = {
      matrixSize :: Int
    , matrixA :: Matrix
    , matrixB :: Matrix
    , status :: String
    , running :: Boolean
    , finished :: Boolean
  }

initialState :: State
initialState = { matrixSize: 0, matrixA: [], matrixB: [], status: "", running: false, finished: false }

runApp :: Eff (Effects ()) Unit
runApp = runHalogenAff do
  body <- awaitBody
  uiContainer <- selectElement "#uiContainer"
  H.runUI matrixUIComponent initialState (fromMaybe body uiContainer)

matrixUIComponent :: forall eff. H.Component State Query (Aff (Effects eff))
matrixUIComponent = H.component { render, eval }
  where
    render :: State -> H.ComponentHTML Query
    render state =
      HH.div_
        [
            HH.div_ [
                  HH.div [
                        HP.class_ (className "status")
                    ] [
                        HH.b_ [ HH.text "Status: " ]
                      , HH.text state.status
                    ]
                , HH.input [
                        HP.value ""
                      , HP.placeholder "Input Matrix Size and Press Enter"
                      , HE.onValueChange (HE.input UpdateMatrixSize)
                      , HP.class_ (className "matrixSizeInput")
                    ]
                , HH.button [
                        HE.onClick (HE.input_ Run)
                      , HP.class_ (className "runButton")
                      , HP.title "RUN"
                    ] [
                        HH.text "RUN"
                    ]
              ]
          , HH.div [
              HP.class_ (className "matricesContainer")
            ]
            if matrixSizeValid state.matrixSize
              then [
                    HH.div [
                          HP.class_ (className "matrixAContainer")
                      ] (
                        map (\ row ->
                              HH.div [
                                    HP.class_ (className "matrixRow")
                                ] (
                                  map (\ col ->
                                      HH.div [
                                            HP.classes $ map className ["matrixValue"]
                                        ] [
                                            HH.input [
                                                  HP.value $ maybeToString $ maybeMatrixValue state.matrixA row col
                                                , HP.placeholder (gShow defaultMatrixValue)
                                                , HE.onValueChange (HE.input (UpdateMatrixValue row col))
                                                , HP.classes $ map className [
                                                      "matrixValueInput"
                                                    , if row == col
                                                        then
                                                          if state.finished
                                                            then "lightRed"
                                                            else "lightBlue"
                                                        else
                                                          if state.finished
                                                            then "red"
                                                            else "blue"
                                                  ]
                                                , HP.disabled state.running
                                                , HP.title $ maybeToString $ maybeMatrixValue state.matrixA row col
                                              ]
                                        ]
                                    ) (range 0 (state.matrixSize -1))
                                )
                          ) (range 0 (state.matrixSize - 1))
                      )
                  , HH.div [
                          HP.class_ (className "matrixBContainer")
                      ] (
                        map (\ row ->
                              HH.div [
                                    HP.class_ (className "matrixRow")
                                ] (
                                  map (\ col ->
                                      HH.div [
                                            HP.classes $ map className ["matrixValue"]
                                        ] [
                                            HH.input [
                                                  HP.value $ maybeToString $ maybeMatrixValue state.matrixB row col
                                                , HP.placeholder (gShow defaultMatrixValue)
                                                , HP.classes $ map className [
                                                      "matrixValueInput"
                                                    , if row == col
                                                        then
                                                          if state.finished
                                                            then "lightBlue"
                                                            else "lightRed"
                                                        else
                                                          if state.finished
                                                            then "blue"
                                                            else "red"
                                                  ]
                                                , HP.disabled true
                                                , HP.title $ maybeToString $ maybeMatrixValue state.matrixB row col
                                              ]
                                        ]
                                    ) (range 0 (state.matrixSize -1))
                                )
                          ) (range 0 (state.matrixSize - 1))
                      )
                ]
              else []
        ]
    eval :: Query ~> H.ComponentDSL State Query (Aff (Effects eff))
    eval (UpdateMatrixSize value next) = do
      let matrixSize = fromMaybe 0 (fromString value)
      if not $ matrixSizeValid matrixSize
        then H.modify (\ state ->
            state {
                  matrixSize = 0
                , matrixA    = []
                , matrixB    = []
                , status     = "Enter a number between " <> (gShow minMatrixSize) <> " and " <> (gShow maxMatrixSize)
                , running    = false
                , finished   = false
              }
          )
        else do
          H.modify (\ state ->
              state {
                    matrixSize = matrixSize
                  , matrixA    = defaultMatrix matrixSize
                  , matrixB    = identityMatrix matrixSize
                  , status     = ""
                  , running    = false
                  , finished   = false
                }
            )
      pure next
    eval (UpdateMatrixValue row col value next) = do
      log' value
      currentState <- H.get
      let maybeNumber = stringToMaybeNumber value
      let matrixRow = fromMaybe [] (currentState.matrixA !! row)
      let number = fromMaybe defaultMatrixValue maybeNumber
      when (not $ null matrixRow) do
        let maybeUpdatedRow = updateAt col number matrixRow
        when (isJust maybeUpdatedRow) do
          let updatedRow = fromMaybe [] maybeUpdatedRow
          let maybeUpdatedMatrix = updateAt row updatedRow currentState.matrixA
          when (isJust maybeUpdatedMatrix) do
            H.modify (\ state ->
                state {
                      matrixA  = (fromMaybe [] maybeUpdatedMatrix)
                    , running  = false
                    , finished = false
                  }
              )
      pure next
    eval (Run next) = do
      H.modify (\ state -> state { status = "Running", running = true })
      currentState <- H.get
      let matrix = currentState.matrixA
      log' (gShow matrix)
      let result = invertMatrix matrix
      let fstResult = fst result
      let sndResult = snd result
      case fstResult of
        Left s ->
          H.modify (\ state ->
              state {
                    status  = s
                  , running = false
                  , finished = false
                }
            )
        Right m -> do
          log' (gShow m)
          log' (gShow sndResult)
          H.modify (\ state ->
              state {
                    status   = "Finished"
                  , matrixA  = m
                  , matrixB  = sndResult
                  , running  = false
                  , finished = true
                }
            )
      pure next

log' :: forall a b. Affable (console :: CONSOLE | b) a => String -> a Unit
log' string = H.fromAff $ when debug $ log string

debug :: Boolean
debug = true
```

#### Utils.purs

The utility module contains various functions that ease development.

Make sure to open up your favorite text editor and
replace `src/Utils.purs` with the following code.

```haskell
{-
  (C) 2017 David Lettier
  lettier.com
-}

module Utils where

import Prelude

import Global (readFloat, isNaN, isFinite)

import Data.Generic (class Generic, gShow)
import Data.Array (length, head)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Foldable (foldl)
import Data.Int (toNumber)

firstOrSecondValues :: (Array Number -> Maybe Number) -> Array (Array Number) -> Array Number
firstOrSecondValues f [] = []
firstOrSecondValues f es = foldl innerFold [] (map f es)
  where
    innerFold acc (Just e) = acc <> [e]
    innerFold acc Nothing  = acc

first :: Array Number -> Maybe Number
first [x, y] = Just x
first _      = Nothing

second :: Array Number -> Maybe Number
second [x, y] = Just y
second _      = Nothing

lengthNum :: forall a. Array a -> Number
lengthNum [] = 0.0
lengthNum xs = (toNumber <<< length) xs

stringToMaybeNumber :: String -> Maybe Number
stringToMaybeNumber s = maybeNumber
  where
    float :: Number
    float = readFloat s
    maybeNumber :: Maybe Number
    maybeNumber = if isNaN float then Nothing else Just float

maybeNumberToString :: Maybe Number -> String
maybeNumberToString Nothing  = ""
maybeNumberToString (Just a) = gShow a

isInfinity :: Number -> Boolean
isInfinity = not <<< isFinite

arrayMinOrMax :: forall a. (Ord a) => (a -> a -> a) -> a -> Array a -> a
arrayMinOrMax _ default []     = default
arrayMinOrMax f _       [h]    = h
arrayMinOrMax f default values = foldl (\ acc value -> f acc value) (fromMaybe default (head values)) values

numberInvalid :: Number -> Boolean
numberInvalid n = isNaN n || isInfinity n

maybeToString :: forall a. Generic a => Maybe a -> String
maybeToString maybe = case maybe of
                        Nothing -> ""
                        Just x  -> gShow x
```

#### Main.purs

At long last we arrive at `main` where our matrix inverse calculator begins.

Make sure to open up your favorite text editor and
replace `Main.purs` with the following code.

```haskell
{-
  (C) 2017 David Lettier
  lettier.com
-}

module Main where

import Prelude

import Control.Monad.Eff (Eff)

import UI (runApp, Effects)

main :: Eff (Effects ()) Unit
main = runApp
```

`main` takes no input, has side effects (`Eff (Effects())`), and returns nothing of importance (`Unit`).
Note that `Effects` and `runApp` come from our custom module, `UI`.

```haskell
Eff (Effects ()) Unit

-- =

type Effects eff = H.HalogenEffects (console :: CONSOLE | eff)

-- +

type HalogenEffects eff = (avar :: AVAR, err :: EXCEPTION, dom :: DOM | eff)

-- =

Eff (avar :: AVAR, err, EXCEPTION, dom :: DOM, console :: CONSOLE | ()) Unit
```

Taking a closer look at the type alias `Effects`, we see that it is all of Halogen's effects
(which are asynchronous variables, exceptions, and DOM manipulations) plus
sending output to the console.

<blockquote>
PureScript’s purescript-eff package defines a monad called Eff,
which is used to handle native effects.
The goal of the Eff monad is to provide a typed API for effectful computations,
while at the same time generating efficient Javascript.
<footer>[Handling Native Effects with the Eff Monad by Phil Freeman](
http://www.purescript.org/learn/eff/
)</footer>
</blockquote>


## Building

We have set up and programmed our entire application.
At this point we can build the project and finally try out the matrix inverse calculator.

```bash
cd matrix-inverse-calculator
yarn run buildDist
cd dist
xdg-open index.html # Or point your browser to file://.../matrix-inverse-calculator/dist/index.html
```

## Wrap-up

From the ground up, we implemented a matrix inverse calculator
using [PureScript](http://www.purescript.org/) and [PureScript-Halogen](https://github.com/slamdata/purescript-halogen).
We discussed using NVM and Yarn to manage our dependencies and automate the building of our project.
We looked at the what, why, and how of inverting a matrix.
We covered the basic Gauss-Jordan elimination algorithm for turning
the input matrix into the identity matrix and the identity matrix into the
inverse of the input matrix.
We took our pseudo code and turned it into our matrix inverse PureScript module.
We defined the dynamically generated HTML and query message handlers that power our user interface.
And lastly we compiled and linked our application together getting it ready for distribution.

If you enjoyed this tutorial be sure to read
[Let's make a Linear Regression Calculator with PureScript](/posts/2017-01-15-linear-regression-and-the-amazing-beard.html) where
we use PureScript to make an interactive, browser-based simple linear regression calculator.
