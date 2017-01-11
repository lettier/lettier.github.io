---
title: Linear Regression and the Amazing Beard
jumbotron_image: /images/2017-01-15-linear-regression-and-the-amazing-beard/jumbotron_image.jpg
preview_image: /images/2017-01-15-linear-regression-and-the-amazing-beard/preview_image.jpg
description: Using PureScript, Halogen, and Chart.js, we implement simple linear regression,
  using the gradient descent algorithm, from scratch.
author: David Lettier
---
<!--https://pixabay.com/en/ball-glass-about-reflection-625908/-->

## Demo and Codebase

To play around with building a simple linear regression model in your browser,
try out the visually interactive [demo](https://lettier.com/linear-regression/).
All of the code for the demo is hosted on [GitHub](https://github.com/lettier/interactive-simple-linear-regression).
Stars are always appreciated.

## The Request

![The SideShow](/images/2017-01-15-linear-regression-and-the-amazing-beard/request.jpg){.post-img .post-img-fill}
<!--https://pixabay.com/en/circus-tarot-seer-742054/-->

As you put away your crystal ball for the evening, the ringmaster steps in.
"We need you to foretell how long Hans' beard will be in the future."
You being the fortune teller, the ringmaster assumes this is directly in your wheelhouse.
"Right now it is 17 feet long but I want marketing to start saying it is 18, 19, 20 whatever.
Since you're never wrong, no one will be any the wiser by the time we reach our next venue."

Hans has an amazing beard---probably the longest in the world---and it always gathers a crowd.
You see marketing is always one stop ahead.
They drum up anticipation so that when the sideshow finally comes, everyone cannot wait to see all of the marvels.

The idea is for you to predict how long Hans' beard will be at the moment in time when the show reaches the next stop.
This way they can market the future length now, in the next town over, and not be found a fraud by the time Hans arrives.
"The bigger the number, the larger the crowd," says the ringmaster.

You are given some historical data.
The independent variable is how many months it has been since they started recording the length.
The dependent variable is how long Hans' beard was at the time of measurement.
Note that every stop usually lasts about a month long.

```javascript
| Month | Length |
|-------|--------|
| 0     | 5.3    |
| 1     | 8.3    |
| 2     | 11.8   |
| 3     | 11.9   |
| 4     | 13.2   |
| 5     | 12.1   |
| 6     | 15.8   |
| 7     | 16.2   |
| 8     | 17.01  |
| 9     | 17     |
```

## Plotting the Data

Recently you've been taking a few MOOCs with the hope of becoming a data scientist.
Fortune telling doesn't pay what it used to and your cousin says data science is a pretty hot right now.

![](/images/2017-01-15-linear-regression-and-the-amazing-beard/data_plot.jpg){.post-img .post-img-small .post-img-limit}

Not sure where to start, you decide to plot the data to get a feel for it.

![](/images/2017-01-15-linear-regression-and-the-amazing-beard/data_plot_string.jpg){.post-img .post-img-small .post-img-limit}

As you study the dots, a string from your robe falls and lays ever so slighty across the dots.

![](/images/2017-01-15-linear-regression-and-the-amazing-beard/data_plot_string_swivel.jpg){.post-img .post-img-small .post-img-limit}

You notice the string gives the dots an up-and-to-the-right type trajectory.
Pulling it taught, you swivel the string through the dots trying to cross as many as possible.

## Hypothesis Function

As you play with the string, you get the idea that you could some how _summarize_ the data with only the string.
Why not use the equation for a straight line you think to yourself.
This equation could model the data.
Not only could you use it to summarize but you could also use it to predict future data points with only a single function.

```haskell
-- y = mx + b
hypothesis :: Number -> Number -> Number -> Number
hypothesis yIntercept slope x = yIntercept + slope * x
```

We will use PureScript to program our solution into the computer.
If you have ever used Haskell, the syntax is very similar.
If you have not used Haskell, don't worry as the code will be very readable.

<blockquote>
PureScript is a small strongly typed programming language that compiles to JavaScript.
<footer>[PureScript.org](http://purescript.org)</footer>
</blockquote>

This is our hypothesis function which is the point-slope form of the equation for a straight line.
Our hypothesis is that the equation of a straight line will best model for the data we were given.
This line will cross the y-axis at the `yIntercept` when `x` is zero.
If the `slope` is zero, the line is horizontal at the `yIntercept`.

## Cost Function

You know that a line will work well but which line?
Any straight line you choose will not intersect all of the points.
So every possible line will be _off_ in some way.

What you really need to figure out is what y-intercept and slope to use.
You want to find the best parameters such that the resulting line
will be the least off out of all the possible straight line choices.

Just then the ringleader steps into your tent, "Just so you know, if I find out you're wrong, I'll have to dock your pay."
"Oh great," you think to yourself, "If I don't pick the best line, it will cost me."

You propose a cost function as a way to model how far off your hypothesis function is.

```haskell
cost :: (Number -> Number -> Number -> Number) -> Number -> Number -> Array (Array Number) -> Number
cost _ _ _ [] = 0.0
cost hypothesisFunc yIntercept slope values = (sum <<< map error) values / size
  where
    size :: Number
    size = (toNumber <<< length) values
    error :: Array Number -> Number
    error = arrayNumberHandler error'
    error' :: Number -> Number -> Number
    error' x y = pow (y - (hypothesisFunc yIntercept slope x)) 2.0
```

Our cost function takes in four parameters:

* our hypothesis function
* y-intercept choice
* slope choice
* `(x,y)` coordinates or `(month, beard length)`

For any given input point, our error is:

```markdown
(predicted value y' returned from our hypothesis function - actual value y)^2
```

We do this for every input point---square the difference between the actual value and the predicted value given by our hypothesis function.
Taking all of these squared prediction errors, we sum them up.
This sum is then divided by the total number of points.
In other words, we compute the _mean squared error_ (MSE).
This is our statistic for how far off (the quality of our estimator) our hypothesis function (and the choice of its parameters) are.

<blockquote>
An MSE of zero, meaning that the estimator predicts observations of the parameter θ with perfect accuracy,
is the ideal, but is typically not possible.
<footer>[Mean Squared Error - Wikipedia](https://en.wikipedia.org/wiki/Mean_squared_error)</footer>
</blockquote>

## Gradient Descent

So you have a way to make a straight line and you have a way to judge the quality of this line but the question still remains---which line?
Stumped, you go to the ringleader and ask for more time.

As dawn arrives, the entire troupe sets out for the next town.
The road is especially hilly.
"The grade is too steep!" the elephant handler yells out, "We need to wind around."
When you reach the bottom of a valley---it dawns on you---you just need to pick the y-intercept and slope that minimizes your
cost function the most.

Thinking back to your math classes, you remember that you've found--at the very least--a local minimum (or maximum)
when the derivative of a function equals zero since it is neither decreasing or increasing.
You decide to derive the partial derivatives of your cost function with respect to the y-intercept (`b`) and the slope (`m`).

```markdown
Partial Derivative of the Cost Function with respect to the y-intercept (b):

d/db = (1/2N)     * sum[(    y'       - y)^2 ]
     = (1/2N)     * sum[(    (mx + b) - y)^2 ] # Expand our Hypothesis Function
     = (1/2N)     * sum[     (mx + b  - y)^2 ]
     = (1/2N)     * sum[2 *  (mx + b  - y)   ] # Chain Rule -- x, m, and y are constants
     = (2 * 1/2N) * sum[     (mx + b  - y)   ] # Cancel Out the 2
     = (1/N)      * sum[     (mx + b  - y)   ]

Partial Derivative of the Cost Function with respect to the slope (m):

d/dm = (1/2N)     * sum[(   y'       - y)^2  ]
     = (1/2N)     * sum[(   (mx + b) - y)^2  ] # Expand our Hypothesis Function
     = (1/2N)     * sum[    (mx + b  - y)^2  ]
     = (1/2N)     * sum[2 * (mx + b  - y) * x] # Chain Rule -- x, b, and y are constants
     = (2 * 1/2N) * sum[    (mx + b  - y) * x] # Cancel Out the 2
     = (1/N)      * sum[    (mx + b  - y) * x]
```

The `2` in `(1/2N)` is added in some derivations.
The `m` and `b` that minimize `(1/N) * sum[(y' - y)^2]` will also minimize `(1/2N) * sum[(y' - y)^2]`.
You'll notice that it does simplify the derivatives.

With the two derivatives in hand, we can now look at using the gradient `<d/db, d/dm>`
of the cost function in our attempt to find its minimum.

<blockquote>
Consider a surface whose height above sea level at point (x, y) is H(x, y).
The gradient of H at a point is a vector pointing in the direction of the steepest slope or grade at that point.
The steepness of the slope at that point is given by the magnitude of the gradient vector.
<footer>[Gradient - Wikipedia](https://en.wikipedia.org/wiki/Mean_squared_error)</footer>
</blockquote>

Given a point on the cost function, the gradient points in the direction of the greatest increase.
Thus we want to head in the opposite direction of the gradient.
This will allow us to either reach a local minimum or the global minimum of our cost function.

```haskell
gradientForYIntercept :: Number -> Number -> Array (Array Number) -> Number
gradientForYIntercept yIntercept slope = gradient innerSum
  where
    innerSum :: Number -> Number -> Number
    innerSum x y = (yIntercept + slope * x) - y -- d/db = (1/N) * sum[(mx + b - y)]

gradientForSlope :: Number -> Number -> Array (Array Number) -> Number
gradientForSlope yIntercept slope = gradient innerSum
  where
    innerSum :: Number -> Number -> Number
    innerSum x y = ((yIntercept + slope * x) - y) * x -- d/dm = (1/N) * sum[(mx + b - y) * x]

gradient :: (Number -> Number -> Number) -> Array (Array Number) -> Number
gradient f values = (1.0 / size) * (sum <<< map innerSum) values
  where
    size :: Number
    size = (toNumber <<< length) values
    innerSum :: Array Number -> Number
    innerSum = arrayNumberHandler f
```

These functions make up the two derivatives listed above.
All together, they constitute the gradient of our cost function.
We can use these to find better and better (more optimal) y-intercepts and slopes.

![](/images/2017-01-15-linear-regression-and-the-amazing-beard/gradient_descent.svg){.post-img .post-img-small .post-img-limit}

As we travel along the cost function (looking for the global minimum),
descending against the gradient, we must decide on how big of next step we will take.
Take too big of a step and you may just overshoot the minimum.
Take too small of a step and it may take forever to reach the minimum.
This is known as the _learning rate_.

To keep it simple, you can maintain a constant learning rate throughout the duration of gradient descent.
However this may cause some problems depending on your cost function landscape.
In the [demo](https://github.com/lettier/interactive-simple-linear-regression/blob/master/src/Main.purs#L206)
we employ a technique known as [Bold Driver](http://www.willamette.edu/~gorr/classes/cs449/momrate.html).
Ideally we'd like to make leaps and bounds the farther away we are from the minimum until we get very near.
Once we start getting close, we want to take smaller and smaller steps until we slowly slide right into the minimum.

```haskell
newYIntercept :: Number -> Array (Array Number) -> Number -> Number -> Number
newYIntercept learningRate values oldYIntercept slope = newYIntercept'
  where
    gradientForYIntercept' :: Number
    gradientForYIntercept' = gradientForYIntercept oldYIntercept slope values
    newYIntercept' :: Number
    newYIntercept' = oldYIntercept - (learningRate * gradientForYIntercept')

newSlope :: Number -> Array (Array Number) -> Number -> Number -> Number
newSlope learningRate values yIntercept oldSlope = newSlope'
  where
    gradientForSlope' :: Number
    gradientForSlope' = gradientForSlope yIntercept oldSlope values
    newSlope' :: Number
    newSlope' = oldSlope - (learningRate * gradientForSlope')
```

These functions round out our gradient descent algorithm.
We take an initial slope and y-intercept,
compute its gradient component,
multiply the gradient component by the learning rate,
take this product minus the initial value,
and arrive at our new value.

To recap:

* Make some guess as to what the y-intercept and slope is
* Compute y-intercept's and slope's respective gradient component
* For each component, multiply it by the learning rate
* Subtract from the old value the learning rate multiplied by the gradient component
* Return this newly updated value
* Repeatedly feed the cost function the updated y-intercepts and slopes (along with the data points)
* Stop when you've either reached convergence (the global or a local minimum) or some threshold

Once we have (hopefully) reached the (possibly global) minimum of the cost function,
the most optimal line for modeling our data emerges.
In other words, we minimized the cost function as best we could by repeatedly using the gradient to update our
y-intercept and slope guesses.
The final y-intercept and slope can be plugged into our hypothesis function giving us our complete simple linear regression model.
It is here that we have _machine learned_ what the y-intercept and slope needs to be.

## Simple Linear Regression

```haskell
runLinearRegression :: Int -> Int -> Number -> Number -> Number -> Number -> Array (Array Number) -> Array Number
runLinearRegression _ _ _ _ yIntercept slope [] = [yIntercept, slope] -- if the values are empty, return what they passed
runLinearRegression -- otherwise perform Linear Regression
  currentIteration
  maxIterations
  maxCost
  learningRate
  yIntercept
  slope
  values
  =
    if cost' <= maxCost || currentIteration >= maxIterations
      then [newYIntercept', newSlope']
      else runLinearRegression (currentIteration + 1) maxIterations maxCost learningRate newYIntercept' newSlope' values
  where
    newYIntercept' = newYIntercept learningRate values yIntercept slope
    newSlope' = newSlope learningRate values yIntercept slope
    cost' = cost hypothesis newYIntercept' newSlope' values
```

This function puts it all together.
We start off with the
current iteration,
a cap on iterations,
a cost threshold,
the learning rate,
the initial y-intercept guess,
the initial slope guess,
and the x y coordinates.
If the cost is less than our threshold or if the current iteration is higher than the max, return the learned y-intercept and slope.
These you can feed into the hypothesis function, along with any x value, to output a y prediction.
If the cost is not less than our threshold and the current iteration is not higher than the max,
call this function again with an increased iteration count, the `newYIntercept`, and the `newSlope`.
We may never get our ideal cost (0.0), so we cap the number of iterations.
If we didn't, this function would continously call itself.
Note that the recursive call is at the _tail_. This way we won't reach the max stack depth.

```haskell
> import LinearRegression
> runLinearRegression 0 100000 1e-11 0.0005 0.0 0.0 [[0.0, 0.0], [1.0, 1.0], [2.0, 2.0], [3.0, 3.0], [4.0, 4.0], [5.0, 5.0]]
[0.000005599254920274064,0.9999984220684297]
```

This is how you would call `runLinearRegression`.
As a sanity check, we can pass it some points that are on the line `y = 1 * x + 0` (the y-intercept is zero and the slope is one).
As you can see, the result `y = 0.999998 * x + 0.000005` is very close to the actual line that generated the input points.

![](/images/2017-01-15-linear-regression-and-the-amazing-beard/found_y_intercept_slope.jpg){.post-img .post-img-small .post-img-limit}

As you make your way to town, you rush over to the ringmaster.
"I have my prediction," you say,
"Hans' beard will be 1.2 * 10 + 7.4 = 19.4 feet long next mont..."
However, before you could get another word out, the fire breather walks by, blurting out,
"There's a closed form solution you know. You didn't need to use gradient descent."

## Wrap-up

We discussed the details of building a simple linear regression model (from scratch)
using a fictitious story about predicting the length of a beard.
Along the way, we looked at how each step is implemented in the [interactive demo](http://lettier.com/linear-regression/)
built with PureScript, [PureScript-Halogen](https://github.com/slamdata/purescript-halogen), and [Chart.js](http://www.chartjs.org/).

Now that you've seen how simple linear regression via gradient descent works, take a look at some other machine learning algorithms such as
[k-Nearest Neighbors](/posts/2016-06-10-k-nearest-neighbors-from-scratch.html)
and [K-Means](/posts/2016-04-24-k-means-from-scratch.html).
