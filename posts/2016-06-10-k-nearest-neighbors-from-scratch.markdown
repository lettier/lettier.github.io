---
title: k-Nearest Neighbors from Scratch
jumbotron_image: /images/2016-06-10-k-nearest-neighbors-from-scratch/jumbotron_image.png
preview_image: /images/2016-06-10-k-nearest-neighbors-from-scratch/preview_image.png
description: Using JavaScript, we implement the k-Nearest Neighbors algorithm from the bottom up.
author: David Lettier
---
<!--https://pixabay.com/en/tulips-flowers-yellow-beautiful-15155/-->

# Demo and Codebase

If you would like to play with the k-Nearest Neighbors algorithm in your browser,
try out the visually interactive [demo](http://www.lettier.com/knearestneighbors).
All of the code for the demo is hosted on [GitHub](https://github.com/lettier/interactiveknn).
Stars are always appreciated.

# The Scenario

Say you have a garden that is host to many different kinds of plants.
Each plant's location in the garden is based on two of its features.
The west to east direction of the garden corresponds to the diameter of the plant's flower
while the south to north direction relates to the length of the plant's leaf.
Each plant in the garden has been carefully labeled with a small tag stuck in the dirt located near its base.

![Your garden.](/images/2016-06-10-k-nearest-neighbors-from-scratch/garden.jpg){.post-img .post-img-fill}

Recently you ordered a large truck load of plants in order to fill out your garden.
However, after having received all of these new plants, you find none are labeled.
You quickly measure each plant's flower diameter and leaf length.
With these two numbers per plant, you place them in your garden at their appropriate coordinates.

The only problem is, none of these new plants have labels and they look similar to two or more nearby existing plants.
You cannot just leave them unlabeled and so you resort to using the k-Nearest Neighbors classification algorithm.

# N by U Distances

The first thing you do is figure out how far away each unlabeled plant is to each labeled plant.
After drawing out a large table on your notepad--where the columns are the unknown plants and the rows are the
known plants--you fill in each cell with the euclidean distance.
For example, the distance between `U1` (unknown) and `N1` (known).

```javascript
     |====|====|====|====|====
     | U1 | U2 | U3 | U4 | ...
|====|----|----|----|----|----
| N1 |  3 |  1 |  3 |  6 | ...
|----|----|----|----|----|----
| N2 |
|----|
| N3 |       ...
|----|
| .  |
  .
  .
```

In the interactive demo, the code looks like this:

```javascript
KNN.prototype._distances = function () {
  this.currentDot.distances = fjs.map(
    function (knownDot) {
      return [
        this.distanceCalculator.distance(
          knownDot,
          this.currentDot
        ),
        knownDot
      ];
    }.bind(this),
    this.knownDots
  );
};
```

We map over all of the known dots with a function that takes a known dot and returns the distance between it and the unknown current dot.
What we end up with is an array--the same size as the known dots array--where each element is a distance.

```javascript
KNN.prototype._setUpCycle = function () {
  this.currentDot = fjs.first(
    "x => true",
    this.unknownDots
  );

  if (!this.currentDot) {return false;}

  this.unknownDots = fjs.select(
    "x => x.id !== " + this.currentDot.id,
    this.unknownDots
  );

  return true;
};
```

We do this for each unknown dot until we know the distances between each unknown dot and known dots.
It is here that we are setting up the next "column" where we get the next unknown dot and set it as the current dot.

# Choosing K

At this point you run into a bit of a problem--you are not sure what to choose for k.

<blockquote>
The best choice of k depends upon the data; generally, larger values of k reduce the effect of
noise on the classification, but make boundaries between classes less distinct.
A good k can be selected by various heuristic techniques (see hyperparameter optimization).
The special case where the class is predicted to be the class of the closest training sample
(i.e. when k = 1) is called the nearest neighbor algorithm.
<footer>[k-Nearest Neighbors, Wikipedia](https://en.wikipedia.org/wiki/K-nearest_neighbors_algorithm)</footer>
</blockquote>

If you choose k to be the number of all known plants, then each unknown plant will just be labeled with the most frequent (the mode) label
in your garden.
And obviously, if you set k to zero, then no unknown plant gets labeled.
Since the sun is going down soon you decide to go with three and move on.

```javascript
GUI.prototype.init = function (params) {
  var application = params.application;

  this.datGUI = new dat.GUI();
  this.datGUI.add(application.kNN, 'k').min(1).max(20).step(1).listen();
  // ...
};
```

In the demo we do not have this problem as we let the user select `k` between one and 20.

# Sorting the Distances

Now that you know how far each unknown to known plant is, you tediously make a copy of each U column.
Before you copy down each U column, you sort its rows from smallest to largest.

```javascript
     |====|      |====|      |====|          |====|
     | U1 |      | U2 |      | U3 |          | U* |
|====|====| |====|====| |====|====|     |====|====|
| N1 |  3 | | N7 |  1 | | N8 |  3 |     | N* |  * |
|----|----| |----|----| |----|----|     |----|----|
| N2 |  6 | | N1 |  5 | | N9 |  9 | ... | N* |  * |
|----|----| |----|----| |----|----|     |----|----|
   . |  . | |  . |  . | |  . |  . |     |  . |  . |
   . |  . | |  . |  . | |  . |  . |     |  . |  . |
   . |  . | |  . |  . | |  . |  . |     |  . |  . |
```

```javascript
KNN.prototype._sortDistances = function () {
  this.currentDot.distances.sort(function (a, b) {
    return a[0] - b[0];
  });
};
```

For each current unknown dot, we sort the distances in ascending order.

# The Nearest Neighbors

The distances have been calculated and, for each unknown plant, you sorted the distances from smallest to largest.
Looking at each individual unknown plant's list of distances,
you now take the first k distances (remember you decided on k being three).
These k distances are the k-Nearest Neighbors for any given U unknown plant.

![Selecting the three k-Nearest Neighbors.](/images/2016-06-10-k-nearest-neighbors-from-scratch/knn_flower_pots.svg){.post-img .post-img-fill}

```javascript
KNN.prototype._kNearestNeighbors = function () {
  this.currentDot.kNearestNeighbors = this.currentDot.distances.slice(
    0,
    this.k
  );
};
```

After we have sorted the distances, we merely slice out the `k` smallest.
These become the `currentDot`'s k-Nearest Neighbors.

# Adding up the Votes

Each unknown plant now has its k-Nearest Neighbors but you still have to determine the plant type.
To do this you count up how many k-Nearest Neighbors or known plants fall into each unique plant type.
In other words, among U unknown plant's k-Nearest Neighbors, you are looking for the [mode](https://www.mathsisfun.com/mode.html).

For example, take your `U1` list:

```javascript
|============|    |====|
| Type       |    | U1 |
|============|====|====| --
| Raspberry  | N1 |  3 |  |
|------------|----|----|  |
| Raspberry  | N2 |  6 |  | k-Nearest Neighbors
|------------|----|----|  |
| Blackberry | N4 |  7 |  |
|------------|----|----| --
| Strawberry | N7 |  8 |
|------------|----|----|
| Blackberry | N9 |  9 |
|------------|----|----|
     .         .    .
     .         .    .
     .         .    .
```

Looking at the first three distances we have: `Raspberry: 2` and `Blackberry: 1`.
Here the keys are `Raspberry` and `Blackberry` and the values are `2` and `1` respectively.

```javascript
KNN.prototype._classsByVote = function () {
  var dot = null;
  var classs = null;
  var classsCount = {};

  fjs.each(function (kNearestNeighbor) {
    var classs = kNearestNeighbor[1].classs;

    classsCount[classs] = classsCount[classs] || 0;
    classsCount[classs] += 1;
  }.bind(this), this.currentDot.kNearestNeighbors);

  // ...
```

Going through each `kNearestNeighbors` of the `currentDot`, we count how many times we see a unique `classs`
(the unique colors of the dots in the left sidebar).

# The Most Votes Win

Taking these type counts, you sort them from largest to smallest.
To determine the type of the unknown plant, you take the most counted type.
For the case of `U1`, you conclude that it is a `Raspberry` plant.
After doing this for each unknown plant, you can finally label each.

```javascript
  // ...

  classs = fjs.first(
    "x => true",
    fjs.best(function (a, b) {
      return a[1] > b[1];
    }.bind(this), fjs.toArray(classsCount))
  );

  dot = this.currentDot;

  PubSub.publish(
    "classified",
    {
      dot: dot,
      classs: classs
    }
  );
};
```

Taking the `classsCount` object (`{yellow: 3, blue: 2, ...}`), we turn it into an array of arrays
(`[["yellow", 3], ` `["blue", 2], ...]`).
To determine the `classs` (color type) of the current dot, we find the `best`/max or largest count in the array of arrays.
The `best` function will return say `["yellow", 3]` and the `first` function will return just `"yellow"`.
With the current dot classified, we publish a `classified` message that will be consumed elsewhere in the demo.

# Putting it all Together

```javascript
KNN.prototype._cycle = function () {
  this._distances();
  this._sortDistances();
  this._kNearestNeighbors();
  this._classsByVote();

  if (this.unknownDots.length > 0) {
    if (this._setUpCycle()) {
      window.requestAnimationFrame(this._cycle.bind(this));
    }
  }
};
```

The first step is to compute all of the needed distances between classified and unclassified data points.
Next, for each unclassified data point, you sort its distances in ascending order.
These sorted distances from 0 up to but not including k become the unclassified data point's k-Nearest Neighbors.
Looking at only a data point's k-Nearest Neighbors, you count how many times you see each known class type.
Once counted, you sort the class types by their counts and take the class type with the biggest count.
This most-seen-class type becomes the class type for your unclassified data point.


# Wrap-up

We discussed the details of the k-Nearest Neighbors algorithm using a fictitious story about classifying unknown plants.
Along the way we looked at how each step is implemented in the [interactive demo](http://www.lettier.com/knearestneighbors/).

Now that you have seen how a supervised classification algorithm works, take a look at
[K-Means from Scratch](/posts/2016-04-24-k-means-from-scratch.html)--an unsupervised clustering method for unlabeled data.
