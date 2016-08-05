---
title: Matthews Correlation Coefficient
jumbotron_image: /images/2016-08-05-matthews-correlation-coefficient/jumbotron_image.jpg
preview_image: /images/2016-08-05-matthews-correlation-coefficient/preview_image.jpg
description: Using a fictitious scenario, we explore the Matthews Correlation Coefficient quality measure of binary classification.
author: David Lettier
---
<!--https://pixabay.com/en/eggs-bowl-easter-holiday-1278166/-->

# The Scenario

You've recently acquired a job at your local egg processing plant.
This plant takes in large deliveries of eggs sourced from local farms.
Your job, near the end of the processing pipeline, is to pluck the rotten eggs off of the conveyor belt.
Most of the eggs are fine but a certain few are rotten to the yolk.

![Picture this but with eggs.](/images/2016-08-05-matthews-correlation-coefficient/factory_worker.jpg){.post-img .post-img-limit}

<!--https://pixabay.com/en/produce-food-canning-processing-448536/-->

Since you are new to the job, they have put you on a probationary period to determine how well you spot the rotten eggs.
Further down the conveyor belt is a senior employee who is perfect at spotting rotten eggs.
They will be able to catch any rotten eggs that you miss.

# Matthews Correlation Coefficient

The Matthews Correlation Coefficient (MCC) has a range of -1 to 1 where -1 indicates a completely wrong binary classifier while
1 indicates a completely correct binary classifier.
Using the MCC allows one to gauge how well their classification model/function is performing.
Another method for evaluating classifiers is known as the
[ROC curve](/posts/2016-03-28-reelin-and-rocin-receiver-operating-characteristic.html).

<blockquote>
[MCC] takes into account true and false positives and negatives and is generally
regarded as a balanced measure which can be used even if the classes are of very different sizes.
<footer>[Matthews correlation coefficient, Wikipedia](https://en.wikipedia.org/wiki/Matthews_correlation_coefficient)</footer>
</blockquote>

The MCC formula is:

```javascript
                |  Said Is                 | Said Is Not                 ||
                |==========================|=============================||=================================|
    Actually Is | True Positive  (TP)      | False Negative    (FN)      || Total Actually Is     (TP + FN) |
                |--------------------------|-----------------------------||---------------------------------|
Actually Is Not | False Positive (FP)      | True Negative     (TN)      || Total Actually Is Not (FP + TN) |
                |==========================|=============================||=================================|
                | Total Said is  (TP + FP) | Total Said Is Not (FN + TN) ||

      TP * TN - FP * FN
MCC = -----------------------------------------------------
      [(TP + FP) * (FN + TN) * (FP + TN) * (TP + FN)]^(1/2)
```

You should notice that the numerator consists of just the four inner cells (crisscross style)
while the denominator consists of the four outer cells (the totals) of the confusion matrix (table).

![Prediction versus reality.](/images/2016-08-05-matthews-correlation-coefficient/eggs.svg){.post-img .post-img-limit}

# Evaluation

Upper management has decided on using the MCC to rate your rotten egg spotting abilities since the two classes (rotten, not rotten) are
not evenly balance given any sample of the conveyor belt.

## All Negative

You are feeling lazy so you decide to say that any egg that passes by is fine (not rotten).

![An all negative prediction.](/images/2016-08-05-matthews-correlation-coefficient/eggs_all_negative.svg){.post-img .post-img-limit}

Once they looked at all of the eggs, there were a total of 327 actual not rotten eggs and a total of 24 actual rotten eggs
for a total of `327 + 24 = 351` eggs decided on.
In other words, there were 24 positive ones (rotten) and 327 negative ones (not rotten).

```javascript
                    | Said Rotten | Said Not Rotten ||
                    |================================================|
    Actually Rotten | 0 (TP)      |  24 (FN)        ||  24 (TP + FN) |
                    |-------------------------------||---------------|
Actually Not Rotten | 0 (FP)      | 327 (TN)        || 327 (FP + TN) |
                    |================================================|
                    | 0 (TP + FP) | 351 (FN + TN)   || 351

            0 * 327 - 0 * 24
MCC = 0.0 = --------------------------
            (0 * 351 * 327 * 24)^(1/2)
```

Your score is actually undefined since the denominator is zero but since the numerator is zero we will call it zero.
Since your score was zero they decide to give you another shot.

Too bad they didn't look at say accuracy.

```javascript
accuracy = (TP + TN) / Total = (0 + 327) / 351 = .932
```

A great score for not even trying!

## All Positive

Today is Friday and you cannot be bothered so you decide to say that any egg that passes by is rotten.

![An all positive prediction.](/images/2016-08-05-matthews-correlation-coefficient/eggs_all_positive.svg){.post-img .post-img-limit}

Again, there were 24 positive ones (rotten) and 327 negative ones (not rotten).

```javascript
                    | Said Rotten   | Said Not Rotten ||
                    |==================================================|
    Actually Rotten |  24 (TP)      | 0 (FN)          ||  24 (TP + FN) |
                    |---------------------------------||---------------|
Actually Not Rotten | 327 (FP)      | 0 (TN)          || 327 (FP + TN) |
                    |==================================================|
                    | 351 (TP + FP) | 0 (FN + TN)     || 351

            24 * 0 - 327 * 0
MCC = 0.0 = --------------------------
            (351 * 0 * 327 * 24)^(1/2)
```

Your score is still zero and even though they would have lost all of those 351 eggs (you _did_ say they were all rotten),
they give you yet another chance.

If only they had looked at recall instead--you might have been promoted.

```javascript
recall = TP / (TP + FN) = 24 / (24 + 0) = 1.0
```

Perfect recall even though you wasted 327 perfectly good eggs.

## All Correct

Something is in the air and after work you plan on playing the lottery but today you feel extremely precise.
Despite all the odds, you managed to correctly classify/label/say what each egg actually was.

![An all correct prediction.](/images/2016-08-05-matthews-correlation-coefficient/eggs_all_correct.svg){.post-img .post-img-limit}

Staying consistent, there were 24 positive ones (rotten) and 327 negative ones (not rotten).

```javascript
                    | Said Rotten   | Said Not Rotten ||
                    |==================================================|
    Actually Rotten |  24 (TP)      |   0 (FN)        ||  24 (TP + FN) |
                    |---------------------------------||---------------|
Actually Not Rotten |   0 (FP)      | 327 (TN)        || 327 (FP + TN) |
                    |==================================================|
                    |  24 (TP + FP) | 327 (FN + TN)   || 351

            24 * 327 - 0 * 0
MCC = 1.0 = ---------------------------
            (24 * 327 * 327 * 24)^(1/2)
```

It seems that giving you the two previous passes has paid off since your score is 1.
However, management deems it a fluke and would like to evaluate you one more time.

## All Wrong

All of your lotto tickets didn't pay off and today is just not your day.
On top of spilling your coffee all over the factory floor you managed to get every classification backwards.
If the egg was rotten you said not rotten and if the egg was not rotten you said it was rotten.

![An all incorrect prediction.](/images/2016-08-05-matthews-correlation-coefficient/eggs_all_incorrect.svg){.post-img .post-img-limit}

Staying true to the scenarios, there were 24 positive ones (rotten) and 327 negative ones (not rotten).

```javascript
                    | Said Rotten   | Said Not Rotten ||
                    |===================================================|
    Actually Rotten |   0 (TP)      | 24 (FN)         ||  24 (TP + FN)  |
                    |---------------------------------||----------------|
Actually Not Rotten | 327 (FP)      |  0 (TN)         || 327 (FP + TN)  |
                    |===================================================|
                    | 327 (TP + FP) | 24 (FN + TN)    || 351

             0 * 0 - 327 * 24
MCC = -1.0 = ---------------------------
             (327 * 24 * 327 * 24)^(1/2)
```

One of the managers yells out, "I knew it. Fired!"
It seems that your previous score of 1 was indeed a fluke.
Your new score is the absolute worst at -1.
As they walk you out the door, you try explaining that they could just flip whatever
you say and they'd get a perfect worker but their minds are already made up.

# Wrap-up

Using a fictitious story about egg classification, we explored the edge cases of the Matthews Correlation Coefficient.
For the cases where we labeled every egg rotten or every egg not rotten (regardless of their actual class),
we compared the MCC score to other well known evaluation metrics (accuracy and recall).
In these particular cases, the other metrics indicated a much more positive result than the MCC score showed.
