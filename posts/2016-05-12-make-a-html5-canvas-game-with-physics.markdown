---
title: Make a HTML5 Canvas Game with Physics
jumbotron_image: /images/2016-05-12-make-a-html5-canvas-game-with-physics/jumbotron_image.jpg
preview_image: /images/2016-05-12-make-a-html5-canvas-game-with-physics/preview_image.jpg
description: Using FuntionalJS, PhysicsJS, PubSubJS, and EaselJS, we develop a HTML5 canvas game called Dubul Rubul.
author: David Lettier
---

# Dubul Rubul

![](/images/2016-05-12-make-a-html5-canvas-game-with-physics/dubulrubul.gif){.post-img .post-img-small .post-img-limit}

The game we will be building is inspired by [Breakout](https://en.wikipedia.org/wiki/Breakout_%28video_game%29).
There are major differences. The blocks move realistically and the player competes opposite side of the computer to smash
the most blocks.
Certain blocks give more points then others and hitting the walls or the other player's ball gives a point the other side.

The entire project can be found on [GitHub](https://www.github.com/lettier/dubulrubul) and
a playable version is hosted on [Lettier.com](http://www.lettier.com/projects/dubulrubul).
If for any reason the hosted version is down, you can clone the repository, `cd` into `dist`, and open `index.html` in your browser.

```bash
git clone git@github.com:lettier/dubulrubul.git
cd dubulrubul/dist
xdg-open index.html # Or open on Mac OS X.
```

If you find the game enjoyable, feel free to <i class="fa fa-star" aria-hidden="true"></i>
the project on [GitHub](https://github.com/lettier/dubulrubul/stargazers).

## Dependencies

To get started, will we need to acquire our needed dependencies:

* [PhysicsJS](http://wellcaffeinated.net/PhysicsJS/) - Models the world gravity and all of the objects' mass, velocity,
surface friction, and collisions inside the game universe
* [FunctionalJS](http://functionaljs.com/) - Useful for mapping, reducing, and filtering objects as well as currying,
composing, and creating anonymous functions
* [EaselJS](http://www.createjs.com/easeljs) - Renders all of the onscreen elements to our canvas
* [PubSubJS](https://github.com/mroderick/PubSubJS) - Provides asynchronous message passing between the components

We will also need [NVM](https://github.com/creationix/nvm). Make sure to install a version of Node `>= v4.0.0`.

## Components

Dubul Rubul consists of multiple components that handle the various functionalities of the game.

* `application.js` - All of the initialization, update, and reset logic
* `ball.js` - The ball model keeping track of the physics, dimensions, and the color
* `block.js` - Similar to the ball model but for the blocks onscreen
* `canvas.js` - All of the draw and physics logic governing over the `CanvasElement`s onscreen
* `canvasElement.js` - The parent model which `Ball`, `Block`, and `Paddle` inherit from
* `computerPaddleControls.js` - The logic controlling the computer's paddle seen to the left
* `init.js` - Creates a new `Application` and hides the title screen
* `paddle.js` - The paddle model keeping track of the paddle's physics, shape, and color
* `paddleControls.js` - The parent object to `ComputerPaddleControls` and `PlayerPaddleControls`
* `playerPaddleControls.js` - The logic monitoring the player's input and updating their onscreen paddle
* `referee.js` - Handles the business logic of what actions receive what points
* `scoreBoard.js` - Updates and resets the onscreen scores displayed at the top of the screen
* `update.js` - The main game loop
* `util.js` - Various helper methods DRY-ing up the code base

## Game Arena

![The game arena with scoreboard.](/images/2016-05-12-make-a-html5-canvas-game-with-physics/canvas.jpg){.post-img .post-img-fill}

We will place [two document divisions](https://github.com/lettier/dubulrubul/blob/master/src/index.html#L45)
at the top of the screen to display the computer's and player's scores.
The other parts of the arena are the two canvases.
The [first canvas](https://github.com/lettier/dubulrubul/blob/master/src/index.html#L41)
is used to render/draw/view the physics calculations. This is useful for debugging but is not normally shown.
The [second canvas](https://github.com/lettier/dubulrubul/blob/master/src/index.html#L43)
displays all of the `canvasElement`s or the game's entities.
Upon initialization, the canvases are re-sized to fit the entire screen.
If the window is ever re-sized, the game arena's size is [updated](https://github.com/lettier/dubulrubul/blob/master/src/js/application.js#L69)
to match.

![The physics canvas.](/images/2016-05-12-make-a-html5-canvas-game-with-physics/physics_canvas.jpg){.post-img .post-img-fill}

## Canvas Elements

The game consists of three models: the [ball](https://github.com/lettier/dubulrubul/blob/master/src/js/ball.js),
the [block](https://github.com/lettier/dubulrubul/blob/master/src/js/block.js),
and the [paddle](https://github.com/lettier/dubulrubul/blob/master/src/js/paddle.js).
To avoid duplication of common properties, they all inherit from the
[CanvasElement](https://github.com/lettier/dubulrubul/blob/master/src/js/canvasElement.js)
object.


### Ball

The ball has two constants: `HEIGHT` and `WIDTH`.
It has a `x` and `y` [2D position](https://github.com/lettier/dubulrubul/blob/master/src/js/canvasElement.js#L12)
in pixels and a [rotation](https://github.com/lettier/dubulrubul/blob/master/src/js/canvasElement.js#L15) in radians.
Since the ball is uniform in shape and color, the rotation cannot be seen unless the physics are drawn.

### Block

The block is similar to the ball but defines logic around
[prizes](https://github.com/lettier/dubulrubul/blob/master/src/js/application.js#L157).
Prizes are blocks that are colored corresponding to either the left or right side.
If one side smashes a prize block with its color, it gets extra points.
If another side smashes a prize block for the opposite side, the opposite gets the extra points.
The goal for the player is to avoid the computer's prize blocks, hit its own prize blocks, and hit as many neutral gray
blocks as they can.

### Paddle

The paddles are used to hit the ball back and forth across the arena.
They can spin, helping to aim or deflect the ball where the player or computer would like them to go.
To spin them, the player moves their mouse over the screen from left to right.

![](/images/2016-05-12-make-a-html5-canvas-game-with-physics/paddle_action.gif){.post-img .post-img-small .post-img-limit}

## Initialization

Everything starts with the [init](https://github.com/lettier/dubulrubul/blob/master/src/js/init.js#L6)
function which is called once the player presses the start button.
This creates a new application and calls its `init` function.
Once the application initializes, it hides the title [overlay](https://github.com/lettier/dubulrubul/blob/master/src/js/init.js#L17).

The application initialization setups all of the game components.
It creates the canvas, paddles, balls, and blocks.
For each object created, it calls the object's `init` function.

![The dynamic block grid.](/images/2016-05-12-make-a-html5-canvas-game-with-physics/block_grid.jpg){.post-img .post-img-fill}

The blocks are [dynamically created](https://github.com/lettier/dubulrubul/blob/master/src/js/application.js#L145).
They stretch from the top to bottom of the screen and take up about 15% of the center screen real estate.
Once they are hit by the balls, the blocks topple over.

With all of the objects setup, the application wires up all of the asynchronous events and `PubSubJS` messages.
When another object publishes the [game over](https://github.com/lettier/dubulrubul/blob/master/src/js/application.js#L60)
message, it calls the `reset` function.
The major events it listens for are `resize` and `keyup`.

## The Game Loop

![The game loop.](/images/2016-05-12-make-a-html5-canvas-game-with-physics/game_loop.png){.post-img .post-img-fill}

The [game loop](https://github.com/lettier/dubulrubul/blob/master/src/js/application.js#L185)
is repeatedly called until a round is over.
It is responsible for getting the graphics and physics to update with each new requested animation frame.

We would like the game animation to play at roughly the same speed no matter what hardware the game is running on.
We'll use [Time-based Animation](https://www.viget.com/articles/time-based-animation) to keep the animation fairly consistent
across different machines. To do this, we calculate [how long](https://github.com/lettier/dubulrubul/blob/master/src/js/application.js#L205)
it took to get back to updating the game loop.
By passing this `delta` scalar into `canvas.update`, any position or rotation update can be scaled by how
long it took since `delta` was last calculated.

If it is taking no time at all to get back to the `update` function, the changes in position or rotation will be small.
However, if it is taking forever to call `update` again and again, each change will be scaled up immensely.
Another way to think about it is a [flip book](https://en.wikipedia.org/wiki/Flip_book).
Picture two flip books where each is an animation of a person running from the left to the right.
For the fast flippers, you would give them the flip book with more pages where for each page, the person is updated ever so slightly.
For the slow flippers, you would give them the flip book with less pages where each page updates the person at a greater clip.
The fast flippers have more pages to get through while the slow flippers have less.
If both start at the same time, the animated person running between the two flip books will match.

![The two flip books.](/images/2016-05-12-make-a-html5-canvas-game-with-physics/flip_book.png){.post-img .post-img-fill}

## The Canvas

The `Canvas` object has two main concerns.
The first is abstracting the adding, updating, and removal of the game objects for both `EaselJS` and `PhysicsJS`.
The second major concern is keeping the
[Stage](https://github.com/lettier/dubulrubul/blob/master/src/js/canvas.js#L32)
and
[world](https://github.com/lettier/dubulrubul/blob/master/src/js/canvas.js#L67)
objects in
[sync](https://github.com/lettier/dubulrubul/blob/master/src/js/canvas.js#L493).

For every iteration of [update](https://github.com/lettier/dubulrubul/blob/master/src/js/canvas.js#L126),
the `Canvas` performs three steps.
Some `canvasElement`s are not fully controlled by the physics simulation.
For these objects, their x-y coordinates and/or rotation are
[updated](https://github.com/lettier/dubulrubul/blob/master/src/js/canvas.js#L473)
via their `world` `body` object.
After updating the physics simulation, the `world` is allowed to advanced one
[step](https://github.com/lettier/dubulrubul/blob/master/src/js/canvas.js#L138).
This updates all of the physical
[bodies](https://github.com/lettier/dubulrubul/blob/master/src/js/canvas.js#L525) allowing us
to
[update](https://github.com/lettier/dubulrubul/blob/master/src/js/canvas.js#L508)
all of the `Stage` objects ultimately resulting in the animation you see in the game.
With everything updated, we can
[begin](https://github.com/lettier/dubulrubul/blob/master/src/js/canvas.js#L148)
the render process that draws the shapes on the screen.

The other `Canvas` concerns are
[listening](https://github.com/lettier/dubulrubul/blob/master/src/js/canvas.js#L44)
for and
[publishing](https://github.com/lettier/dubulrubul/blob/master/src/js/canvas.js#L570)
certain events.
These events are consumed or broadcast using `PubSubJS`.
The events listened for are to update the paddles or blocks and to
[shake](https://github.com/lettier/dubulrubul/blob/master/src/js/canvas.js#L607)
the canvas. When received, the HTML canvas is moved up and down every so slightly to give
the illusion of the game arena being rattled.
The broadcast messages are for the various types of
[collisions](https://github.com/lettier/dubulrubul/blob/master/src/js/canvas.js#L541).
These will be consumed by the `Referee` which in turn will determine the computer's or player's score update.

## Paddle Controls

The paddle controls allow the player and computer to move their paddle up or down and to rotate it in place.

### Computer

This is where we define the AI for the computer paddle.
For now the logic is extremely basic--it merely follows the player's ball.
In a later post, we will revisit with a machine learning approach.

### Player

The player can
[rotate](https://github.com/lettier/dubulrubul/blob/master/src/js/playerPaddleControls.js#L24)
their paddle by moving their mouse from side to side.
Moving the mouse
[up or down](https://github.com/lettier/dubulrubul/blob/master/src/js/playerPaddleControls.js#L15)
will also move their paddle up or down.

## Scoring

Most games have some scoring mechanism and Dubul Rubul is no different.

### Scoreboard

The `Scoreboard` object maintains both the green and blue [div](https://github.com/lettier/dubulrubul/blob/master/src/js/scoreboard.js#L14)
bars at the top of the screen as well as the player's and computer's onscreen
[score counts](https://github.com/lettier/dubulrubul/blob/master/src/js/scoreboard.js#L85).
Typically the `Referee` will publish a `updateScore` message which will be
[consumed](https://github.com/lettier/dubulrubul/blob/master/src/js/scoreboard.js#L22)
by the `Scoreboard` object. Once it receives this message, it
[updates](https://github.com/lettier/dubulrubul/blob/master/src/js/scoreboard.js#L56)
either the computer's or player's current score.

### Referee

The `Referee` object handles most of the [Business Logic](https://en.wikipedia.org/wiki/Business_logic) or gameplay mechanics.
It listens for the
[noMoreBlocks](https://github.com/lettier/dubulrubul/blob/master/src/js/referee.js#L32)
message. Once received, it will publish the `gameOver` message.
This `gameOver` message will be picked up by the `Application` object,
[resetting](https://github.com/lettier/dubulrubul/blob/master/src/js/application.js#L224) the game.

The [blockBallHit](https://github.com/lettier/dubulrubul/blob/master/src/js/referee.js#L44)
collision, published by the `Canvas` object, translates into a point scheme.
When the player loses points to the computer, the `Referee` publishes the `shakeCanvas`
[message](https://github.com/lettier/dubulrubul/blob/master/src/js/referee.js#L61).
This provides a visual cue to the player that they are missing out on points.

The computer or player can lose points to the opposite side if they
[touch](https://github.com/lettier/dubulrubul/blob/master/src/js/referee.js#L78)
the other side's ball with their own paddle.
If the ball
[touches](https://github.com/lettier/dubulrubul/blob/master/src/js/referee.js#L96)
the bounding box, the opposite side earns a point.

# Wrap-up

Using
[functional programming](https://github.com/lettier/dubulrubul/blob/master/src/js/canvas.js#L544),
a HTML5 canvas, a physics simulation, and a publish subscribe message model,
we built Dubul Rubul--a video game playable in any modern browser. In future posts we will revisit the computer
paddle's AI and look at adding sound effects for added immersion.
