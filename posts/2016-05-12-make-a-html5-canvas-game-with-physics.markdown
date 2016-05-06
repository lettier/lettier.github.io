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
There are major differences. The blocks move and the player competes opposite side of the computer to smash
the most blocks. Certain blocks give more points then others and hitting the walls takes a point.

The entire project can be found on [GitHub](https://www.github.com/lettier/dubulrubul) and
a playable version is hosted on [Lettier.com](http://www.lettier.com/projects/dubulrubul).
If for any reason the hosted version is down, you can clone the repository, `cd` into `dist`, and open `index.html` in your browser.

```bash
git clone git@github.com:lettier/dubulrubul.git
cd dubulrubul/dist
xdg-open index.html # Or open on Mac OS X.
```

If you find the game enjoyable, feel free to <i class="fa fa-star" aria-hidden="true"></i> the project on [GitHub](https://github.com/lettier/dubulrubul/stargazers).

## Dependencies

To get started, will we need to acquire our needed dependencies:

* [PhysicsJS](http://wellcaffeinated.net/PhysicsJS/) - Models the world gravity and all of the objects' mass, velocity, surface friction, and collisions inside the game universe
* [FunctionalJS](http://functionaljs.com/) - Useful for mapping, reducing, and filtering objects as well as currying, composing, and creating anonymous functions
* [EaselJS](http://www.createjs.com/easeljs) - Renders all of the onscreen elements to our canvas
* [PubSubJS](https://github.com/mroderick/PubSubJS) - Provides asynchronous message passing between the components

We will also need [NVM](https://github.com/creationix/nvm). Make sure to install a version of Node `>= v4.0.0`.

## Components

Dubul Rubul consists of multiple components that handle the various functionalities of the game.

* `ball.js` - The ball model keeping track of the physics, dimensions, and the color
* `block.js` - Similar to the ball model but for the blocks onscreen
* `canvas.js` - All of the draw and physics logic governing over the canvas elements onscreen
* `canvasElement.js` - The parent model which ball and block inherit from
* `computerPaddleControls.js` - The logic controlling the computer's paddle seen to the left
* `init.js` - All of the initialization logic setting up each game component
* `paddle.js` - The paddle model keeping track of the paddle's physics, shape, and color
* `paddleControls.js` - The parent object to `computerPaddleControls` and `playerPaddleControls`
* `playerPaddleControls.js` - The logic monitoring the player's input and updating their onscreen paddle
* `referee.js` - Handles the business logic of what actions receive what points
* `scoreBoard.js` - Updates and resets the onscreen scores displayed at the top of the screen
* `update.js` - The main game loop
* `util.js` - Various helper methods DRY-ing up the code base

## The Arena

```html
      <canvas id="physics" width="500" height="500">
      </canvas>
      <canvas id="canvas" width="500" height="500">
      </canvas>
      <div id="scoreLeft" class="scoreBase scoreLeft"></div>
      <div id="scoreRight" class="scoreBase scoreRight"></div>
```

We will place two document divisions at the top of the screen to display the computer's and player's scores.
The first canvas is used to view the physics calculations. This is useful for debugging but not shown normally.
The second canvas displays all of the `canvasElement`s.
Upon initialization, the canvases are re-sized to fit the entire screen.

## Canvas Elements

The game consists of three models: the ball, the block, and the paddle.

### Ball

```javascript
/*
  David Lettier (C) 2016
  http://www.lettier.com/
*/

function Ball() {}

// Inherit from the CanvasElement object.
Ball.prototype = Object.create(CanvasElement.prototype);
// Set the constructor though to Ball.
Ball.prototype.constructor = Ball;

Ball.prototype.WIDTH = (function () {
  // Get the height from the hidden ballLeft element.
  var attr = getcomputedStyleAttr("ballLeft", "width");

  // The style attribute has "px" so remove it
  // before parsing the string into an integer.
  return parseInt(attr.replace("px", ""), 10);
})();

Ball.prototype.HEIGHT = (function () {
  // Get the height from the hidden ballLeft element.
  var attr = getcomputedStyleAttr("ballLeft", "height");

  // The style attribute has "px" so remove it
  // before parsing the string into an integer.
  return parseInt(attr.replace("px", ""), 10);
})();
```

The ball has two constants: `HEIGHT` and `WIDTH`.
It also keeps its 2D position and rotation in radians.

### Block

```javascript
/*
  David Lettier (C) 2016
  http://www.lettier.com/
*/

function Block() {}

// Inherits from the CanvasElement object.
Block.prototype = Object.create(CanvasElement.prototype);

// Set the constructor to block.
Block.prototype.constructor = Block;

Block.prototype.WIDTH = (function () {
  // This allows the block width to be set
  // via CSS.
  var attr = getcomputedStyleAttr("blockBase", "width");

  return parseInt(attr.replace("px", ""), 10);
})();

Block.prototype.HEIGHT = (function () {
  // This allows the block height to be set
  // via CSS.
  var attr = getcomputedStyleAttr("blockBase", "height");

  return parseInt(attr.replace("px", ""), 10);
})();

Block.prototype.COLOR = (function () {
  // This allows the block color to be set
  // via CSS.
  return getcomputedStyleAttr("blockBase", "color");
})();

Block.prototype.init = function (params) {
  // Call CanvasElement's init first.
  CanvasElement.prototype.init.call(this, params);

  // Then set prizeFor and deleted which
  // are specific to just blocks.

  // The prizeFor can be for the left or right side.
  // If this block is a prize, it will give extra
  // points to the same side and no points for the
  // other side not matter what ball smashes the block.
  //
  // For example, say prizeFor is right.
  // A right ball smashes the block. The player side
  // gets the points. Now say a left ball smashes the
  // block. The player side still gets the points.
  this.prizeFor = params.prizeFor || null;
  this.deleted = false;
};

Block.prototype.isPrize = function () {
  // If the prizeFor is null, it is not a prize.
  return this.prizeFor !== null;
};

Block.prototype.isPrizeForRight = function () {
  return this.isPrizeFor("right");
};

Block.prototype.isPrizeForLeft = function () {
  return this.isPrizeFor("left");
};

Block.prototype.isPrizeFor = function (side) {
  // If it is not a prize, return false.
  if (!this.isPrize()) {return false;}

  // Look for either right or left in the prizeFor string.
  return this.prizeFor.toLowerCase().indexOf(side.toLowerCase()) > -1;
};
```

The block is similar to the ball but defines logic around prizes.
Prizes are blocks that are colored corresponding to either the left or right side.
If one side smashes a prize block with its color, it gets extra points.
If another side smashes a prize block for the opposite side, the opposite gets the extra points.
The goal for the player is to avoid the computer's prize blocks.

### Paddle

```javascript
/*
  David Lettier (C) 2016
  http://www.lettier.com/
*/

function Paddle() {}

// Inherit from CanvasElement.
Paddle.prototype = Object.create(CanvasElement.prototype);
// But set the constructor that creates the object to
// the Paddle function.
Paddle.prototype.constructor = Paddle;

// Get the width from the CSS that styles
// the paddleLeft node.
Paddle.prototype.WIDTH = (function () {
  var attr = getcomputedStyleAttr("paddleLeft", "width");

  // Remove "px" and convert the string to an integer.
  return parseInt(attr.replace("px", ""), 10);
})();

// Get the height from the CSS that styles
// the paddleLeft node.
Paddle.prototype.HEIGHT = (function () {
  var attr = getcomputedStyleAttr("paddleLeft", "height");

  // Remove "px" and convert the string to an integer.
  return parseInt(attr.replace("px", ""), 10);
})();
```

The paddles are used to hit the ball back and forth across the arena.
They can spin helping to aim or deflect the ball where the player or computer would like them to go.

## Initialization

Everything starts with the `init` function which is called once the player presses the start button.

```javascript
/*
  David Lettier (C) 2016
  http://www.lettier.com/
*/

function init() {
  // Get the window dimensions.
  var windowWidth  = window.innerWidth;
  var windowHeight = window.innerHeight;
  // Get the left and right color defined in the CSS.
  var leftColor  = getcomputedStyleAttr("paddleLeft",  "color");
  var rightColor = getcomputedStyleAttr("paddleRight", "color");
  // Initialize all of our components.
  var scoreBoard = new ScoreBoard();
  var referee = new Referee();
  var playerPaddleControls = new PlayerPaddleControls();
  var computerPaddleControls = new ComputerPaddleControls();
  // The instructions overlay before the game starts.
  var overLay = getNodeById("overlay");

  // Create the canvas component.
  // Store the object on this
  // so that it can be accessed in
  // update.
  this.canvas = new Canvas();

  // Initialize the canvas
  // expanding it to the size of the
  // window. Do not render the physics
  // simulation.
  this.canvas.init(
    {
      canvasId: "canvas",
      physicsCanvasId: "physics",
      renderPhysics: false,
      bounds: {
        top: ScoreBoard.prototype.HEIGHT,
        bottom: windowHeight,
        left: 0,
        right: windowWidth
      }
    }
  );

  // Setup the scoreboard.
  scoreBoard.init({scoreLeftId: "scoreLeft", scoreRightId: "scoreRight"});

  // Setup the referee that handles most of the game logic.
  referee.init();
```

To start off, we Initialize all of the components the game needs.

```javascript
  // Create the two paddles and balls--one for each side.
  function initPaddlesBalls() {
    var playerPaddle = null;
    var computerPaddle = null;

    computerPaddle = this.canvas.addPaddle(
      {
        name: "paddleLeft",
        x: 5,
        y: windowHeight / 2 - Paddle.prototype.HEIGHT / 2,
        color: leftColor
      }
    );
    this.canvas.addBall(
      {
        name: "ballLeft",
        x: 5 + Paddle.prototype.WIDTH,
        y: windowHeight / 2 - Ball.prototype.HEIGHT / 2,
        color: leftColor
      }
    );
    playerPaddle = this.canvas.addPaddle(
      {
        name: "paddleRight",
        x: windowWidth - 5 - Paddle.prototype.WIDTH,
        y: windowHeight / 2 - Paddle.prototype.HEIGHT / 2,
        color: rightColor
      }
    );
    this.canvas.addBall(
      {
        name: "ballRight",
        x: windowWidth - 5 - Paddle.prototype.WIDTH - Ball.prototype.WIDTH,
        y: windowHeight / 2 - Ball.prototype.HEIGHT / 2,
        color: rightColor
      }
    );

    // Give the *PaddleControls their respective paddles.
    playerPaddleControls.init({paddle: playerPaddle});
    computerPaddleControls.init({paddle: computerPaddle});
  }
```

Define the paddles and balls used in the game and wire up the paddles to their controls.

```javascript
  // Creates a M x N grid of blocks with some blocks
  // being prizes for their respective sides.
  function initBlocks () {
    var blockSpacing = 2;
    var blocksWidthPercent  = 0.15;
    // Define the initial grid size of the blocks.
    var blockBounds = {
      left:   (windowWidth  / 2) - (windowWidth  * blocksWidthPercent),
      right:  (windowWidth  / 2) + (windowWidth  * blocksWidthPercent),
      top:    this.canvas.bounds.top + blockSpacing,
      bottom: this.canvas.bounds.bottom - blockSpacing
    };
    var rows = [];
    var cols = [];

    // Adjust the block height and width based on the bounds.
    Block.prototype.HEIGHT = Block.prototype.WIDTH = (blockBounds.bottom - blockBounds.top) / 10;

    rows = range(blockBounds.top,  blockBounds.bottom, Block.prototype.HEIGHT + blockSpacing);
    cols = range(blockBounds.left, blockBounds.right,  Block.prototype.WIDTH  + blockSpacing);

    // For each block row.
    fjs.each(function (row, i) {
      // For each block column.
      fjs.each(function (col, j) {
        var isPrize = Math.random() > 0.6 ? true : false;
        var prizeFor = null;
        var blockColor = null;

        // Will this block be a prize?
        if (isPrize) {
          result = (
            Math.random() > 0.5 ? {
              color: leftColor,
              prizeFor: "paddleLeft"
            } : {
              color: rightColor,
              prizeFor: "paddleRight"
            }
          );
          prizeFor = result.prizeFor;
          blockColor = result.color;
        } else {
          blockColor = Block.prototype.COLOR;
          prizeFor = null;
        }

        // Tell the canvas to add this block.
        this.canvas.addBlock(
          {
            name: "block" + i + "_" + j,
            x: col,
            y: row,
            color: blockColor,
            prizeFor: prizeFor
          }
        );
      }.bind(this), cols);
    }.bind(this), rows);
  }
```

Create a grid of blocks spaced ever so slightly apart.
The width of the grid will be about about 15 percent of the screen width.
Some blocks will be prizes.

```javascript
  // Subscribe to the gameOver message.
  // This occurs when all the blocks are
  // smashed.
  pubSubTokens = [
    ["gameover", PubSub.subscribe("gameOver", reset)]
  ];
```

Listen for the game over message calling the reset function (defined below)
when received.

```javascript
  // Initialize the paddle and balls.
  initPaddlesBalls();
  // Initialize the block grid.
  initBlocks();
```

Call the two functions defined up above.

```javascript
  // Initialize the reset function.
  // This is called if the window is resized
  // or when a round is over.
  function reset() {
    // Reset all components.
    this.canvas.reset();
    scoreBoard.reset();
    referee.reset();
    playerPaddleControls.reset();
    computerPaddleControls.reset();

    // Remove all objects listening
    // for messages.
    PubSub.clearAllSubscriptions();

    // Do not listen for the resize event.
    window.removeEventListener("resize", reset);

    // Unsubscribe from the gameOver message.
    fjs.each(function (pubSubToken) {
      PubSub.unsubscribe(pubSubToken[1]);
    }.bind(this), this.pubSubTokens);

    // Initialize the game over from scratch.
    init();
  }
```

The reset functions wipes the game clean and setups a fresh round.

```javascript
  // Start the game loop with the first call to update.
  window.requestAnimationFrame(update.bind(this));
  // Call reset if the player resizes the browser window.
  window.addEventListener("resize", reset.bind(this));
```

Kick off the game loop and listen for the re-size event.

```javascript
  // Hide the instructions.
  overlay.style.visibility = "hidden";
}
```

After finishing all of the initialization, hide the instructions and start button.

## The Game Loop

The game loop is called repeatedly until the round is over.
It is responsible for getting the graphics and physics to update with each new requested animation frame.

```javascript
/*
  David Lettier (C) 2016
  http://www.lettier.com/
*/

// Runs the game loop.
function update() {
  var delta = null;

  // Get the current time.
  this.now = Date.now();

  if (!this.then) {
    // If then isn't defined, set it to now.
    this.then = this.now;
  }
```

We would like the game animation to run at roughly the same speed no matter what hardware the game is running on.
We'll use [Time-based Animation](https://www.viget.com/articles/time-based-animation) to keep the animation fairly consistent
across different machines.

```javascript
  delta = (this.now - this.then) / 1000;

  // Canvas returns true if it is ready.
  if (this.canvas.update(delta) === true) {
      this.requestAnimationFrameId = window.requestAnimationFrame(
        update
      );
  } else {
    // Canvas returned false so cancel the next request.
    window.cancelAnimationFrame(this.requestAnimationFrameId);
  }

  // Reset then to now.
  this.then = this.now;
}
```

We calculate how long it took to get back to updating the game loop.
By passing this `delta` scalar into `canvas.update`, any position or rotation update can be scaled by how long it took since the `delta` was last calculated.

## The Canvas

```javascript
/*
  David Lettier (C) 2016
  http://www.lettier.com/
*/

function Canvas () {
}
```

First we will define the constructor.

```javascript
Canvas.prototype.init = function (params) {
  this.initParams = params;

  this.bounds = params.bounds;

  this.canvasId = params.canvasId;

  this.physicsCanvasId = params.physicsCanvasId;
  this.renderPhysics = params.renderPhysics;

  this.canvasNode = getNodeById(this.canvasId);
  this.canvasNode.width  = window.innerWidth;
  this.canvasNode.height = window.innerHeight;

  this.paddles = [];
  this.balls   = [];
  this.blocks  = [];
  this.prizes  = [];

  // Handles drawing all the objects to the canvas.
  this.stage = new createjs.Stage(this.canvasId);

  // These are added to each balls' velocity
  // each physics world step.
  this.ballVelX = this.ballVelY = 0.02;

  // Initialize all of the world physics.
  this.initPhysics();

  // Subscribe to the updatePaddle message
  // sent from the Computer/PlayerPaddleControls components.
  this.pubSubTokens = [];
  this.pubSubTokens.push(
    [
      "updatePaddle",
      PubSub.subscribe("updatePaddle", this.updatePaddleFromMessage.bind(this))
    ],
    [
      "updateBlock",
      PubSub.subscribe("updateBlock", this.updateBlockFromMessage.bind(this))
    ],
    [
      "shakeCanvas",
      PubSub.subscribe("shakeCanvas", this.shakeCanvasAsync.bind(this))
    ]
  );

  // If not ready, the Canvas will not update.
  this.ready = true;
};
```

Retrieve the two canvas DOM nodes. Setup the empty arrays to hold all of the `CanvasElement`s.
Set an additive ball velocity to keep the ball moving.
Initialize all of the physics simulation.
Subscribe to the various message streams assigning the appropriate function to handle each message.
Finally, set `ready` to true, signally that `Canvas.update` can be called.

```javascript
Canvas.prototype.initPhysics = function () {
  var physicsRenderer = null;
  var physicsCanvasElement = null;

  this.world = Physics();

  if (this.renderPhysics) {
    // For debug purposes, draw the physics
    // objects over their canvasElements.
    physicsRenderer = Physics.renderer(
      "canvas",
      {
            el: this.physicsCanvasId,
         width: this.canvasNode.width,
        height: this.canvasNode.height
      }
    );
    this.world.add(physicsRenderer);
  } else {
    // Otherwise just remove the canvas node.
    physicsCanvasElement = getNodeById(this.physicsCanvasId);
    if (physicsCanvasElement) {
      physicsCanvasElement.parentNode.removeChild(physicsCanvasElement);
    }
  }

  // Add gravity, collision detection, collision response, and
  // an optimization procedure.
  this.world.add(Physics.behavior('constant-acceleration'));
  this.world.add(Physics.behavior("body-impulse-response"));
  this.world.add(Physics.behavior("body-collision-detection"));
  this.world.add(Physics.behavior("sweep-prune"));

  // Surround the canvas with a bounding box
  // so that the balls do not leave the arena.
  this.worldBoundingBox = Physics.aabb(
    this.bounds.left,
    this.bounds.top,
    this.bounds.right,
    this.bounds.bottom
  );
  // Initialize the canvas bounding box
  // collision detection.
  this.worldBoundingBox = Physics.behavior(
    "edge-collision-detection",
    {
      aabb: this.worldBoundingBox,
      restitution: 0.5, // Add some rebound bounce to each collision.
      cof: 0 // Remove all friction.
    }
  );
  // Set the canvasElement type for later logic.
  this.worldBoundingBox.body.canvasElementType = "aabb";
  this.world.add(this.worldBoundingBox);

  // Send all collisions to the bodyCollisions
  // function.
  this.world.on(
    "collisions:detected",
    this.bodyCollisions.bind(this)
  );
};
```

`PhysicsJS` will handle all of the collision detection, friction, momentum, mass, and velocity calculations governing the position and rotation of each `CanvasElement` held by the `Canvas` object.
We will surround the arena with a bounding box so that the balls do not fly off the screen.

```javascript
Canvas.prototype.update = function () {
  // If not ready, just exit.
  if (this.ready === false) {return false;}

  // Alter any current physics body state
  // before it goes through the next physics step.
  this.updateToPhysics();

  // Move forward one step in the physics simulation.
  this.world.step(Date.now());

  // Update the Stage elements based on their
  // physics simulation states.
  this.updateFromPhysics();

  // Render the physics simulation for debugging.
  if (this.renderPhysics) { this.world.render(); }

  // Clear and redraw the canvas.
  this.stage.update();

  return true;
};
```

The canvas update function updates the current physics simulation state, advances the physics state one step, updates the canvas elements based on their physical models, and then redraws the canvas with the updated canvas elements in their new positions and rotations.

```javascript
Canvas.prototype.reset = function () {
  // Signal that the canvas is not ready for updating.
  this.ready = false;

  // Clear out all objects.
  this.paddles = [];
  this.balls   = [];
  this.blocks  = [];
  this.prizes  = [];

  // Clear the stage.
  this.stage.removeAllChildren();
  this.stage.removeAllEventListeners();
  this.stage = undefined;

  // Unsubscribe from all messages.
  fjs.each(function (pubSubToken) {
    PubSub.unsubscribe(pubSubToken[1]);
  }.bind(this), this.pubSubTokens);

  // Remove the physics simulation.
  this.world.destroy();
  this.world = undefined;
};
```

When the game is over, the canvas clears out all of its data structures, clears the stage, unsubscribes from all message streams, and destroys the physical simulation.

```javascript
Canvas.prototype.addPaddle = function (params) {
  // Add a paddle to the Canvas object.
  var paddle = this.addCanvasElement(Paddle, params);

  // Add the actual canvas shape to
  // be drawn each frame.
  this.addStageRect(paddle);

  // Add to the physical world
  // simulation and associate the
  // physics body with the paddle
  // canvasElement.
  this.addPhysics(
    paddle,
    paddle.constructor.name,
    "rectangle",
    {
      x: paddle.x + (paddle.WIDTH / 2),
      y: paddle.y + (paddle.HEIGHT / 2),
      width: paddle.WIDTH,
      height: paddle.HEIGHT,
      treatment: "static",
      restitution: 1,
      cof: 0
    }
  );

  return paddle;
};
```

First create the `Paddle` object and then add both the `Stage` shape and physics body.


```javascript
Canvas.prototype.addBlock = function (params) {
  // Add a block to the Canvas object.
  var block = this.addCanvasElement(Block, params);

  // Add the actual canvas shape to
  // be drawn each frame.
  this.addStageRect(block);

  // Add to the physical world
  // simulation and associate the
  // physics body with the paddle
  // canvasElement.
  this.addPhysics(
    block,
    block.constructor.name,
    "rectangle",
    {
      x: block.x + (block.WIDTH / 2),
      y: block.y + (block.HEIGHT / 2),
      width: block.WIDTH,
      height: block.HEIGHT,
      mass: 10,
      vy: 1,
      restitution: 1,
      cof: 1
    }
  );
};
```

Same as the `addPaddle` function.

```javascript
Canvas.prototype.addBall = function (params) {
  // Add a ball to the Canvas object.
  var ball = this.addCanvasElement(Ball, params);

  // Add the actual canvas shape to
  // be drawn each frame.
  this.addStageCirc(ball);

  // Add to the physical world
  // simulation and associate the
  // physics body with the paddle
  // canvasElement.
  this.addPhysics(
    ball,
    ball.constructor.name,
    "circle",
    {
      x: ball.x + (ball.WIDTH / 2),
      y: ball.y + (ball.HEIGHT / 2),
      // Depending on the side, adjust the velocity in the X direction.
      vx: params.name.toLowerCase().indexOf("right") != -1 ? -2.0 : 2.0,
      vy: 0.5,
      radius: ball.WIDTH / 2,
      mass: 5,
      restitution: 1,
      cof: 1
    }
  );
};
```

We flip the sign of the initial velocity of the ball in the x-direction based on the side of the ball.

```javascript
Canvas.prototype.addCanvasElement = function (Type, params) {
  // Add either a ball, block, or paddle based on the Type.
  var typeStrPlural = this.getArrayKeyByType(Type);
  var canvasElement = null;

  canvasElement = new Type();
  canvasElement.init(params);

  // Add it to the other objects of the same type.
  this[typeStrPlural].push(canvasElement);

  return canvasElement;
};
```

The canvas element links the `Stage` object and `PhysicsJS` world body object.

```javascript
Canvas.prototype.addPhysics = function (canvasElement, canvasElementType, bodyType, bodyParams) {
  // Based on the type, add a physical body to the world
  // physics simulation.
  var physics = Physics.body(bodyType, bodyParams);

  if (physics) {
    // Associate the physics body with its canvasElement.
    // This will be needed later for color changing and scoring.
    physics.canvasElementType = canvasElementType;
    physics.canvasElement = canvasElement;
    canvasElement.physics = physics;
    this.world.add(physics);
  }

  return physics;
};
```

Create a physical body and add it to the physics world simulation.
The `canvasElement` can access the physical body via its physics attribute.

```javascript
Canvas.prototype.addStageRect = function (canvasElement) {
  // Add a rectangular Stage shape object.
  return this.addStageShape(canvasElement, "drawRect");
};

Canvas.prototype.addStageCirc = function (canvasElement) {
  // Add a circular Stage shape object.
  return this.addStageShape(canvasElement, "drawEllipse");
};
```

Two helper methods that can either add a `Stage` rectangle or circle to the canvas.

```javascript
Canvas.prototype.addStageShape = function (canvasElement, drawFunct) {
  var view, displayObject = null;

  // Create the Stage shape.
  view = new createjs.Shape();
  view.graphics.beginFill(
    canvasElement.color
  )[drawFunct](0, 0, canvasElement.WIDTH, canvasElement.HEIGHT);
  view.x = canvasElement.x;
  view.y = canvasElement.y;
  view.regX = canvasElement.WIDTH / 2; // The center of the object.
  view.regY = canvasElement.HEIGHT / 2; // The center of the object.

  // Associate the Stage object with the canvasElement object.
  displayObject = this.stage.addChild(view);
  displayObject.canvasElement = canvasElement;
  canvasElement.view = displayObject;
  canvasElement.id = displayObject.id;

  return view;
};
```

Creates the `Stage` shape that will be rendered on the canvas each frame.

```javascript
// Adapts the updatePaddle function for use with PubSub.
Canvas.prototype.updatePaddleFromMessage = function (message, params) {
  this.updatePaddle(params.name, params);
};

// Adapts the updateBlock function for use with PubSub.
Canvas.prototype.updateBlockFromMessage = function (message, params) {
  this.updateBlock(params.name, params);
};
```

These methods are called whenever an event happens on the `updateBlock` or `updatePaddle` message streams from `PubSub`.


```javascript
Canvas.prototype.updatePaddle = function (name, params) {
  this.updateType(
    name,
    params,
    Paddle
  );
};

Canvas.prototype.updateBall = function (name, params) {
  this.updateType(
    name,
    params,
    Ball
  );
};

Canvas.prototype.updateBlock = function (name, params) {
  this.updateType(
    name,
    params,
    Block
  );
};

Canvas.prototype.updateType = function (name, params, Type) {
  // Find the object in the various arrays.
  var found = this.findByNameType(name, Type);
  // Normalize the type name.
  var typeName = Type.name.toLowerCase();

  if (found) {
    // If X is set, update the X position
    // for the model and Stage object.
    if (params.x) {
      found.x = params.x;
      found.view.x = params.x;
    }

    // If Y is set, update the Y position
    // for the model and Stage object.
    if (params.y) {
      found.y = params.y;
      found.view.y = params.y;
    }

    // If the rotation is set, update the rotation
    // for the Stage object.
    if (params.rotRad) {
      found.rotRad = params.rotRad;
      // Stage only deals with degrees.
      // The rotation comes from PhysicsJS which deals
      // only with radians.
      found.view.rotation = radToDeg(params.rotRad);
    }

    // Update the canvasElement color if given.
    if (params.color) {
      found.changeColor(params.color);
    }

    // Let any component listening know that
    // we updated this object that has a
    // certain canvasElement Type.
    params = {};
    params[typeName] = found;
    PubSub.publish(
      typeName + "Updated",
      params
    );
  }
};
```

The canvas update interface used by other components to change a `canvasElement`'s position, rotation, and/or color.

```javascript
Canvas.prototype.removeByNameType = function (name, Type) {
  // Find the object in the various arrays.
  var found = this.findByNameType(name, Type);
  // Get the string plural form of the Type.
  var arrayKey = this.getArrayKeyByType(Type);

  if (found && this[arrayKey]) {
    // Filter out any in the array
    // that has the same name
    // as the object we are removing.
    // Uses an anonymous function.
    this[arrayKey] = fjs.select(
      "x => x.name !== '" + name + "'",
      this[arrayKey]
    );

    if (arrayKey == "blocks" && this[arrayKey].length <= 0) {
      // If the array of blocks is empty
      // (they were all smashed), let any
      // component listening know.
      // This will signal the end of the game.
      PubSub.publish("noMoreBlocks", {});
    }
  }
};
```

Using `FunctionalJS`, we find the `canvasElement` by its name and type and then remove it from its corresponding array of `canvasElement`s of the same type. We remove it by selecting all the elements in the array that do not have the same name.

```javascript
Canvas.prototype.findByNameType = function (name, Type) {
  // Get the string plural lowercase form of the Type.
  var arrayKey = this.getArrayKeyByType(Type);

  // Return the first object with the same name
  // in the array of objects with Type.
  return fjs.first(
    "x => x.name === '" + name + "'",
    this[arrayKey]
  );
};

Canvas.prototype.getArrayKeyByType = function (Type) {
  // Takes a type and returns the array object key.
  // For example, it turns 'Paddle' into 'paddles'.
  // 'Paddle' => 'paddles' => this['paddles'] => this.paddles
  if (!Type) {return "";}

  return Type.name.toLowerCase() + "s";
};
```

First get the plural array key string by the type. Once we have this, we access the array and find the first element that has the same name. This is an example of using `FunctionalJS`'s lambda function syntax.

```javascript
Canvas.prototype.updateToPhysics = function () {
  if (!this.world) {
    return;
  }

  fjs.each(function (body) {
    var vel = null;

    if (body.canvasElementType === "Ball") {
      // Increase the magnitude of each ball's
      // velocity vector.
      // Without it, the balls would eventually
      // come to rest.
      vel = body.state.vel;
      body.state.vel = Physics.vector(
        // Make sure to get the sign right
        // to maintain the same direction
        // but increase the magnitude.
        vel.x + sign(vel.x) * this.ballVelX,
        vel.y + sign(vel.y) * this.ballVelY
      );
    } else if (body.canvasElementType === "Paddle") {
      // Update the paddle positions in the physics
      // simulation based on the positions
      // set by the *PaddleControls.
      body.state.pos = Physics.vector(
        body.canvasElement.x,
        body.canvasElement.y
      );
      body.state.angular.pos = body.canvasElement.rotRad;
    }
  }.bind(this), this.world.getBodies());
};
```

Normally, unless acted upon, the balls will come to rest.
To correct this, we add back some velocity to each ball's physics model.

```javascript
Canvas.prototype.updateFromPhysics = function () {
  var updateParams = null;

  if (!this.world) {
    return;
  }

  // After the world physics state has updated,
  // update the canvasElement balls and blocks
  // with their new positions and rotations.
  // This will be sent to Stage which will
  // redraw them on the canvas at their new locations
  // and rotations based on the physics calculations.
  // This bridges the gap between PhysicsJS and EaselJS.
  fjs.each(function (body) {
    updateParams = {
           x: body.state.pos.x,
           y: body.state.pos.y,
      rotRad: body.state.angular.pos
    };

    if (body.canvasElementType === "Ball") {
      this.updateBall(
        body.canvasElement.name,
        updateParams
      );
    } else if (body.canvasElementType === "Block") {
      this.updateBlock(
        body.canvasElement.name,
        updateParams
      );
    }
  }.bind(this), this.world.getBodies());
};
```

After the physics simulation steps forward in time, we need to update each `canvasElement`'s attributes.
When we update a canvas element, its view or `Stage` shape is also updated.
Now when we go to redraw the canvas, the canvas elements drawn will reflect their physics model counterparts.

```javascript
Canvas.prototype.bodyCollisions = function (data) {
  var collisions = data.collisions;

  // Helper function that we will curry.
  function getType(bodyA, bodyB, typeName) {
    // Using an anonymous function,
    // find the first with the same typeName.
    return fjs.first(
      "x => x.canvasElementType === '" + typeName + "'",
      [bodyA, bodyB]
    );
  }

  fjs.each(function (collision) {
    var bodyA = collision.bodyA;
    var bodyB = collision.bodyB;
    var find = fjs.curry(getType)(bodyA, bodyB);
    var bodyBlock   = find("Block");
    var bodyBall    = find("Ball");
    var bodyPaddle  = find("Paddle");
    var boundingBox = find("aabb");

    function removeBlock() {
      if (!this.ready) {return;}

      // Remove the block from Stage.
      this.stage.removeChild(bodyBlock.canvasElement.view);
      // Remove the block from PhysicsJS.
      this.world.remove(bodyBlock);
      // Mark the block deleted.
      bodyBlock.canvasElement.deleted = true;
      // Remove the block from the this.blocks array.
      this.removeByNameType(bodyBlock.canvasElement.name, Block);
    }

    // Different collision cases.

    // Ball hit a block.
    if (bodyBlock && bodyBall) {
      PubSub.publish(
        'blockBallHit',
        {ball: bodyBall.canvasElement, block: bodyBlock.canvasElement}
      );

      // Go ahead a remove the block in 200 milliseconds.
      window.setTimeout(removeBlock.bind(this), 200);
    }

    // A ball hit a paddle.
    if (bodyBall && bodyPaddle) {
      // Let the Referee know.
      PubSub.publish(
        'ballPaddleHit',
        {ball: bodyBall.canvasElement, paddle: bodyPaddle.canvasElement}
      );
    }

    // A ball hit the bounding box that surrounds the canvas.
    if (bodyBall && boundingBox) {
      // Let the Referee know.
      PubSub.publish(
        'boundingBoxBallHit',
        {ball: bodyBall.canvasElement, boundingBox: boundingBox}
      );
    }
  }.bind(this), collisions);
};
```

Going through each collision, we handle all of the cases of interest.
For each case, we send out a message indicating the collision to any interested party.
When a block and ball collided, we remove the block after 200 milliseconds.

```javascript
Canvas.prototype.shakeCanvasAsync = function () {
  // Calls shakeCanvas in 0 milliseconds.
  window.setTimeout(this.shakeCanvas.bind(this), 0);
};

Canvas.prototype.shakeCanvas = function () {
  // If not set.
  if (!this.shakeCanvasSteps) {
    // Set it to zero.
    this.shakeCanvasSteps = 0;
  }

  // Shake the canvas 20 times.
  if (this.shakeCanvasSteps <= 20) {
    // If even.
    if (this.shakeCanvasSteps % 2 === 0) {
      // Shake it up.
      this.canvasNode.style.position = "absolute";
      this.canvasNode.style.top = "-5px";
    } else {
      // Shake it down.
      this.canvasNode.style.position = "absolute";
      this.canvasNode.style.top = "10px";
    }

    // Update the number of shakes and call this function
    // in 10 milliseconds.
    this.shakeCanvasSteps += 1;
    window.setTimeout(this.shakeCanvas.bind(this), 10);
  } else {
    // All shakes were performed so reset the canvas
    // back to its normal position.
    this.shakeCanvasSteps = 0;
    this.canvasNode.style.position = "absolute";
    this.canvasNode.style.top = "0px";
  }
};
```

The final portion of `Canvas` is a function to shake the DOM canvas node.
This puts the "rubul" in Dubul Rubul.
The `Referee` will send out a message to shake the canvas when ever the player
loses out on points.
The effect adds a sense of urgency and realism with minimal overhead.

## Paddle Controls

The paddle controls allow the player and computer to move their paddle up or down and to rotate it in place.

### Computer

```javascript
/*
  David Lettier (C) 2016
  http://www.lettier.com/
*/

function ComputerPaddleControls () {}

// Inherit from the PaddleControls object.
ComputerPaddleControls.prototype = Object.create(PaddleControls.prototype);
// Set the constructor to ComputerPaddleControls.
ComputerPaddleControls.prototype.constructor = ComputerPaddleControls;
```

```javascript
ComputerPaddleControls.prototype.init = function (params) {
  this.pubSubTokens = [];

  // Call PaddleControls's init function after
  // defining the pubSubTokens.
  PaddleControls.prototype.init.call(this, params);
};
```

Initialize and call the `init` method up the prototype chain (`super` in some sense).

```javascript
ComputerPaddleControls.prototype.movePaddle = function (message, params) {
  // This is where we define the "AI" for the computer paddle.
  var ball = null;

  // If params are not defined, just return.
  if (!params) {return;}

  ball = params.ball;

  // If the ball is not defined, just return.
  if (!ball) {return;}

  if (ball.name === "ballRight") {
    // We could make the computer paddle
    // perfect by just following its own ball.
    // However, will make it follow the
    // player's ball which is the right ball.
    // Later on we will construct a more robust
    // AI for the computer's paddle.
    PubSub.publish(
      'updatePaddle',
      {
        name: this.paddle.name,
        y: ball.y - (ball.HEIGHT / 2)
      }
    );
  }
};
```

This is where we define the AI for the computer paddle.
For now the logic is extremely basic.
In a later post, we will revisit with a machine learning approach.

```javascript
ComputerPaddleControls.prototype.enable = function () {
  if (!this.pubSubTokens) {return;}

  // Listen for the ballUpdated message
  // from the Canvas.
  this.pubSubTokens.push(
    [
      "ballUpdated",
      PubSub.subscribe("ballUpdated", this.movePaddle.bind(this))
    ]
  );
};

ComputerPaddleControls.prototype.disable = function () {
  if (!this.pubSubTokens) {return;}

  // Unsubscribe from any message which was
  // subscribed to previously.
  fjs.each(function (pubSubToken) {
    PubSub.unsubscribe(pubSubToken[1]);
  }.bind(this), this.pubSubTokens);
};
```

Enable and disable functions primarily used during initialization and shutdown.
These could be used to add another dynamic to the game.
For example, disable a paddle briefly if its corresponding ball smashes the other side's blocks.

### Player

```javascript
/*
  David Lettier (C) 2016
  http://www.lettier.com/
*/

function PlayerPaddleControls () {}

// Inherit from the PaddleControls object.
PlayerPaddleControls.prototype = Object.create(PaddleControls.prototype);
// Set the constructor to PlayerPaddleControls.
PlayerPaddleControls.prototype.constructor = PlayerPaddleControls;
```

```javascript
PlayerPaddleControls.prototype.movePaddle = function (event) {
  // The height the mouse is at on the screen.
  var y = event.clientY;

  // The paddle rotation in radians.
  // If the mouse is all the way to the left,
  // the rotation is 0 radians.
  // If the mouse is all the way to the right,
  // the rotation is ~6.283 radians or ~360 degrees.
  // By rotating the paddle, the player can aim or
  // deflect the ball where they want it to go.
  var rotRad = (event.clientX / window.innerWidth) * 6.283185307179586;

  // Tell the canvas to update the player's paddle canvasElement.
  PubSub.publish(
    "updatePaddle",
    {
      name: this.paddle.name,
      y: y,
      rotRad: rotRad
    }
  );
};
```

Based on the mouse's position at the time of the event, rotate and move the player's paddle up or down.

```javascript
PaddleControls.prototype.enable = function () {
  // Listen for the mousemove event.
  // When the player moves their mouse, call movePaddle.
  window.addEventListener("mousemove", this.movePaddle.bind(this));
};

PaddleControls.prototype.disable = function () {
  // Do not listen for the mousemove event.
  window.removeEventListener("mousemove", this.movePaddle.bind(this));
};
```

## Scoring

### Scoreboard

The `Scoreboard` keeps track of the HTML elements holding the player's and computer's onscreen scores.

```javascript
/*
  David Lettier (C) 2016
  http://www.lettier.com/
*/

function Scoreboard() {}

Scoreboard.prototype.init = function (params) {
  this.initParams = params;
  this.scoreLeftId  = params.scoreLeftId;
  this.scoreRightId = params.scoreRightId;

  // Get the computer's score div.
  this.scoreLeftElement = getNodeById(this.scoreLeftId);
  // Get the player's score div.
  this.scoreRightElement = getNodeById(this.scoreRightId);

  this.scoreElements = [this.scoreLeftElement, this.scoreRightElement];

  // Listen for the updateScore message.
  // This will come from the Referee.
  this.updateScorePubSubToken = PubSub.subscribe(
    "updateScore",
    this.updateScore.bind(this)
  );

  // Initialize each score to zero.
  this.setEachScore(params.initialScore || 0);
};
```

Waits for any `updateScore` message and sets both scores to zero.

```javascript
Scoreboard.prototype.HEIGHT = (function () {
  var attr = getcomputedStyleAttr("scoreLeft", "height");

  // Useful for determining the height of the bounding
  // box so it does not overlap with the scoreboard.
  return parseInt(attr.replace("px", ""), 10);
})();
```

Gets the height of the scoreboard defined by the CSS.

```javascript
Scoreboard.prototype.reset = function () {
  // Reset both scores to zero.
  this.setEachScore(0);
  this.scoreElements = undefined;

  // Unsubscribe from the updateScore message.
  PubSub.unsubscribe(this.updateScorePubSubToken);
};
```

Reset each score to zero.

```javascript
Scoreboard.prototype.setEachScore = function (score) {
  // For each score board element.
  fjs.each(function (element) {
    // Set the score text to score.
    element.innerHTML = score;
  }.bind(this), this.scoreElements);
};
```

Iterates through each score's side and sets the score.

```javascript
Scoreboard.prototype.updateScore = function (message, params) {
  // If score left is defined.
  if (params.scoreLeft) {
    // Update the computer's onscreen score.
    this.updateScoreLeft(params.scoreLeft.score);
  }

  // If score left is defined.
  if (params.scoreRight) {
    // Update the player's onscreen score.
    this.updateScoreRight(params.scoreRight.score);
  }
};

Scoreboard.prototype.updateScoreLeft = function (score) {
  // Update the left side.
  this.updateScoreSide(score, 0);
};

Scoreboard.prototype.updateScoreRight = function (score) {
  // Update the right side.
  this.updateScoreSide(score, 1);
};

Scoreboard.prototype.updateScoreSide = function (score, side) {
  var currentScorce = this.scoreElements[side].innerHTML;

  if (side < this.scoreElements.length) {
    // Update the onscreen score text for side.
    this.scoreElements[side].innerHTML = parseInt(currentScorce, 10) + score;
  }
};
```

Straightforward update functions.

### Referee

The `Referee` objects handles most of the [Business Logic](https://en.wikipedia.org/wiki/Business_logic) or game-play mechanics.

```javascript
/*
  David Lettier (C) 2016
  http://www.lettier.com/
*/

function Referee() {}

Referee.prototype.init = function (params) {
  if (!params) {
    params = {};
  }

  this.initParams = params;

  this.pubSubTokens = [];
  this.pubSubTokens.push(
    [
      "blockBallHit",
      PubSub.subscribe("blockBallHit", this.blockBallHit.bind(this))
    ],
    [
      "ballPaddleHit",
      PubSub.subscribe("ballPaddleHit", this.ballPaddleHit.bind(this))
    ],
    [
      "boundingBoxBallHit",
      PubSub.subscribe("boundingBoxBallHit", this.boundingBoxBallHit.bind(this))
    ],
    [
      // This message will eventually end the game loop.
      "noMoreBlocks",
      PubSub.subscribe("noMoreBlocks", this.gameOver.bind(this))
    ]
  );
};
```

Subscribe to all collision messages.
Listen for the `noMoreBlocks` message in order to fire off the `gameOver` message.

```javascript
Referee.prototype.reset = function () {
  // Unsubscribe from all PubSub messages.
  this.pubSubTokens.forEach(function (pubSubToken) {
    PubSub.unsubscribe(pubSubToken[1]);
  });
};
```

The `Referee` resets by unsubscribing from all messages.

```javascript
Referee.prototype.blockBallHit = function (message, params) {
  var ball = params.ball;
  var block = params.block;

  if (!ball || !block) {return;}

  if (ball.isRight() & !block.isPrize()) { // For the right side.
    // The ball is for the right side and the block is for any side.
    PubSub.publish("updateScore", {scoreRight: {score: 1}});
    PubSub.publish("updateBlock", {name: block.name, color: "#f1c40f"});
  } else if (ball.isRight() && block.isPrizeForRight()) {
    // The ball is for the right side and the block is for the right side.
    PubSub.publish("updateScore", {scoreRight: {score: 20}});
    PubSub.publish("updateBlock", {name: block.name, color: "#7459FF"});
  } else if (ball.isRight() && block.isPrizeForLeft()) {
    // The ball is for the right side and the block is for the left side.
    PubSub.publish("updateScore", {scoreLeft: {score: 10}});
    PubSub.publish("shakeCanvas", {});
    PubSub.publish("updateBlock", {name: block.name, color: "#e74c3c"});
  } else if (ball.isLeft() && !block.isPrize()) { // For the left side.
    // The ball is for the left side and the block is for any side.
    PubSub.publish("updateScore", {scoreLeft: {score: 1}});
    PubSub.publish("updateBlock", {name: block.name, color: "#f1c40f"});
  } else if (ball.isLeft() && block.isPrizeForLeft()) {
    // The ball is for the left side and the block is for the left side.
    PubSub.publish("updateScore", {scoreLeft: {score: 20}});
    PubSub.publish("updateBlock", {name: block.name, color: "#7459FF"});
  } else if (ball.isLeft() && block.isPrizeForRight()) {
    // The ball is for the left side and the block is for the right side.
    PubSub.publish("updateScore", {scoreRight: {score: 10}});
    PubSub.publish("updateBlock", {name: block.name, color: "#e74c3c"});
  }
};
```

Each collision case and corresponding action.
If the sides match, then the side in question gets the point.
Otherwise, the opposite side gets some points.
When the player does not get the points, the `Referee` sends out a message to shake the canvas.

```javascript
Referee.prototype.ballPaddleHit = function (message, params) {
  var ball = params.ball;
  var paddle = params.paddle;

  if (!ball || !paddle) {return;}

  if (ball.isRight() && paddle.isLeft()) {
    // If the ball is for the right side but the paddle
    // is for the left side, give the right side 5 points.
    PubSub.publish("updateScore", {scoreRight: {score: 5}});
  } else if (ball.isLeft() && paddle.isRight()) {
    // If the ball is for the left side but the paddle
    // is for the right side, give the left side 5 points.
    PubSub.publish("updateScore", {scoreLeft: {score: 5}});
    PubSub.publish("shakeCanvas", {});
  }
};
```

The computer or player can give points to the opposite side if they touch the other side's ball with their own paddle.

```javascript
Referee.prototype.boundingBoxBallHit = function (message, params) {
  var ball = params.ball;

  if (!ball) {return;}

  if (ball.isRight()) {
    // The right (player side) ball hit the bounding box.
    // Give the computer 1 point and tell the Canvas
    // to shake.
    PubSub.publish("updateScore", {scoreLeft: {score: 1}});
    PubSub.publish("shakeCanvas", {});
  } else if (ball.isLeft()) {
    // The left (computer side) ball hit the bounding box.
    // Give the player one point.
    PubSub.publish("updateScore", {scoreRight: {score: 1}});
  }
};
```

If the ball touches the bounding box, the opposite side earns a point.

```javascript
Referee.prototype.gameOver = function (message, params) {
  // Signal that the game is over.
  // This will trigger the reset
  // function initialized in init.js.
  PubSub.publish("gameOver", {});
};
```

This is called once all of the blocks have been smashed.

# Wrap-up

Using functional programming, the HTML canvas, a physics simulation, and a publish subscribe message model, we built Dubul Rubul--a video game playable in any modern browser. In future posts we will revisit the computer paddle's AI and look at adding sound effects for added immersion.
