---
title: Let's make an Animation with PureScript and C++
jumbotron_image: /images/2017-10-23-animation-purescript-cpp-sfml/jumbotron-image.jpg
preview_image: /images/2017-10-23-animation-purescript-cpp-sfml/preview-image.jpg
description: Using PureScript, Pure11, C++, and SFML, we programmatically create a recursive animation.
author: David Lettier
---

<!--https://pixabay.com/en/staircase-staircase-spiral-france-2607787/-->

## The animation

![](/images/2017-10-23-animation-purescript-cpp-sfml/purescript-animation.gif){.post-img .post-img-small .post-img-limit}

Note that the animation contains the
["PureScript Logo"](https://github.com/purescript/logo) by Gareth Hughes,
licensed under [CC BY 4.0](https://creativecommons.org/licenses/by/4.0/legalcode) / "PURESCRIPT" text removed from the original.

## Who this is for

* Programmers looking to try out PureScript
* PureScript programmers looking to target the desktop
* Programmers interested in functional programming
* Game developers interested in SFML
* Recursion enthusiasts

## What we'll cover

* Installing the Haskell Platform
* Setting up PureScript/Pure11
* Pass by reference vs pass by value
* Binding SFML's C++ API to PureScript
* Garbage collection
* Time-based Animation
* Compiling our project

## Project setup

Before we begin developing, we need to set up our project with all of its files and dependencies.

### Project structure

Listed below is the project structure we will end up with.

```bash
project/
  src/
    Main.purs
    Main.cc
    Main.hh
  static/
    purescript-logo.png
    purescript-logo-inverted.png
  Makefile
  psc-package.json
```

Go ahead and run the following commands.

```bash
mkdir -p project project/src project/static
touch src/Main.purs src/Main.css src/Main.hh
```

### Images

Below are the image files we will need.
Make sure to right-click, save link as, and place them in `project/static/`.

* [
purescript-logo.png
](/images/2017-10-23-animation-purescript-cpp-sfml/purescript-logo.png)
* [
purescript-logo-inverted.png
](/images/2017-10-23-animation-purescript-cpp-sfml/purescript-logo-inverted.png)

Note that both images contain the
["PureScript Logo"](https://github.com/purescript/logo) by Gareth Hughes,
licensed under [CC BY 4.0](https://creativecommons.org/licenses/by/4.0/legalcode) / "PURESCRIPT" text removed from the original.

### Stack

To compile Pure11, the "C++11/native compiler backend for PureScript", we will need the program Stack.

You can install Stack via the [Haskell Platform](https://www.haskell.org/platform/).

### Pure11

To target C++, we will need to build Pure11 for our machine.
Make sure you have `git`, `make`, and a C++ compiler installed.

```bash
git clone git@github.com:pure11/pure11.git
cd pure11
stack setup
stack install
export PATH="$PATH:$HOME/.local/bin/"
```

### Makefile

With Pure11 built, we can now generate the `Makefile` we will need to build our project.

```bash
cd project
pcc
```

We should now have a `Makefile` and a `psc-package.json` file in the root of our project directory.

### SFML

We will need the [Simple and Fast Multimedia Library](https://www.sfml-dev.org/) to load and animate our images.
Go ahead and install SFML for your platform.
Note that [Version 2.4.2](https://www.sfml-dev.org/download/sfml/2.4.2/) is the version we will be using for our project.

## Source

The source code for the project consists of three files.

* `Main.hh`
    * The C++ header file containing the function declarations for the functions defined in `Main.cc`
* `Main.cc`
    * The function definitions of the functions declared in the header file `Main.hh`
* `Main.purs`
    * The PureScript file defining our program

### Main.hh

In `Main.hh`, we declare all of the functions we will be using in `Main.purs`.

```cpp
/*
  (C) 2017 David Lettier
  lettier.com
*/

namespace Main {
  using namespace PureScript;
  using namespace std;

  auto toNumber(const int) -> double;

  auto easeIn(const double) -> double;

  auto timeNow() -> double;

  auto makeWindow(const int, const int, const std::string) -> any;
  auto makeColor(const int, const int, const int, const int) -> any;
  auto makeTextureFromFile(const std::string) -> any;
  auto makeRectangleShape(const double, const double) -> any;

  auto shapeSetTexture(const any&, const any&, const any&) -> any;

  auto transformableSetRotation(const any&, const any&, const double) -> any;
  auto transformableSetScale(const any&, const any&, const double, const double) -> any;
  auto transformableSetOrigin(const any&, const any&, const double, const double) -> any;
  auto transformableMove(const any&, const any&, const double, const double) -> any;

  auto eventIsClosed(const any&) -> any;

  auto windowIsOpen(const any&) -> any;
  auto windowPollEvent(const any&, const any&) -> any;
  auto windowClear(const any&, const any&) -> any;
  auto windowDisplay(const any&) -> any;
  auto windowDraw(const any&, const any&, const any&) -> any;
  auto windowClose(const any&) -> any;

  auto loop(const any&, const any&) -> any;
}
```

Go ahead and copy this into `project/src/Main.hh`.

At the top of the file we define a namespace called `Main`.
This will line up with our module name `Main` in `Main.purs`.

```cpp
| PureScript  | C++           |
|-------------|---------------|
| Number      | double        |
| Int         | int           |
| String      | std::string   |
| Boolean     | bool          |
| Char        | char          |
| any         | *             |
```

Pure11 supports the C++ types `double`, `int`, `char`, `std::string`, and `bool`.
Thus we make use of them wherever possible.
For the other types, that we will be dealing with, Pure11 provides us with the
[any](https://github.com/pure11/pure11/blob/pure11/pcc/runtime/purescript.hh#L81) type.
You can think of `any` as a dynamic type.

```c++
auto identifier (...args) -> return_type;
```

If it has been awhile since you have written C++, you may not recognize the arrow syntax.
The `auto` keyword lets the compiler know that the return type for the identifier will follow after it.

If you have never programmed in C++, you may not recognize the `&` operator.
`const any&` means that the function expects an `any` reference or
alias---reference meaning that the `any` instance outside the function and the `any` instance inside the function
are one in the same.
In other words, the `any` inside the function is a _not_ a copy of the one outside the function.

<blockquote>
By definition, pass by value means you are making a copy in memory of the actual parameter's value that is passed in, a copy of the contents of the actual parameter. [...]
In pass by reference (also called pass by address), a copy of the address of the actual parameter is stored.
<footer>
[
Function pass by value vs. pass by reference by Dr. Carol Zander
](http://courses.washington.edu/css342/zander/css332/passby.html)
</footer>
</blockquote>

### Main.cc

The following source contains all of the definitions for the functions we declared in `Main.hh`.
Go ahead and copy it into `project/src/Main.cc`.

```cpp
/*
  (C) 2017 David Lettier
  lettier.com
*/

#include <iostream>
#include <cmath>
#include <math.h>
#include <chrono>
#include <SFML/Graphics.hpp>
#import "PureScript/PureScript.hh"

namespace Main {
  using namespace PureScript;
  using namespace std;
  using namespace std::chrono;
  using namespace sf;

  auto toNumber(const int x) -> double {
    return (double) x;
  }

  auto timeNow() -> double {
    return (double) duration_cast<std::chrono::milliseconds>(
      system_clock::now().time_since_epoch()
    ).count();
  }

  auto easeIn(const double x) -> double {
    return pow(M_E, x / 5.0) * (1.0 / 20.0);
  }

  auto makeWindow(const int width, const int height, const std::string title) -> any {
    return [=]() -> any {
      auto window = make_managed<sf::RenderWindow>(
        sf::VideoMode(width, height),
        title
      );
      return window;
    };
  }

  auto makeColor(const int r, const int g, const int b, const int a) -> any {
    return [=]() -> any {
      auto color = make_managed<sf::Color>(r, g, b, a);
      return color;
    };
  }

  auto makeTextureFromFile(const std::string path) -> any {
    return [=]() -> any {
      auto texture = make_managed<sf::Texture>();
      texture->loadFromFile(path);
      return texture;
    };
  }

  auto makeRectangleShape(const double width, const double height) -> any {
    return [=]() -> any {
      auto shape = make_managed<sf::RectangleShape>(sf::Vector2f(width, height));
      return shape;
    };
  }

  auto shapeSetTexture(const any& _, const any& s, const any& t) -> any {
    return [=]() -> any {
      auto& shape = cast_managed<sf::Shape>(s);
      auto& texture = cast_managed<sf::Texture>(t);
      shape.setTexture(&texture);
    };
  }

  auto transformableSetRotation(const any& _, const any& t, const double rotation) -> any {
    return [=]() -> any {
      auto& transformable = cast_managed<sf::Shape>(t);
      transformable.setRotation(rotation);
    };
  }

  auto transformableSetScale(const any& _, const any& t, const double scaleX, const double scaleY) -> any {
    return [=]() -> any {
      auto& transformable = cast_managed<sf::Shape>(t);
      transformable.setScale(scaleX, scaleY);
    };
  }

  auto transformableSetOrigin(const any& i, const any& t, const double x, const double y) -> any {
    return [=]() -> any {
      auto& transformable = cast_managed<sf::Shape>(t);
      transformable.setOrigin(x, y);
    };
  }

  auto transformableMove(const any& _, const any& t, const double offsetX, const double offsetY) -> any {
    return [=]() -> any {
      auto& transformable = cast_managed<sf::Shape>(t);
      transformable.move(offsetX, offsetY);
    };
  }

  auto eventIsClosed(const any& e) -> any {
    return [=]() -> any {
      auto& event = cast_managed<sf::Event>(e);
      return event.type == sf::Event::Closed;
    };
  }

  auto windowIsOpen(const any& w) -> any {
    return [=]() -> any {
      auto& window = cast_managed<sf::RenderWindow>(w);
      return window.isOpen();
    };
  }

  auto windowPollEvent(const any& w, const any& f) -> any {
    return [=]() -> any {
      auto& window = cast_managed<sf::RenderWindow>(w);
      auto event = make_managed<sf::Event>();
      while (window.pollEvent(*event)) {
        f(event)();
      }
    };
  }

  auto windowDraw(const any& _, const any& w, const any& d) -> any {
    return [=]() -> any {
      auto& window = cast_managed<sf::RenderWindow>(w);
      auto& drawable = cast_managed<sf::Drawable>(d);
      window.draw(drawable);
    };
  }

  auto windowClear(const any& w, const any& c) -> any {
    return [=]() -> any {
      auto& window = cast_managed<sf::RenderWindow>(w);
      auto& color = cast_managed<sf::Color>(c);
      window.clear(color);
    };
  }

  auto windowDisplay(const any& w) -> any {
    return [=]() -> any {
      auto& window = cast_managed<sf::RenderWindow>(w);
      window.display();
    };
  }

  auto windowClose(const any& w) -> any {
    return [=]() -> any {
      auto& window = cast_managed<sf::RenderWindow>(w);
      window.close();
    };
  }

  auto loop(const any& iteration, const any& input) -> any {
    return [=]() -> any {
      any result = iteration(input)();
      bool cont = result.at("continue");
      while(cont) {
        result = iteration(result)();
        cont = result.at("continue");
      }
    };
  }
}
```

At the top of the file we include/import our dependencies.
This is where we import the SFML graphics API.
As we did in `Main.hh`, we declare our `Main` namespace.
The `sf` namespace (that we let the compiler know we are using) is for SFML.

When writing bindings to a library, not written in PureScript for the use in PureScript, we have to decide which
functions are pure
(the function always returns the same output for the same input
and no side effects occur, that is, nothing outside the function is mutated or accessed)
and which ones are impure
(side effects do occur meaning something outside the function is mutated or at least accessed).

We will designate `toNumber` and `easeIn` as pure.
The rest of the functions will be designate as having effects.

For impure functions, which take one or more arguments/parameters (an arity of one or more),
we will have to use a C++11 lambda (an anonymous function, that is, a function with no identifier).

```cpp
  auto windowClose(const any& w) -> any {
    return [=]() -> any {
      auto& window = cast_managed<sf::RenderWindow>(w);
      window.close();
    };
  }
```

For example, take a look at `windowClose`.
It takes an address to an `any` instance and returns a lambda that captures `w` by value,
makes `w` read only,
takes no arguments,
and returns the `any` type.
Note that its real return type is `void` since there is no `return` statement.

Fortunately, for foreign functions (these functions are foreign to PureScript) that take one or more parameters,
we do not have to write them as curried functions.

```cpp
  auto windowClear(const any& w) -> any {
    return [=](const any& c) -> any {
      return [=]() -> any {
        auto& window = cast_managed<sf::RenderWindow>(w);
        auto& color = cast_managed<sf::Color>(c);
        window.clear(color);
      };
    };
  }
```

For example, we do _not_ have to write `windowClear` like this (notice there is an extra lambda for `c`).

```cpp
  auto windowDraw(const any& _, const any& w, const any& d) -> any {
    return [=]() -> any {
      auto& window = cast_managed<sf::RenderWindow>(w);
      auto& drawable = cast_managed<sf::Drawable>(d);
      window.draw(drawable);
    };
  }
```

Some functions have an ignored parameter such as `windowDraw`.
In these cases, the generated C++ passes a PureScript type constraint to the function (in addition to its parameters).
For example, `windowDraw` accepts a `Drawable` type which could be a `RectangleShape`, a `Shape`, or any other type
that either directly or indirectly inherits from the `Drawable` C++ class.
This class hierarchy is implemented as a type class on the PureScript side.
As you will see later in `Main.purs`,`windowDraw` accepts any argument that is an instance of the type class (interface) `IDrawable`.
For these polymorphic functions, the type constraint parameter is ignored on the C++ side.

```cpp
shared_pointer pointer = make_managed<some_type>(params, to, constructor);
// ...
some_type instance = cast_managed<some_type>(any_reference);
```

Pure11 provides automatic memory management (garbage collection) through the use of shared pointers.
To take advantage of this, we will need to use `make_managed` and `cast_managed`.

```cpp
  auto makeTextureFromFile(const std::string path) -> any {
    return [=]() -> any {
      auto texture = make_managed<sf::Texture>();
      texture->loadFromFile(path);
      return texture;
    };
  }

  auto makeRectangleShape(const double width, const double height) -> any {
    return [=]() -> any {
      auto shape = make_managed<sf::RectangleShape>(sf::Vector2f(width, height));
      return shape;
    };
  }

  auto shapeSetTexture(const any& _, const any& s, const any& t) -> any {
    return [=]() -> any {
      auto& shape = cast_managed<sf::Shape>(s);
      auto& texture = cast_managed<sf::Texture>(t);
      shape.setTexture(&texture);
    };
  }
```

For example, take a look at `makeTextureFromFile`, `makeRectangleShape`, and `shapeSetTexture`.
Both `makeTextureFromFile` and `makeRectangleShape` use `make_managed` which creates
a shared pointer (a piece of memory that contains another memory address)
that points to an instance of whatever is in the angle brackets (`<...>`).
`shapeSetTexture` takes two `any` instances (`s` and `t`) and uses `cast_managed` to extract
from them the shared `shape` and `texture` instances.
It then sets the shape's texture using the two extracted instances.
Note that `shape` and `texture`, in `shapeSetTexture`, are the actual instances
instead of the shared pointers to the instances, hence the `auto&`.

```cpp
  auto loop(const any& iteration, const any& input) -> any {
    return [=]() -> any {
      any output = iteration(input)();
      bool cont = output.at("continue");
      while(cont) {
        output = iteration(output)();
        cont = output.at("continue");
      }
    };
  }
```

The last foreign function in `Main.cc` we will look at is `loop`.
`loop` powers our draw cycle.
Before it begins, it takes the `iteration` function it was given and feeds it some initial `input`.
It then takes the `iteration` `output` (which is a PureScript record), checks if `continue` is true, and if so, runs `iteration` again.
As long as `iteration` keeps returning `{ continue: true }`,
`loop` will continue to run `iteration` by passing its `output` as its `input`.

We could make `loop` a native PureScript function but without tail-call elimination for self-recursive monadic functions,
we would end up overflowing the stack and crashing the application.
So we will keep `loop` as a foreign function and make use of its efficient `while` loop.
Note that there is a PureScript package for "stack-safe monadic tail recursion" called
[purescript-tailrec](https://github.com/purescript/purescript-tailrec) but
it was not listed as an officially supported package for Pure11.

### Main.purs

Now that we have our C++ bindings, we can write the rest of our application in PureScript.
Make sure to copy the following into `project/src/Main.purs`.

```haskell
{-
  (C) 2017 David Lettier
  lettier.com
-}

module Main where

import Prelude
import Control.Monad.Eff (Eff)

class ITransformable a
class IDrawable a
class IShape a

foreign import data CHRONO :: !
foreign import data SF :: !

foreign import data Window :: *
foreign import data Shape :: *
foreign import data Transformable :: *
foreign import data Drawable :: *
foreign import data RectangleShape :: *
foreign import data Event :: *
foreign import data Color :: *
foreign import data Texture :: *

instance iTransformableRectangleShape :: ITransformable RectangleShape
instance iDrawableRectangleShape :: IDrawable RectangleShape
instance iShapeRectangleShape :: IShape RectangleShape

type ChronoEffect a = forall e. Eff (chrono::CHRONO | e) a
type SfEffect a = forall e. Eff (sf::SF | e) a
type AllEffects a = forall e. Eff (sf::SF, chrono::CHRONO | e) a

type IterationData =
  {
      window :: Window
    , pastTime :: Number
    , backgroundColor :: Color
    , startingRotation :: Number
    , startingScale :: Number
    , rotation :: Number
    , scale :: Number
    , bgShape :: RectangleShape
    , fgShape :: RectangleShape
    , continue :: Boolean
  }

foreign import toNumber :: Int -> Number

foreign import easeIn :: Number -> Number

foreign import timeNow :: ChronoEffect Number

foreign import makeWindow :: Int -> Int -> String -> SfEffect Window
foreign import makeColor :: Int -> Int -> Int -> Int -> SfEffect Color
foreign import makeTextureFromFile :: String -> SfEffect Texture
foreign import makeRectangleShape :: Number -> Number -> SfEffect RectangleShape

foreign import shapeSetTexture :: forall s. (IShape s) => s -> Texture -> SfEffect Unit

foreign import transformableSetRotation :: forall t. (ITransformable t) => t -> Number -> SfEffect Unit
foreign import transformableSetScale :: forall t. (ITransformable t) => t -> Number -> Number -> SfEffect Unit
foreign import transformableSetOrigin :: forall t. (ITransformable t) => t -> Number -> Number -> SfEffect Unit
foreign import transformableMove :: forall t. (ITransformable t) => t -> Number -> Number -> SfEffect Unit

foreign import eventIsClosed :: Event -> SfEffect Boolean

foreign import windowIsOpen :: Window -> SfEffect Boolean
foreign import windowPollEvent :: Window -> (Event -> SfEffect Unit) -> SfEffect Unit
foreign import windowClear :: Window -> Color -> SfEffect Unit
foreign import windowDisplay :: Window -> SfEffect Unit
foreign import windowDraw :: forall d. (IDrawable d) => Window -> d -> SfEffect Unit
foreign import windowClose :: Window -> SfEffect Unit

foreign import loop ::
      forall r.
      ({continue :: Boolean | r} -> AllEffects {continue :: Boolean | r})
  ->  {continue :: Boolean | r}
  ->  AllEffects Unit

main :: AllEffects Unit
main = do
  window <- makeWindow windowWidth windowHeight "PureScript Pure11 C++11 Animation - Lettier.com"
  startAppLoop window

windowWidth :: Int
windowWidth = 600

windowHeight :: Int
windowHeight = 600

windowWidthN :: Number
windowWidthN = toNumber windowWidth

windowHeightN :: Number
windowHeightN = toNumber windowHeight

startAppLoop :: Window -> AllEffects Unit
startAppLoop window = do
  let textureWidth = 500.0
  let textureHeight = textureWidth

  let startingRotation = 0.0
  let startingScale = 0.0
  let rotation = startingRotation
  let scale = startingScale
  let scaleTemp = easeIn scale

  texture0 <- makeTextureFromFile "static/purescript-logo.png"
  texture1 <- makeTextureFromFile "static/purescript-logo-inverted.png"

  shape0 <- makeRectangleShape textureWidth textureHeight
  shape1 <- makeRectangleShape textureWidth textureHeight

  shapeSetTexture shape0 texture0
  shapeSetTexture shape1 texture1

  transformableSetOrigin shape0 (textureWidth / 2.0) (textureHeight / 2.0)
  transformableSetOrigin shape1 (textureWidth / 2.0) (textureHeight / 2.0)

  transformableMove shape0 (windowWidthN / 2.0) (windowHeightN / 2.0)
  transformableMove shape1 (windowWidthN / 2.0) (windowHeightN / 2.0)

  transformableSetRotation shape0 rotation
  transformableSetRotation shape1 rotation

  transformableSetScale shape0 scaleTemp scaleTemp
  transformableSetScale shape1 scaleTemp scaleTemp

  let bgShape = shape1
  let fgShape = shape0

  backgroundColor <- makeColor 29 34 45 255
  presentTime <- timeNow

  loop
    iteration
    {
        window: window
      , pastTime: presentTime
      , backgroundColor: backgroundColor
      , startingRotation: startingRotation
      , startingScale: startingScale
      , rotation: rotation
      , scale: scale
      , bgShape: bgShape
      , fgShape: fgShape
      , continue: true
    }

iteration ::
      IterationData
  ->  AllEffects IterationData
iteration
  {
      window: window
    , pastTime: pastTime
    , backgroundColor: backgroundColor
    , startingRotation: startingRotation
    , startingScale: startingScale
    , rotation: rotation
    , scale: scale
    , bgShape: bgShape
    , fgShape: fgShape
    , continue: continue
  }
  = do
  presentTime <- timeNow
  let diff = presentTime - pastTime
  let delta = diff / 1000.0

  let rotation' = ((60.0 * delta) + (if rotation >= 360.0 || scale > 34.0 then startingRotation else rotation))
  let scale' = ((8.0 * delta) + (if scale > 34.0 then startingScale else scale))
  let scaleTemp = easeIn scale'

  let bgShape' = if scale > 34.0 then fgShape else bgShape
  let fgShape' = if scale > 34.0 then bgShape else fgShape

  transformableSetRotation fgShape' rotation'
  transformableSetScale    fgShape' scaleTemp scaleTemp

  windowPollEvent window (\ event -> do
      isClosed <- eventIsClosed event
      if isClosed
        then do
          windowClose window
          pure unit
        else pure unit
    )

  let output =
        {
            window: window
          , pastTime: presentTime
          , backgroundColor: backgroundColor
          , startingRotation: startingRotation
          , startingScale: startingScale
          , rotation: rotation'
          , scale: scale'
          , bgShape: bgShape'
          , fgShape: fgShape'
          , continue: true
        }

  isOpen <- windowIsOpen window
  if not isOpen
    then pure (output { continue = false })
    else do
      render
        {
            window: window
          , backgroundColor: backgroundColor
          , bgShape: bgShape'
          , fgShape: fgShape'
        }

      pure output

render ::
      forall r.
      {
          window :: Window
        , backgroundColor :: Color
        , bgShape :: RectangleShape
        , fgShape :: RectangleShape
        | r
      }
  ->  SfEffect Unit
render
  {
      window: window
    , backgroundColor: backgroundColor
    , bgShape: bgShape
    , fgShape: fgShape
  }
  = do
  windowClear window backgroundColor
  windowDraw window bgShape
  windowDraw window fgShape
  windowDisplay window
```

At the top of the file, we define our `Main` module, import the `Prelude`, and import the `Eff` type constructor.
`Eff` takes a row of effects (like the `SF` effect you will see later) and a return type.
These row of effects are checked and if the compiler detects a called function with an effect not listed, it will raise an error.

As we mentioned earlier, some of our foreign functions are polymorphic (they accept different types that share an interface).
To facilitate them, we define the three types classes `ITransformable`, `IDrawable`, and `IShape`.
The `I` prefix indicates they are an interface.

```haskell
foreign import data CHRONO :: !
foreign import data SF :: !
```

We define two custom effects called `SF` and `CHRONO` which are named after the namespaces we use in `Main.cc`.
Note that we could call them something else but these names make it clear.

```haskell
foreign import data Window :: *
foreign import data Shape :: *
foreign import data Transformable :: *
foreign import data Drawable :: *
foreign import data RectangleShape :: *
foreign import data Event :: *
foreign import data Color :: *
foreign import data Texture :: *
```

After the custom effects, we define our foreign data types that are returned by our foreign functions.

```haskell
instance iTransformableRectangleShape :: ITransformable RectangleShape
instance iDrawableRectangleShape :: IDrawable RectangleShape
instance iShapeRectangleShape :: IShape RectangleShape
```

Keeping in line with SFML's class hierarchy, we define three type class instances.
For example, `RectangleShape` an instance of `ITransformable` since it has the interface methods of the `sf::Transformable` C++ class.

```haskell
type ChronoEffect a = forall e. Eff (chrono::CHRONO | e) a
type SfEffect a = forall e. Eff (sf::SF | e) a
type AllEffects a = forall e. Eff (sf::SF, chrono::CHRONO | e) a
```

Out of convenience, we define three type synonyms to help describe the various types of side-effect having functions we use.
The `ChronoEffect` is for impure functions that support the `CHRONO` effect,
the `SfEffect` is for impure functions that support the `SF` effect.
and the `AllEffects` is for impure functions that support the `SF` effect, the `CHRONO` effect, or both.
By listing the effects, we can control what effects are allowed to occur.

The `forall e. (... | e)` portion has to do with "extensible effects."
By using this construction, we can call functions that use `SF` and/or `CHRONO` from functions that use `AllEffects`.
We cannot, however, call functions that use `ChronoEffect` from functions that use `SfEffect` and vice versa.
If we wanted to, we could remove the `e` from `AllEffects` and `Main.purs` would still compile.
It would not compile, however, if we removed it from say `SfEffect` because
there would be no way to match `(sf :: SF)` with `(chrono :: CHRONO | e)` and make it look like
`(sf :: SF , chrono :: CHRONO | e)` which is `AllEffects`.

```haskell
type IterationData =
  {
      window :: Window
    , pastTime :: Number
    , backgroundColor :: Color
    , startingRotation :: Number
    , startingScale :: Number
    , rotation :: Number
    , scale :: Number
    , bgShape :: RectangleShape
    , fgShape :: RectangleShape
    , continue :: Boolean
  }
```

`IterationData` is a record used to hold the state of the application.
`loop` will continuously feed this record to `iteration` until `iteration` returns `continue` as false.

```haskell
foreign import toNumber :: Int -> Number

foreign import easeIn :: Number -> Number

foreign import timeNow :: ChronoEffect Number

foreign import makeWindow :: Int -> Int -> String -> SfEffect Window
foreign import makeColor :: Int -> Int -> Int -> Int -> SfEffect Color
foreign import makeTextureFromFile :: String -> SfEffect Texture
foreign import makeRectangleShape :: Number -> Number -> SfEffect RectangleShape

foreign import shapeSetTexture :: forall s. (IShape s) => s -> Texture -> SfEffect Unit

foreign import transformableSetRotation :: forall t. (ITransformable t) => t -> Number -> SfEffect Unit
foreign import transformableSetScale :: forall t. (ITransformable t) => t -> Number -> Number -> SfEffect Unit
foreign import transformableSetOrigin :: forall t. (ITransformable t) => t -> Number -> Number -> SfEffect Unit
foreign import transformableMove :: forall t. (ITransformable t) => t -> Number -> Number -> SfEffect Unit

foreign import eventIsClosed :: Event -> SfEffect Boolean

foreign import windowIsOpen :: Window -> SfEffect Boolean
foreign import windowPollEvent :: Window -> (Event -> SfEffect Unit) -> SfEffect Unit
foreign import windowClear :: Window -> Color -> SfEffect Unit
foreign import windowDisplay :: Window -> SfEffect Unit
foreign import windowDraw :: forall d. (IDrawable d) => Window -> d -> SfEffect Unit
foreign import windowClose :: Window -> SfEffect Unit

foreign import loop ::
      forall r.
      ({continue :: Boolean | r} -> AllEffects {continue :: Boolean | r})
  ->  {continue :: Boolean | r}
  ->  AllEffects Unit
```

Here we import our foreign functions into PureScript that are defined in `Main.cc`.
For the impure functions, you can see how we use the `Eff` monad type synonyms.
Remember that `toNumber` and `easeIn` are pure and thus do not use the `Eff` monad.

```haskell
foreign import transformableSetRotation :: forall t. (ITransformable t) => t -> Number -> SfEffect Unit
```

For the polymorphic functions, you can see the type constraints

```haskell
forall instanceOf. (TypeClass instanceOf) => instanceOf -> ...
```

we talked about earlier.

```haskell
foreign import loop ::
      forall r.
      ({continue :: Boolean | r} -> AllEffects {continue :: Boolean | r})
  ->  {continue :: Boolean | r}
  ->  AllEffects Unit
```

Here you see the PureScript import for `loop` which uses the "extensible records" syntax.
The import says that `loop`:

* accepts
    * a function that
        * accepts a record that contains at least a field called `continue`
        * has/supports `AllEffects`
        * returns a record that contains at least a field called `continue`
    * a record that contains at least a field called `continue`
* has `AllEffects`
* returns `Unit` (nothing)

```haskell
main :: AllEffects Unit
main = do
  window <- makeWindow windowWidth windowHeight "PureScript Pure11 C++11 Animation - Lettier.com"
  startAppLoop window
```

`main` is the entry point for our application.
This is where we create the window and start the application loop.

```haskell
startAppLoop :: Window -> AllEffects Unit
startAppLoop window = do

  -- ...

  loop
    iteration
    {
        window: window
      , pastTime: presentTime
      , backgroundColor: backgroundColor
      , startingRotation: startingRotation
      , startingScale: startingScale
      , rotation: rotation
      , scale: scale
      , bgShape: bgShape
      , fgShape: fgShape
      , continue: true
    }
```

`startAppLoop` initializes the state of our application and calls `loop` with the `interation` function and the initial state.
The initial state contains our two shapes that, when rendered, show the two images we downloaded earlier.

```haskell
iteration ::
      IterationData
  ->  AllEffects IterationData
iteration
  -- ...
  = do

  presentTime <- timeNow
  let diff = presentTime - pastTime
  let delta = diff / 1000.0

  let rotation' = ((60.0 * delta) + (if rotation >= 360.0 || scale > 34.0 then startingRotation else rotation))
  let scale' = ((8.0 * delta) + (if scale > 34.0 then startingScale else scale))
  let scaleTemp = easeIn scale'
```

`iteration` starts off by calculating the amount of time that has passed since it was last called.
If no time has passed, `delta` will be zero.
However, if say a second has gone by, `delta` will be one.

We need this `delta` to produce a time-based animation versus say a frame-based animation.
A time-based animation is preferred because our program will run at different frame rates or frames per second
depending on the available resources of the machine it is running on.
By making the animation time-based, the animation will be fairly consistent no matter what the frame rate is.

Every second (1000 milliseconds), we want the rotation to increase by 60 degrees and the scale to increase by eight times.
Working out the math, you can see how `delta` helps determine the proportion of increase needed for this frame or iteration.
If two seconds have passed, we need to update the rotation by 120 degrees and the scale by 16.
If half of a second has passed, we need to update the rotation by 30 degrees and the scale by four.
And so on and so forth.

```haskell
  let bgShape' = if scale > 34.0 then fgShape else bgShape
  let fgShape' = if scale > 34.0 then bgShape else fgShape

  transformableSetRotation fgShape' rotation'
  transformableSetScale    fgShape' scaleTemp scaleTemp
```

Our animation works by only operating on the foreground image while leaving the background image in the state it
was in just before we flipped it from foreground to background.
The number 34 was found through trial and error.
Once the scale reaches 34 or greater, we make the foreground image the background image and vice versa.

Before we render the images to the screen, we update the rotation and scale of the foreground shape/image.

```haskell
  windowPollEvent window (\ event -> do
      isClosed <- eventIsClosed event
      if isClosed
        then do
          windowClose window
          pure unit
        else pure unit
    )
```

Here you see our event loop where we handle the _close-the-window_ event after the user attempts to close the window.

``` haskell
  let output = -- ...

  isOpen <- windowIsOpen window
  if not isOpen
    then pure (output { continue = false })
    else do
      render
        {
            window: window
          , backgroundColor: backgroundColor
          , bgShape: bgShape'
          , fgShape: fgShape'
        }

      pure output
```

If the window is closed, we return `continue` as false to `loop` and the program will exit.
Otherwise, we render the background and foreground shapes to the window and return to `loop` the updated state of our program.

```haskell
render ::
      forall r.
      -- ...
  ->  SfEffect Unit
render
  {
      window: window
    , backgroundColor: backgroundColor
    , bgShape: bgShape
    , fgShape: fgShape
  }
  = do
  windowClear window backgroundColor
  windowDraw window bgShape
  windowDraw window fgShape
  windowDisplay window
```

The render function is tasked with updating the final composite image that is shown to the user.
Off to the side and out of view, it first fills in the image with the background color.
Like a painter, it starts with the background and works its way forward.

Once it is done drawing, it takes the finished image and swaps it with the one currently being displayed to the user.
If run fast enough, this pulls off the illusion of movement and creates our animation.

## Build and run

Now that we have the source code, we can compile our program and run it.

```bash
cd project
make GC=yes LDFLAGS="-lsfml-graphics -lsfml-window -lsfml-system"
```

Make sure you are in the root of the project before you run the `make` command.
The `LDFLAGS` are passed to the linker and are needed to use SFML.
Make sure to include the `GC=yes` flag.
If we leave it off, our program will crash by running out of memory or by
trying to access memory it does not have access to.

```bash
./output/bin/main
```

If everything went well, you should see the animation shown earlier.

## Wrap-up

Using [PureScript](http://www.purescript.org/),
the [Pure11](https://github.com/pure11) back end,
and the [Simple and Fast Multimedia Library](https://www.sfml-dev.org/),
we created a desktop GUI program that continuously renders an animation to the screen.
PureScript normally targets JavaScript but by using Pure11, we can target C++ which opens up a wide range of opportunities.
If we were to keep going with this example, we could easily create a 2D video game for the desktop.

If you enjoyed this article be sure to checkout
[Let's make a Matrix Inverse Calculator with PureScript](/posts/2017-02-25-matrix-inverse-purescript.html)
and
[Let's make a Linear Regression Calculator with PureScript](/posts/2017-01-15-linear-regression-and-the-amazing-beard.html).

