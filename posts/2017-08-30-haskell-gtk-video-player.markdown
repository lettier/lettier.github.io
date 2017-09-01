---
title: Let's make a GTK Video Player with Haskell
jumbotron_image: /images/2017-08-30-haskell-gtk-video-player/jumbotron_image.jpg
preview_image: /images/2017-08-30-haskell-gtk-video-player/preview_image.jpg
description: Using Haskell, GTK+, GStreamer, xlib, and the haskell-gi bindings, we build a video player complete with seek and volume controls.
author: David Lettier
---
<!--https://pixabay.com/en/wild-outdoors-landscape-nature-865296/-->

## Overview

![The UI of Movie Monad showing [Sintel from the Blender Foundation](https://durian.blender.org/).](
/images/2017-08-30-haskell-gtk-video-player/ui.jpg
){.post-img .post-img-limit}

When we last left off with [Movie Monad](/posts/2016-08-15-making-movie-monad.html),
we had built a desktop video player using all web-based technologies (HTML, CSS, JavaScript, and Electron).
The twist was that all of the source code for the project was written in Haskell.

One of the limitations of our web-based approach was that the video file size could only be so large.
If the file size was too large, it would crash the application.
To avoid this, we put in a file size check and told the user if the video file was too large.

We could continue with the web-based approach and setup a back-end server to stream the video file
to the HTML5 player and run the server and the Electron application side-by-side.
Instead we will go with a non-web-based approach using GTK+, GStreamer, and the X11 windowing system.
Note that if you use another windowing system, such as Wayland, Quartz, or WinAPI, this approach can be adapted to work
with your particular GDK back-end.
The adaptation part being the embedding of the
[GStreamer playbin](
https://gstreamer.freedesktop.org/data/doc/gstreamer/head/gst-plugins-base-plugins/html/gst-plugins-base-plugins-playbin.html
) video output into the Movie Monad window.

<blockquote>
GDK is an important part of GTK+'s portability.
Since low-level cross-platform functionality is already provided by GLib,
all that is needed to make GTK+ run on other platforms is to port GDK
to the underlying operating system's graphics layer.
Hence, the GDK ports to the Windows API and Quartz
are what makes GTK+ applications run on Windows and macOS, respectively.
<footer>[GDK - Wikipedia](https://en.wikipedia.org/wiki/GDK)</footer>
</blockquote>

## Who this is for

* Haskell programmers looking to make a GTK+ user interface (UI)
* Programmers interested in functional programming
* GUI builders
* Those looking for an alternative to GitHub's Electron
* Video player aficionados

## What we will cover

* Stack
* The haskell-gi bindings
* Cabal data directory and data files
* Glade
* GTK+
* GStreamer
* How to build Movie Monad

## Project setup

Before we can begin, we will need our machine setup to develop Haskell programs
and our project directory setup with its files and dependencies.

### Haskell Platform

If your machine in not already setup to develop Haskell programs, you can obtain all that we will need by
downloading and installing the [Haskell Platform](https://www.haskell.org/platform/).

### Stack

If you are setup to develop with Haskell but do not have [Stack](https://docs.haskellstack.org/en/stable/README/),
make sure to get Stack installed before you begin.
Note that if you used the Haskell Platform, you should already have Stack.

### ExifTool

Before we can play a video in Movie Monad, we will need to gather some details about the file the user selected.
We will be using
[ExifTool](https://www.sno.phy.queensu.ca/~phil/exiftool/install.html)
to gather these details.
If you are using some Linux distribution, there is a good chance that you already have it (`which exiftool`).
ExifTool is available for Windows, Mac, and Linux.

### Project files

There are three ways you can obtain the project files.

```bash
wget https://github.com/lettier/movie-monad/archive/master.zip
unzip master.zip
mv movie-monad-master movie-monad
cd movie-monad/
```
You can download the [ZIP](https://github.com/lettier/movie-monad/archive/master.zip) and extract it.

```bash
git clone git@github.com:lettier/movie-monad.git
cd movie-monad/
```

You can Git clone it with SSH.

```bash
git clone https://github.com/lettier/movie-monad.git
cd movie-monad/
```

You can Git clone it with HTTPS.

### haskell-gi

[haskell-gi](https://github.com/haskell-gi/haskell-gi) is capable of generating Haskell bindings
for libraries that use the
[GObject introspection middleware](
https://wiki.gnome.org/Projects/GObjectIntrospection
).
At the time of this writing, certain bindings we will need are not available on
[Hackage](https://hackage.haskell.org/packages/search?terms=gi).
Thus we will use haskell-gi to generate these bindings for us.
For now, let us install haskell-gi.

```bash
cd movie-monad/
stack setup
stack install haskell-gi
```

### xlib

We will need a binding to the xlib library.

```bash
cd movie-monad/
haskell-gi -o lib/gi-xlib/xlib.overrides -O lib/gi-xlib xlib
```

### GdkX11-3.0

We will need a binding to the GdkX11 library.

```bash
cd movie-monad/
haskell-gi -o lib/gi-gdkx11/GdkX11.overrides -O lib/gi-gdkx11 GdkX11-3.0
```

### Dependencies

Go ahead now and install the project dependencies.

```bash
cd movie-monad/
stack install --dependencies-only
```

## The code

We are now setup to implement Movie Monad.
You can either delete the source files and recreate it them or just follow along.

### Paths_movie_monad.hs

`Paths_movie_monad.hs` is used to find our [Glade](https://glade.gnome.org/) XML GUI file at runtime.
While we are developing, we use a dummy module (`movie-monad/src/dev/Paths_movie_monad.hs`) to find the
`movie-monad/src/data/gui.glade` file.
After we build/install the project, the real `Paths_movie_monad` module is auto generated.
This auto generated module provides us with the `getDataFileName` function.
`getDataFileName` prefixes its input with the absolute path to where the
`data-dir` (`movie-monad/src/`) `data-files` were copied or installed to.

```haskell
{-# LANGUAGE OverloadedStrings #-}

module Paths_movie_monad where

dataDir :: String
dataDir = "./src/"

getDataFileName :: FilePath -> IO FilePath
getDataFileName a = do
  putStrLn "You are using a fake Paths_movie_monad."
  return (dataDir ++ "/" ++ a)
```

The dummy `Paths_movie_monad` module.

```haskell
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_movie_monad (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,0,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/<snip>/.stack-work/install/x86_64-linux-nopie/lts-9.1/8.0.2/bin"
libdir     = "/home/<snip>/.stack-work/install/x86_64-linux-nopie/lts-9.1/8.0.2/lib/x86_64-linux-ghc-8.0.2/movie-monad-0.0.0.0"
dynlibdir  = "/home/<snip>/.stack-work/install/x86_64-linux-nopie/lts-9.1/8.0.2/lib/x86_64-linux-ghc-8.0.2"
datadir    = "/home/<snip>/.stack-work/install/x86_64-linux-nopie/lts-9.1/8.0.2/share/x86_64-linux-ghc-8.0.2/movie-monad-0.0.0.0"
libexecdir = "/home/<snip>/.stack-work/install/x86_64-linux-nopie/lts-9.1/8.0.2/libexec"
sysconfdir = "/home/<snip>/.stack-work/install/x86_64-linux-nopie/lts-9.1/8.0.2/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "movie_monad_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "movie_monad_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "movie_monad_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "movie_monad_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "movie_monad_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "movie_monad_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
```

The auto generated `Paths_movie_monad` module.

### Main.hs

`Main.hs` is the entry point for Movie Monad.
In this file we setup our window with its various widgets, we wire up GStreamer, and we teardown our window once the user exits.

#### Pragmas

We need to tell the compiler (GHC) that we want overloaded strings and lexically scoped type variables.
`OverloadedStrings` allows us to use string literals (`"Literal"`) in places that demand `String/[Char]` or `Text`.
`ScopedTypeVariables` allows us to use a type signature in the parameter pattern of the lambda function
passed to catch when calling ExifTool.

```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
```

#### Imports

```haskell
module Main where

import Prelude
import Foreign.C.Types
import System.Process
import System.Exit
import Control.Monad
import Control.Exception
import Data.Maybe
import Data.Int
import Data.Text
import Data.String.Utils
import Data.GI.Base
import Data.GI.Base.Properties
import GI.GLib
import GI.GObject
import qualified GI.Gtk
import GI.Gst
import GI.GstVideo
import GI.GdkX11
import Paths_movie_monad
```

Since we are dealing with C bindings, we will need to work with types that exist in the C language.
A large portion of the imports are the bindings generated by haskell-gi.

#### IsVideoOverlay

The GStreamer video bindings (`gi-gstvideo`) have an `IsVideoOverlay` type class (interface).
The GStreamer bindings (`gi-gst`) have an element type.
In order to use `playbin` (an element) with the function `GI.GstVideo.videoOverlaySetWindowHandle`,
we must declare `GI.Gst.Element` a type instance of `IsVideoOverlay`.
On the C side, `playbin` implements the `VideoOverlay` interface.

```haskell
newtype GstElement = GstElement GI.Gst.Element
instance GI.GstVideo.IsVideoOverlay GstElement
```

Note that we wrap `GI.Gst.Element` in a newtype to avoid an orphaned instance since we are declaring the instance outside of the
haskell-gi bindings.

#### main

`main` is our largest function where we initialize all of the GUI widgets and define callback procedures based on certain events.

```haskell
main :: IO ()
main = do
```

#### GI initialization

```haskell
  _ <- GI.Gst.init Nothing
  _ <- GI.Gtk.init Nothing
```

Here we initialize GStreamer and GTK+.

#### Building our GUI widgets

```haskell
  gladeFile <- getDataFileName "data/gui.glade"
  builder <- GI.Gtk.builderNewFromFile (pack gladeFile)

  window <- builderGetObject GI.Gtk.Window builder "window"
  fileChooserButton <- builderGetObject GI.Gtk.FileChooserButton builder "file-chooser-button"
  drawingArea <- builderGetObject GI.Gtk.Widget builder "drawing-area"
  seekScale <- builderGetObject GI.Gtk.Scale builder "seek-scale"
  onOffSwitch <- builderGetObject GI.Gtk.Switch builder "on-off-switch"
  volumeButton <- builderGetObject GI.Gtk.VolumeButton builder "volume-button"
  desiredVideoWidthComboBox <- builderGetObject GI.Gtk.ComboBoxText builder "desired-video-width-combo-box"
  errorMessageDialog <- builderGetObject GI.Gtk.MessageDialog builder "error-message-dialog"
  aboutButton <- builderGetObject GI.Gtk.Button builder "about-button"
  aboutDialog <- builderGetObject GI.Gtk.AboutDialog builder "about-dialog"
```

As described earlier, we obtain the absolute path to the `data/gui.glade` file which is a
[XML file](
https://github.com/lettier/movie-monad/blob/master/src/data/gui.glade
) describing all of our GUI widgets.
Next we create a builder from the file and acquire each of our GUI widgets.
If we didn't use Glade, we would have had to build all of these widgets manually which can become rather verbose and tedious.

#### Playbin

```haskell
  playbin <- fromJust <$> GI.Gst.elementFactoryMake "playbin" (Just "MultimediaPlayer")
```

Here we create a GStreamer pipeline called `playbin`.
This pipeline is setup to handle a wide variety of needs and saves us the time of having to build our own pipeline.
We give this element the name `MultimediaPlayer`.

#### Embedding the GStreamer output

Two bring together GTK+ and GStreamer, we need a way to tell GStreamer where to render the video to.
If we do not tell GStreamer where to render to, it will create its own window since we are using `playbin`.

```haskell
  _ <- GI.Gtk.onWidgetRealize drawingArea $ do
    gdkWindow <- fromJust <$> GI.Gtk.widgetGetWindow drawingArea
    x11Window <- GI.Gtk.unsafeCastTo GI.GdkX11.X11Window gdkWindow

    xid <- GI.GdkX11.x11WindowGetXid x11Window
    let xid' = fromIntegral xid :: CUIntPtr

    GI.GstVideo.videoOverlaySetWindowHandle (GstElement playbin) xid'
```

Here you see the callback setup for when our `drawingArea` widget is ready.
The `drawingArea` is where we want GStreamer to render to.
We obtain the parent GDK window for the drawing area widget.
Next we get the window handle or `XID` of the X11 window powering our GTK+ window.
The `CUIntPtr` line is converting the ID from `CULong` to `CUIntPtr` which `videoOverlaySetWindowHandle` expects.
Once we have the correct type, we inform GStreamer that it can render the output of `playbin` to our window with the handle `xid'`.

Note that here is where you would adapt Movie Monad to work with your windowing system if you are using something other than the
X windowing system.

#### Choosing the file

```haskell
  _ <- GI.Gtk.onFileChooserButtonFileSet fileChooserButton $ do
    _ <- GI.Gst.elementSetState playbin GI.Gst.StateNull

    filename <- fromJust <$> GI.Gtk.fileChooserGetFilename fileChooserButton
    let uri = "file://" ++ filename

    volume <- GI.Gtk.scaleButtonGetValue volumeButton
    Data.GI.Base.Properties.setObjectPropertyDouble playbin "volume" volume
    Data.GI.Base.Properties.setObjectPropertyString playbin "uri" (Just $ pack uri)

    desiredVideoWidth <- getDesiredVideoWidth desiredVideoWidthComboBox
    (success, width, height) <- getWindowSize desiredVideoWidth filename

    if success
      then do
        _ <- GI.Gst.elementSetState playbin GI.Gst.StatePlaying
        GI.Gtk.switchSetActive onOffSwitch True
        setWindowSize width height fileChooserButton drawingArea window
      else do
        _ <- GI.Gst.elementSetState playbin GI.Gst.StatePaused
        GI.Gtk.switchSetActive onOffSwitch False
        resetWindowSize desiredVideoWidth fileChooserButton drawingArea window
        _ <- GI.Gtk.onDialogResponse errorMessageDialog (\ _ -> GI.Gtk.widgetHide errorMessageDialog)
        void $ GI.Gtk.dialogRun errorMessageDialog
```

To kick off a video playing session, the user must be able to pick a video file.
When they do pick a file, we must perform some critical steps to ensure everything goes smoothly.

* Gather the file name from the file chooser widget
* Tell `playbin` what file it must play
* Set the `playbin` volume to the volume widget level
* Determine the appropriate window width and height based on the desired video width selection and the video size
* If getting the window size was a success
    * Start playing the video
    * Set the toggle play/pause button to the on state
    * Resize the window to fit the relative size of the video
* Else if getting the window size was a failure
    * Tell `playbin` to pause
    * Set the toggle switch to the off position
    * Reset the window size
    * Display a small dialog box informing the user of an error occurring

#### Play and pause

```haskell
  _ <- GI.Gtk.onSwitchStateSet onOffSwitch $ \ switchOn -> do
    if switchOn
      then void $ GI.Gst.elementSetState playbin GI.Gst.StatePlaying
      else void $ GI.Gst.elementSetState playbin GI.Gst.StatePaused
    return switchOn
```

Rather straight forward.
If the toggle switch is on, we set the `playbin` element's state to playing.
Otherwise, we set the `playbin` element's state to paused.

#### Setting the volume

```haskell
  _ <- GI.Gtk.onScaleButtonValueChanged volumeButton $
      \ volume -> void $ Data.GI.Base.Properties.setObjectPropertyDouble playbin "volume" volume
```

Whenever the volume widget level changes, we forward this level on to GStreamer so that it can adjust the video volume.

#### Seek

```haskell
  seekScaleHandlerId <- GI.Gtk.onRangeValueChanged seekScale $ do
    (couldQueryDuration, duration) <- GI.Gst.elementQueryDuration playbin GI.Gst.FormatTime

    when couldQueryDuration $ do
      percentage' <- GI.Gtk.rangeGetValue seekScale
      let percentage = percentage' / 100.0
      let position = fromIntegral (round ((fromIntegral duration :: Double) * percentage) :: Int) :: Int64
      void $ GI.Gst.elementSeekSimple playbin GI.Gst.FormatTime [ GI.Gst.SeekFlagsFlush ] position
```

Movie Monad comes with a seek scale where as you drag the slider forwards or backwards,
you move forwards or backwards through the video's frames.

The scale of the seek slider is from zero to 100 and represents the percentage of video time passed.
Advancing the slider to say 50, will move the video to the time marker that is half way between start and finish.
We could set the slider's scale to be from zero to however long the video is but this method allows us to generalize better.

Note that for this callback, we keep around the signal ID (`seekScaleHandlerId`) since we will need it later.

#### Seek Scale update

```haskell
  _ <- GI.GLib.timeoutAddSeconds GI.GLib.PRIORITY_DEFAULT 1 $ do
    (couldQueryDuration, duration) <- GI.Gst.elementQueryDuration playbin GI.Gst.FormatTime
    (couldQueryPosition, position) <- GI.Gst.elementQueryPosition playbin GI.Gst.FormatTime

    let percentage =
          if couldQueryDuration && couldQueryPosition && duration > 0
            then 100.0 * (fromIntegral position / fromIntegral duration :: Double)
            else 0.0

    GI.GObject.signalHandlerBlock seekScale seekScaleHandlerId
    GI.Gtk.rangeSetValue seekScale percentage
    GI.GObject.signalHandlerUnblock seekScale seekScaleHandlerId

    return True
```

Too keep the seek scale in sync with the video's progress, we must play messenger between GTK+ and GStreamer.
Every second, we query for the video's current position and update the seek scale to match.
This way the user can know how far along they are into the video and if they go to slide the seeker, it will
be in the correct state.

As to not trigger the callback we setup earlier, we disable the `onRangeValueChanged` signal handler while we update the seek scale.
The `onRangeValueChanged` callback should only run if the _user_ changes the seek slider.

#### Changing the video size

```haskell
  _ <- GI.Gtk.onComboBoxChanged desiredVideoWidthComboBox $ do
    filename' <- GI.Gtk.fileChooserGetFilename fileChooserButton
    let filename = fromMaybe "" filename'

    desiredVideoWidth <- getDesiredVideoWidth desiredVideoWidthComboBox
    (success, width, height) <- getWindowSize desiredVideoWidth filename

    if success
      then setWindowSize width height fileChooserButton drawingArea window
      else resetWindowSize desiredVideoWidth fileChooserButton drawingArea window
```

This widget lets the user select the desired width of the video.
The height of the window will be set based on the aspect ratio of the video and the user's width selection.

#### About

```haskell
  _ <- GI.Gtk.onWidgetButtonReleaseEvent aboutButton $ \ _ -> do
    _ <- GI.Gtk.onDialogResponse aboutDialog (\ _ -> GI.Gtk.widgetHide aboutDialog)
    void $ GI.Gtk.dialogRun aboutDialog
    return True
```

The last widget we will cover is the about dialog window.
Here we wire up the about dialog window to the about button shown on the main window.

#### Teardown

```haskell
  _ <- GI.Gtk.onWidgetDestroy window $ do
    _ <- GI.Gst.elementSetState playbin GI.Gst.StateNull
    _ <- GI.Gst.objectUnref playbin
    GI.Gtk.mainQuit
```

When the user destroys the window, destroy the `playbin` pipeline and quit the main GTK loop.

#### Startup

```haskell
  GI.Gtk.widgetShowAll window
  GI.Gtk.main
```

At long last we show or render the main window and fire up the main GTK+ loop.
This loop will block until `mainQuit` is called.

#### The entire Main.hs file

Below is the `movie-monad/src/Main.hs` file. The other portions not covered are various utility functions that dry up `main`.

```haskell
{-
  Movie Monad
  (C) 2017 David lettier
  lettier.com
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Prelude
import Foreign.C.Types
import System.Process
import System.Exit
import Control.Monad
import Control.Exception
import Data.Maybe
import Data.Int
import Data.Text
import Data.String.Utils
import Data.GI.Base
import Data.GI.Base.Properties
import GI.GLib
import GI.GObject
import qualified GI.Gtk
import GI.Gst
import GI.GstVideo
import GI.GdkX11
import Paths_movie_monad

-- Declare Element a type instance of IsVideoOverlay via a newtype wrapper
-- Our GStreamer element is playbin
-- Playbin implements the GStreamer VideoOverlay interface
newtype GstElement = GstElement GI.Gst.Element
instance GI.GstVideo.IsVideoOverlay GstElement

main :: IO ()
main = do
  _ <- GI.Gst.init Nothing
  _ <- GI.Gtk.init Nothing

  gladeFile <- getDataFileName "data/gui.glade"
  builder <- GI.Gtk.builderNewFromFile (pack gladeFile)

  window <- builderGetObject GI.Gtk.Window builder "window"
  fileChooserButton <- builderGetObject GI.Gtk.FileChooserButton builder "file-chooser-button"
  drawingArea <- builderGetObject GI.Gtk.Widget builder "drawing-area"
  seekScale <- builderGetObject GI.Gtk.Scale builder "seek-scale"
  onOffSwitch <- builderGetObject GI.Gtk.Switch builder "on-off-switch"
  volumeButton <- builderGetObject GI.Gtk.VolumeButton builder "volume-button"
  desiredVideoWidthComboBox <- builderGetObject GI.Gtk.ComboBoxText builder "desired-video-width-combo-box"
  errorMessageDialog <- builderGetObject GI.Gtk.MessageDialog builder "error-message-dialog"
  aboutButton <- builderGetObject GI.Gtk.Button builder "about-button"
  aboutDialog <- builderGetObject GI.Gtk.AboutDialog builder "about-dialog"

  playbin <- fromJust <$> GI.Gst.elementFactoryMake "playbin" (Just "MultimediaPlayer")

  _ <- GI.Gtk.onWidgetRealize drawingArea $ do
    gdkWindow <- fromJust <$> GI.Gtk.widgetGetWindow drawingArea
    x11Window <- GI.Gtk.unsafeCastTo GI.GdkX11.X11Window gdkWindow

    xid <- GI.GdkX11.x11WindowGetXid x11Window
    let xid' = fromIntegral xid :: CUIntPtr

    GI.GstVideo.videoOverlaySetWindowHandle (GstElement playbin) xid'

  _ <- GI.Gtk.onFileChooserButtonFileSet fileChooserButton $ do
    _ <- GI.Gst.elementSetState playbin GI.Gst.StateNull

    filename <- fromJust <$> GI.Gtk.fileChooserGetFilename fileChooserButton
    let uri = "file://" ++ filename

    volume <- GI.Gtk.scaleButtonGetValue volumeButton
    Data.GI.Base.Properties.setObjectPropertyDouble playbin "volume" volume
    Data.GI.Base.Properties.setObjectPropertyString playbin "uri" (Just $ pack uri)

    desiredVideoWidth <- getDesiredVideoWidth desiredVideoWidthComboBox
    (success, width, height) <- getWindowSize desiredVideoWidth filename

    if success
      then do
        _ <- GI.Gst.elementSetState playbin GI.Gst.StatePlaying
        GI.Gtk.switchSetActive onOffSwitch True
        setWindowSize width height fileChooserButton drawingArea window
      else do
        _ <- GI.Gst.elementSetState playbin GI.Gst.StatePaused
        GI.Gtk.switchSetActive onOffSwitch False
        resetWindowSize desiredVideoWidth fileChooserButton drawingArea window
        _ <- GI.Gtk.onDialogResponse errorMessageDialog (\ _ -> GI.Gtk.widgetHide errorMessageDialog)
        void $ GI.Gtk.dialogRun errorMessageDialog

  _ <- GI.Gtk.onSwitchStateSet onOffSwitch $ \ switchOn -> do
    if switchOn
      then void $ GI.Gst.elementSetState playbin GI.Gst.StatePlaying
      else void $ GI.Gst.elementSetState playbin GI.Gst.StatePaused
    return switchOn

  _ <- GI.Gtk.onScaleButtonValueChanged volumeButton $
      \ volume -> void $ Data.GI.Base.Properties.setObjectPropertyDouble playbin "volume" volume

  seekScaleHandlerId <- GI.Gtk.onRangeValueChanged seekScale $ do
    (couldQueryDuration, duration) <- GI.Gst.elementQueryDuration playbin GI.Gst.FormatTime

    when couldQueryDuration $ do
      percentage' <- GI.Gtk.rangeGetValue seekScale
      let percentage = percentage' / 100.0
      let position = fromIntegral (round ((fromIntegral duration :: Double) * percentage) :: Int) :: Int64
      void $ GI.Gst.elementSeekSimple playbin GI.Gst.FormatTime [ GI.Gst.SeekFlagsFlush ] position

  _ <- GI.GLib.timeoutAddSeconds GI.GLib.PRIORITY_DEFAULT 1 $ do
    (couldQueryDuration, duration) <- GI.Gst.elementQueryDuration playbin GI.Gst.FormatTime
    (couldQueryPosition, position) <- GI.Gst.elementQueryPosition playbin GI.Gst.FormatTime

    let percentage =
          if couldQueryDuration && couldQueryPosition && duration > 0
            then 100.0 * (fromIntegral position / fromIntegral duration :: Double)
            else 0.0

    GI.GObject.signalHandlerBlock seekScale seekScaleHandlerId
    GI.Gtk.rangeSetValue seekScale percentage
    GI.GObject.signalHandlerUnblock seekScale seekScaleHandlerId

    return True

  _ <- GI.Gtk.onComboBoxChanged desiredVideoWidthComboBox $ do
    filename' <- GI.Gtk.fileChooserGetFilename fileChooserButton
    let filename = fromMaybe "" filename'

    desiredVideoWidth <- getDesiredVideoWidth desiredVideoWidthComboBox
    (success, width, height) <- getWindowSize desiredVideoWidth filename

    if success
      then setWindowSize width height fileChooserButton drawingArea window
      else resetWindowSize desiredVideoWidth fileChooserButton drawingArea window

  _ <- GI.Gtk.onWidgetButtonReleaseEvent aboutButton $ \ _ -> do
    _ <- GI.Gtk.onDialogResponse aboutDialog (\ _ -> GI.Gtk.widgetHide aboutDialog)
    void $ GI.Gtk.dialogRun aboutDialog
    return True

  _ <- GI.Gtk.onWidgetDestroy window $ do
    _ <- GI.Gst.elementSetState playbin GI.Gst.StateNull
    _ <- GI.Gst.objectUnref playbin
    GI.Gtk.mainQuit

  GI.Gtk.widgetShowAll window
  GI.Gtk.main

builderGetObject ::
  (GI.GObject.GObject b, GI.Gtk.IsBuilder a) =>
  (Data.GI.Base.ManagedPtr b -> b) ->
  a ->
  Prelude.String ->
  IO b
builderGetObject objectTypeClass builder objectId =
  fromJust <$> GI.Gtk.builderGetObject builder (pack objectId) >>=
    GI.Gtk.unsafeCastTo objectTypeClass

getVideoInfo :: Prelude.String -> Prelude.String -> IO (Bool, Prelude.String)
getVideoInfo flag filename = do
  (code, out, _) <- catch (
      readProcessWithExitCode
        "exiftool"
        [flag, "-s", "-S", filename]
        ""
    ) (\ (_ :: Control.Exception.IOException) -> return (ExitFailure 1, "", ""))
  return (code == System.Exit.ExitSuccess, out)

isVideo :: Prelude.String -> IO Bool
isVideo filename = do
  (success, out) <- getVideoInfo "-MIMEType" filename
  return (success && isInfixOf "video" (pack out))

getWindowSize :: Int -> Prelude.String -> IO (Bool, Int32, Int32)
getWindowSize desiredVideoWidth filename = do
  let defaultWidth = 800
  let defaultHeight = 600

  video <- isVideo filename

  if video
    then do
      (success, out) <- getVideoInfo "-ImageSize" filename
      if success && isInfixOf "x" (pack out)
        then do
          let (width''':height''':_) =
                Data.String.Utils.split "x" $ Data.String.Utils.strip out

          let width'' = read width''' :: Int
          let height'' = read height''' :: Int

          let ratio = fromIntegral height'' / fromIntegral width'' :: Double
          let width' = fromIntegral desiredVideoWidth :: Double
          let height' = width' * ratio
          let width = fromIntegral (round width' :: Int) :: Int32
          let height = fromIntegral (round height' :: Int) :: Int32

          return (True, width, height)
        else return (False, defaultHeight, defaultWidth)
    else return (False, defaultHeight, defaultWidth)

getDesiredVideoWidth :: GI.Gtk.ComboBoxText -> IO Int
getDesiredVideoWidth = fmap (\ x -> read (Data.Text.unpack x) :: Int) . GI.Gtk.comboBoxTextGetActiveText

setWindowSize ::
  Int32 ->
  Int32 ->
  GI.Gtk.FileChooserButton ->
  GI.Gtk.Widget ->
  GI.Gtk.Window ->
  IO ()
setWindowSize width height fileChooserButton drawingArea window = do
  GI.Gtk.setWidgetWidthRequest fileChooserButton width

  GI.Gtk.setWidgetWidthRequest drawingArea width
  GI.Gtk.setWidgetHeightRequest drawingArea height

  GI.Gtk.setWidgetWidthRequest window width
  GI.Gtk.setWidgetHeightRequest window height
  GI.Gtk.windowResize window width (if height <= 0 then 1 else height)

resetWindowSize ::
  (Integral a) =>
  a ->
  GI.Gtk.FileChooserButton ->
  GI.Gtk.Widget ->
  GI.Gtk.Window ->
  IO ()
resetWindowSize width' fileChooserButton drawingArea window = do
  let width = fromIntegral width' :: Int32
  setWindowSize width 0 fileChooserButton drawingArea window
```

## Building Movie Monad

Now that we have setup our build environment and have all of the source code in place,
we can build Movie Monad and run the executable/binary.

```bash
cd movie-monad/
stack clean
stack build --pedantic
stack install
movie-monad # Or if you prefer `stack exec -- movie-monad`
```

If all is in order, Movie Monad should run.

## Wrap-up

Revisiting the [Movie Monad](https://github.com/lettier/movie-monad) project,
we remade the application using the software libraries GTK+ and GStreamer.
By using GTK+ and GStreamer, the application remains as portable as the Electron version.
Movie Monad can now handle large video files and comes with all of the standard controls one would expect.

Another benefit to the GTK+ approach is the smaller footprint.
Comparing the resident size in memory on start up,
the GTK+ version only requires ~50 MB while the Electron version requires ~300 MB (a 500% increase).

In the end, the GTK+ approach came with fewer limitations and required less engineering.
To offer the same functionality, the Electron approach would require a tedious client server architecture.
However, thanks to the excellent haskell-gi bindings, we were able to avoid the web-based approach altogether.

If you would like to see another GTK+ application built with Haskell,
be sure to checkout [Gifcurry](https://github.com/lettier/gifcurry).
Gifcurry allows you take video files and produce GIFs optionally overlaid with text.
