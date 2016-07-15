---
title: Building a Haskell Web API
jumbotron_image: /images/2016-07-15-building-a-haskell-web-api/jumbotron_image.jpg
preview_image: /images/2016-07-15-building-a-haskell-web-api/preview_image.jpg
description: Using the Snap framework, we build a web server API that creates, reads, updates, and deletes web bookmarks.
author: David Lettier
---
<!--https://www.flickr.com/photos/fccdotgov/4808162555/in/photolist-o3uW7b-ohXHeC-nEoPvx-jD9Atv-8jT5MT-8jT5iv-8jT5a2-ojZfig-8S5DNp-8jWfTy-8jT5S8-8jWeUf-8jWf3q-o3uAV7-cEE7gE-8jT4DK-8jT6dp-8jWgyu-pi65gm-8jT6tM-8jT6Be-8jT4HD-8jWgBA-G99zCa-rYU1km-rGrNS7-r31SxA-rGsaoY-rYTXqJ-rZ1Vdg-rGsnVS-rGrYXs-rYWKW4-rGrL7C-rYX7yM-rGrXcU-fz5gkM-ojGFLa-o3uM4n-8P2mKm-9XWM9Z-rGqMWu-rYXsTp-rYTtiG-rYWDie-rGqptU-rGrEt3-r31Jao-r31XUC-rWHRqJ-->
<!--https://flic.kr/p/8jT6dp-->

# Overview

In [Haskell to JavaScript](/posts/2016-07-04-haskell-to-javascript.html)
we focused our sights on building front-end software that runs in the browser.
We will now turn our gaze towards the back end where we will build a
[RESTful](https://en.wikipedia.org/wiki/Representational_state_transfer),
[CRUD](https://en.wikipedia.org/wiki/Create,_read,_update_and_delete)
[API](https://en.wikipedia.org/wiki/Application_programming_interface) for web bookmarks.

<blockquote>
In software engineering, front end and back end distinguish between the separation of
concerns between the presentation layer (the front end)--which is the interface between
the user--and the data access layer (the back end). The front and back ends may be distributed among one or more systems.
<footer>[Front and back ends, Wikipedia](https://en.wikipedia.org/wiki/Front_and_back_ends)</footer>
</blockquote>

# Setup

In order to establish our build pipeline, we will need to setup our environment with a few tools, directories, and project files.

Feel free to jump down to the
<a href="#source_code">source code</a>
portion if you are familiar with setting up a project such as this one.

## Stack

[Stack](https://github.com/commercialhaskell/stack)
will handle all of our Haskell environment needs from installing the compiler to managing our Haskell dependencies.
The instructions contain all of the [installation](http://docs.haskellstack.org/en/stable/install_and_upgrade/#installupgrade)
information you will need.

For a general Linux installation you could do:

```bash
cd Downloads
wget https://www.stackage.org/stack/linux-x86_64
tar xvzf stack-1.1.2-linux-x86_64.tar.gz
cd ~
echo 'export PATH="$HOME/Downloads/stack-1.1.2-linux-x86_64/:$PATH"' >> .bashrc
source .bashrc
# Or if you use ZSH.
echo 'export PATH="$HOME/Downloads/stack-1.1.2-linux-x86_64/:$PATH"' >> .zshrc
source .zshrc
stack
```

### Project Directory

With Stack installed, we will need to create our project directory.

```bash
cd ~
mkdir -p webBookmarks
```

We will also need to define some additional directories for our project.

```bash
cd ~/webBookmarks
mkdir -p app src
```

With the directories in place, create the empty project files.

```bash
cd ~/webBookmarks
touch app/Main.hs src/Database.hs src/Model.hs src/View.hs src/Controller.hs Setup.hs
```

### YAML

To use Stack, you will need to define a `stack.yaml` file in the root of the directory.

```bash
cd ~/webBookmarks
touch stack.yaml
```

Go ahead and open this file with your favorite text editor and copy this information into the `stack.yaml` file.

```yaml
resolver: lts-6.7
packages:
- '.'
extra-deps:
- aeson-0.8.1.1
- digestive-functors-heist-0.8.6.2
- digestive-functors-snap-0.6.1.3
- hashmap-1.3.1.1
flags: {}
extra-package-dbs: []
```

## Cabal

We will also need an `webBookmarks.cabal` file which Stack will use to build our project.
Make sure this file is located in the root of the project.

```bash
cd ~/webBookmarks
touch webBookmarks.cabal
```

Go ahead and open this file with your favorite text editor and copy this information into the `webBookmarks.cabal` file.

```yaml
name:                webBookmarks
version:             0.0.0.0
author:              David Lettier
copyright:           2016 David Lettier
category:            Web
build-type:          Simple
-- extra-source-files:
cabal-version:       >=1.10

library
  hs-source-dirs:      src
  exposed-modules:     Database
                     , Model
                     , View
                     , Controller
  ghc-options:         -ddump-minimal-imports -fwarn-unused-imports
  build-depends:       base >= 4.7 && < 5
                     , snap == 0.14.*
                     , text
                     , hashmap
                     , aeson == 0.8.1.1
                     , data-default-class
                     , monad-control
                     , monad-logger
                     , resourcet
                     , bytestring
                     , transformers
                     , persistent
                     , persistent-sqlite
                     , persistent-template
  default-language:    Haskell2010

executable webBookmarks-exe
  hs-source-dirs:      app
  main-is:             Main.hs
  ghc-options:         -threaded -rtsopts -with-rtsopts=-N -ddump-minimal-imports -fwarn-unused-imports
  build-depends:       base
                     , webBookmarks
                     , snap == 0.14.*
  default-language:    Haskell2010
```

With the `webBookmarks.cabal` in place, you can finalize the Stack setup.

```bash
cd ~/webBookmarks
stack setup
```

## Project Structure

With all of the files and directories in place you should end up with:

```bash
webBookmarks/
  app/Main.hs

  src/Database.hs
  src/Model.hs
  src/View.hs
  src/Controller.hs

  stack.yaml
  webBookmarks.cabal
  Setup.hs
```

<a href="#" name="source_code"></a>

# Source Code

Below are the main source code files for the web API software.
Be sure to read the source code comments as you follow along.

```haskell
-- Comments look like this in Haskell.
```

## Setup.hs

```haskell
import Distribution.Simple
main = defaultMain
```

`Setup.hs` is the simplest and boilerplate for this Cabal based project.

## Database.hs

The first major module is `Database.hs` located in `src/`.
It is here that we connect to the
[SQLite](https://www.sqlite.org/about.html) database, create our web bookmarks table, and provide functions for inserting, retrieving
and deleting persisted web bookmarks.

Out of convenience we will use SQLite but the
[Persistent](http://www.yesodweb.com/book/persistent) library allows
one to use other databases such as MongoDB or PostgreSQL for example.
The resulting database will be located in the `webBookmarks_default.db` file unless
the environment variable `$WEB_BOOKMARKS_DB_CONN` is set.

```haskell
{-
  David Lettier (C) 2016.
  http://www.lettier.com/
-}

{-# LANGUAGE OverloadedStrings #-} -- Language Extensions

-- We will export the following functions
-- These will be used in Main.hs and Controller.hs

module Database (
    dbMigration
  , getBookmarks
  , getBookmarkById
  , insertBookmark
  , updateBookmarkById
  , deleteBookmarkById
) where

-- Here we import our custom modules: Model and View
-- These are defined in src/Model.hs and src/View.hs

import Model
import View

-- These are our build dependencies

import System.Environment -- To ge the DB connection string

-- Deals with strings and integers

import Data.Int
import Data.Text
import Data.ByteString
import Data.ByteString.Char8
import Data.ByteString.Lazy

-- Needed for `fromMaybe`

import Data.Maybe

-- For dealing with JSON

import Data.Aeson

-- Used in the `withDbRun` type signature

import Control.Monad.Trans.Resource
import Control.Monad.Trans.Control
import Control.Monad.Logger

-- Needed for interfacing with SQLite

import Database.Persist
import Database.Persist.Class
import Database.Persist.Sqlite as DbSql

-- Gather the database connection string from the environment
-- If not set use the default

sqliteConnString :: IO Data.Text.Text
sqliteConnString = do
  maybeDbConnString <- lookupEnv "WEB_BOOKMARKS_DB_CONN"
  return $ Data.Text.pack $ fromMaybe "webBookmarks_default.db" maybeDbConnString

-- Needed for each database transaction (inserting, updating, retrieval, deleting)

withDbRun :: SqlPersistT (NoLoggingT (ResourceT IO)) b -> IO b
withDbRun command = do
  connString <- sqliteConnString
  runSqlite connString command

-- This will create our web bookmarks table if it does not already exist
-- Persistent will assist with update our table schema should our model change

dbMigration :: IO ()
dbMigration = withDbRun $ runMigration $ migrate entityDefs $ entityDef (Nothing :: Maybe Bookmark)

-- Helper function to convert the URL ID string to the needed 64 bit integer primary key

getBookmarkIdKey :: Maybe Data.ByteString.ByteString -> Key Bookmark
getBookmarkIdKey maybeIdBS = toSqlKey bookmarkIdInt64
  where
    -- If we receive `Nothing` for the ID, we will return an invalid ID of `-1`
    bookmarkIdBS = fromMaybe ("-1" :: Data.ByteString.ByteString) maybeIdBS
    -- Convert the string the needed 64 bit integer
    bookmarkIdInt64 = read (Data.ByteString.Char8.unpack bookmarkIdBS) :: Int64

-- Retrieves multiple bookmark rows from our table starting at `start` and up to the `limit`

getBookmarks :: Maybe Data.ByteString.ByteString -> Maybe Data.ByteString.ByteString -> IO [Entity Bookmark]
getBookmarks maybeLimitTo maybeOffsetBy = do
  -- If the limit and offset are `Nothing`, we will use the defaults 10 for the limit and 0 for the offset
  let limitToBS  = fromMaybe ("10" :: Data.ByteString.ByteString) maybeLimitTo
  let offsetByBS = fromMaybe ("0" :: Data.ByteString.ByteString) maybeOffsetBy
  -- Converts the strings to integers
  let limitToInt  = read (Data.ByteString.Char8.unpack limitToBS) :: Int
  let offsetByInt = read (Data.ByteString.Char8.unpack offsetByBS) :: Int
  -- The actual database call
  withDbRun $ DbSql.selectList ([] :: [Filter Bookmark]) [LimitTo limitToInt, OffsetBy offsetByInt]

getBookmarkById :: Maybe Data.ByteString.ByteString -> IO (Key Bookmark, Maybe Bookmark)
getBookmarkById maybeIdBS = do
  -- Get the bookmark primary key
  let bookmarkIdKey = getBookmarkIdKey maybeIdBS
  -- Retrieve the bookmark from the database
  maybeBookmark <- withDbRun $ DbSql.get bookmarkIdKey
  -- Return both the primary key and maybe the bookmark (if it actually exists in the database)
  return (bookmarkIdKey, maybeBookmark)

insertBookmark :: Bookmark -> IO (Key Bookmark)
-- Create a new bookmark row in the database
insertBookmark bookmark = withDbRun $ DbSql.insert bookmark

updateBookmarkById :: Maybe Data.ByteString.ByteString -> BookmarkJSON -> IO (Key Bookmark, Maybe Bookmark)
updateBookmarkById maybeIdBS bookmarkJSON = do
  let bookmarkIdKey = getBookmarkIdKey maybeIdBS
  -- Look up the bookmark in the database
  (bookmarkKeyId, maybeBookmark) <- getBookmarkById maybeIdBS
  case maybeBookmark of
    -- If the book mark does not exist, return `Nothing`
    Nothing -> return (bookmarkKeyId, Nothing)
    -- If the book mark does exist
    Just bookmark -> do
      -- Create an updated bookmark record
      let bookmarkUpdated = Bookmark {
          -- The JSON maybe not have the title so use the bookmark's current title
          bookmarkTitle = fromMaybe (bookmarkTitle bookmark) (bookmarkJSONTitle bookmarkJSON)
          -- The JSON maybe not have the URL so use the bookmark's current URL
        , bookmarkUrl = fromMaybe (bookmarkUrl bookmark) (bookmarkJSONUrl bookmarkJSON)
      }
      -- Update the bookmark's title and URL in the database
      withDbRun $ DbSql.update bookmarkKeyId [
            BookmarkTitle =. bookmarkTitle bookmarkUpdated
          , BookmarkUrl =. bookmarkUrl bookmarkUpdated
        ]
      return (bookmarkKeyId, Just bookmarkUpdated)

deleteBookmarkById :: Maybe Data.ByteString.ByteString -> IO (Key Bookmark, Maybe Bookmark)
deleteBookmarkById maybeIdBS = do
  let bookmarkIdKey = getBookmarkIdKey maybeIdBS
  -- Look up the bookmark in the database
  (bookmarkKeyId, maybeBookmark) <- getBookmarkById maybeIdBS
  case maybeBookmark of
    -- No bookmark?
    Nothing -> return (bookmarkKeyId, Nothing)
    -- Bookmark?
    Just bookmark -> do
      -- Delete the bookmark from the database
      withDbRun $ DbSql.delete bookmarkKeyId
      return (bookmarkKeyId, Just bookmark)
```

## Model.hs

The second major module is `Model.hs` located in `src/`.
It is here that we define what exactly is a bookmark according to our system.

```haskell
{-
  David Lettier (C) 2016.
  http://www.lettier.com/
-}

-- Various language extensions needed to compile Model.hs
-- Feel free to lookup each one

{-# LANGUAGE
    OverloadedStrings
  , EmptyDataDecls
  , GeneralizedNewtypeDeriving
  , MultiParamTypeClasses
  , DeriveGeneric
  , GADTs
  , TypeFamilies
  , TemplateHaskell
  , QuasiQuotes
  , FlexibleInstances
  , FlexibleContexts
  , StandaloneDeriving #-}

-- Export our `Bookmark` record (model),
-- the entity (bookmark) definition,
-- and the entity fields' setters and getters

module Model (
    Bookmark(..)
  , entityDefs
  , EntityField(..)
) where

-- Needed for encoding and decoding to/from JSON

import GHC.Generics
import Data.Aeson
import Data.Default.Class

-- Needed for generating our bookmark entity

import Database.Persist
import Database.Persist.Class
import Database.Persist.TH

-- Generates our `BookmarkEntity` instance and `Bookmark` record

share [mkPersist sqlSettings, mkSave "entityDefs"][persistLowerCase|
  Bookmark
    -- Two fields
    title String
    url   String
    deriving Show Generic
|]

-- Defines the ToJSON interface for our `Bookmark` record
-- This will take a `Bookmark` record and convert it to JSON
-- For example:
-- > let x = Bookmark {bookmarkTitle = "one", bookmarkUrl = "two"}
-- > toJSON x
-- Object (fromList [("url",String "two"),("title",String "one")])
-- > encode $ toJSON x
-- "{\"url\":\"two\",\"title\":\"one\"}"

instance ToJSON Bookmark where
  toJSON (Bookmark title url) = object ["title" .= title, "url" .= url]
```

## View.hs

The [JSON](http://www.json.org/) format will act as our view.
All API endpoints will respond with a JSON string.

```haskell
{-
  David Lettier (C) 2016.
  http://www.lettier.com/
-}

{-# LANGUAGE
    OverloadedStrings
  , DeriveGeneric #-}

-- Export our view (`BookmarkJSON`) and two helper functions for
-- turning some JSON into a `Bookmark` record or
-- turning a `Bookmark` record into a JSON string

module View (
    BookmarkJSON(..)
  , bookmarkJSONToBookmark
  , bookmarkAsJSONLBS -- LBS stands for Lazy Byte String
) where

-- Our custom Model module

import Model

-- Build dependencies

import GHC.Generics
import Data.Int
import Data.Text
import Data.ByteString
import Data.ByteString.Char8
import Data.ByteString.Lazy
import Data.Maybe
import Data.Aeson
import Data.Default.Class
import Database.Persist
import Database.Persist.Class

-- Our "view" or `BookmarkJSON` record

data BookmarkJSON = BookmarkJSON {
    bookmarkJSONTitle :: Maybe String
  , bookmarkJSONUrl :: Maybe String
} deriving (Show, Generic)

-- Here we defined how to parse a JSON string "{\"title\": \"...\", \"url\": \"...\"}"
-- into a `BookmarkJSON` record

instance FromJSON BookmarkJSON where
  parseJSON (Object v) =
    BookmarkJSON <$> v .:?  "title" -- .:? is syntax for parsing a JSON string field into Maybe String
                 <*> v .:?  "url"   -- The JSON string may not have "{\"url\": \"...\"}"
                                    -- If that is the case, `bookmarkJSONURL` will be `Nothing`

-- Here we define how to take a `BookmarkJSON` record
-- and turn it into JSON {"title": "...", "url": "..."}
-- For example:
-- > let x = BookmarkJSON {bookmarkJSONTitle = Just "one", bookmarkJSONUrl = Just "two"}
-- > toJSON x
-- Object (fromList [("url",String "two"),("title",String "one")])
-- > encode $ toJSON x
-- "{\"url\":\"two\",\"title\":\"one\"}"

instance ToJSON BookmarkJSON
  toJSON (BookmarkJSON title url) = object ["title" .= title, "url" .= url]

bookmarkJSONToBookmark :: BookmarkJSON -> Bookmark
bookmarkJSONToBookmark bookmarkJSON = Bookmark titleJSONToTitle urlJSONToUrl
  where
    -- If the JSON didn't have a title, just set the title to an empty string
    titleJSONToTitle = fromMaybe "" $ bookmarkJSONTitle bookmarkJSON
    -- If the JSON didn't have a URL, just set the title to an empty string
    urlJSONToUrl = fromMaybe "" $ bookmarkJSONUrl bookmarkJSON

bookmarkAsJSONLBS :: Key Bookmark -> Bookmark -> Data.ByteString.Lazy.ByteString
-- Convert a bookmark primary key and `Bookmark` record to a JSON lazy byte string
-- "{\"id\": 1, \"title\": \"...\", \"url\": \"...\"}"
bookmarkAsJSONLBS k b = encode . entityIdToJSON $ Entity k b
```

## Controller.hs

`Controller.hs` is the heart and soul of our web API application.
This is where we define all of the URL routes for creating, reading, updating, and deleting our web bookmarks.

```haskell
{-
  David Lettier (C) 2016.
  http://www.lettier.com/
-}

{-# LANGUAGE OverloadedStrings #-}

module Controller (
  mainRouter -- We only need to export the `mainRouter` function
             -- This is used in Main.hs
) where

import Database
import Model
import View
import Snap
import Data.Int
import Data.Text
import Data.ByteString
import Data.ByteString.Char8
import Data.ByteString.Lazy
import Data.Maybe
import Data.Aeson
import Control.Monad.IO.Class (liftIO, MonadIO)
import Database.Persist
import Database.Persist.Class

-- Here is a top level router
-- This will define the base and bookmarks routes

mainRouter :: Snap ()
mainRouter =  route [
                  (    "", writeBS "") -- Base / route
                , ("bookmarks", bookmarksRouter) -- /bookmarks route
              ]

bookmarksRouter :: Snap ()
bookmarksRouter =  route [
                  (    "", method GET    bookmarksRouteIndex)  -- Gets a list of bookmarks
                , (    "", method POST   bookmarksRouteCreate) -- Creates a new bookmark
                , ("/:id", method GET    bookmarksRouteShow)   -- Gets a single bookmark by /:id
                , ("/:id", method PUT    bookmarksRouteUpdate) -- Updates a single bookmark by /:id
                , ("/:id", method DELETE bookmarksRouteDelete) -- Deletes a single bookmark by /:id
              ]

bookmarksRouteIndex :: Snap ()
bookmarksRouteIndex = do
  -- Get the limit and start paramters (?limit=:limit&start=:start) if sent
  maybeLimitTo  <- getParam "limit"
  maybeOffsetBy <- getParam "start"
  -- Get a list or array of bookmarks from the database
  bookmarks <- liftIO $ getBookmarks maybeLimitTo maybeOffsetBy
  -- Set the content type to JSON
  -- We will be responding with JSON
  modifyResponse $ setHeader "Content-Type" "application/json"
  -- Write out the JSON response
  writeLBS $ encode $ Prelude.map entityIdToJSON bookmarks

bookmarksRouteShow :: Snap ()
bookmarksRouteShow = do
  -- We will start off assuming the bookmark could not be found
  -- This sets the HTTP status code to 404 (not found)
  set404AndContentType
  -- Get the ID parameter
  maybeBookmarkId <- getParam "id"
  -- Get the bookmark primary key and record
  (bookmarkIdKey, maybeBookmark) <- liftIO $ getBookmarkById maybeBookmarkId
  -- Respond with 200 if the bookmark with ID actually exists
  -- This will write out our JSON response
  resposndWithMaybeBookmark 200 bookmarkIdKey maybeBookmark

bookmarksRouteCreate :: Snap ()
bookmarksRouteCreate = do
  -- Read in the request HTTP body
  body <- readRequestBody 50000
  -- Parse the JSON request body into a `Bookmark` model (record)
  let bookmark = bookmarkJSONToBookmark $ parseBodyToBookmarkJSON body
  -- Insert the bookmark into the database
  bookmarkIdKey <- liftIO $ insertBookmark bookmark
  -- Set the content type to JSON
  modifyResponse $ setHeader "Content-Type" "application/json"
  -- Let the client know that we created a new record (201)
  -- Respond with the newly created bookmark in JSON format
  respondWithBookmark 201 bookmarkIdKey bookmark

bookmarksRouteUpdate :: Snap ()
bookmarksRouteUpdate = do
  set404AndContentType
  maybeBookmarkId <- getParam "id"
  body <- readRequestBody 50000
  -- Parse the request body into `BookmarkJSON`
  let bookmarkJSON = parseBodyToBookmarkJSON body
  -- Update the bookmark if it exists
  (bookmarkIdKey, maybeBookmark) <- liftIO $ updateBookmarkById maybeBookmarkId bookmarkJSON
  -- If the bookmark exists, tell the client OK (200)
  -- Respond with the bookmark JSON or an error message in JSON
  resposndWithMaybeBookmark 200 bookmarkIdKey maybeBookmark

bookmarksRouteDelete :: Snap ()
bookmarksRouteDelete = do
  set404AndContentType
  maybeBookmarkId <- getParam "id"
  -- Delete the bookmark in the database if it exists
  (bookmarkIdKey, maybeBookmark) <- liftIO $ deleteBookmarkById maybeBookmarkId
  -- If the bookmark exists, resond with 200 and the bookmark in JSON form
  -- Otherwise respond with 404 (not found) and an error message in JSON format
  resposndWithMaybeBookmark 200 bookmarkIdKey maybeBookmark

set404AndContentType :: Snap ()
set404AndContentType = do
  -- Set the HTTP status code to 404 (not found)
  modifyResponse $ setResponseCode 404
  -- Set the content type as JSON
  -- This will let the client know what kind of data is being returned
  -- in the HTTP response body
  modifyResponse $ setHeader "Content-Type" "application/json"

parseBodyToBookmarkJSON :: Data.ByteString.Lazy.ByteString -> BookmarkJSON
-- Parse a raw HTTP body into a `BookmarkJSON` record
parseBodyToBookmarkJSON body = fromMaybe (BookmarkJSON (Just "") (Just "")) (decode body :: Maybe BookmarkJSON)

resposndWithMaybeBookmark :: Int -> Key Bookmark -> Maybe Bookmark -> Snap()
resposndWithMaybeBookmark code bookmarkIdKey maybeBookmark = case maybeBookmark of
    -- Bookmark not found?
    Nothing -> writeBS ("{\"error\": \"Not found.\"}" :: Data.ByteString.ByteString)
    -- Bookmark found?
    -- The code is the HTTP status code
    Just bookmark -> respondWithBookmark code bookmarkIdKey bookmark

respondWithBookmark :: Int -> Key Bookmark -> Bookmark -> Snap()
respondWithBookmark code bookmarkIdKey bookmark = do
  -- Set the HTTP status code
  modifyResponse $ setResponseCode code
  -- Write out the bookmark in JSON format into the response body
  writeLBS $ bookmarkAsJSONLBS bookmarkIdKey bookmark
```

## Main.hs

At long last we arrive at the starting point for our web server API application.
It is here that we run the database migration and initialize the Snap server.

```haskell
{-
  David Lettier (C) 2016.
  http://www.lettier.com/
-}

module Main where

import Database
import Controller
import Snap

main :: IO ()
main = do
  -- Create or modify the bookmark database table
  dbMigration
  -- Begin serving all HTTP requests
  quickHttpServe mainRouter
```

# Build and Run

With the source code in place, we can finally build and run our project.

```bash
cd ~/webBookmarks
stack build
.stack-work/dist/x86_64-linux/Cabal-1.22.5.0/build/webBookmarks-exe/webBookmarks-exe -p 55555
Listening on http://0.0.0.0:55555/

[15/Jul/2016:00:00:0 -0000] Server.httpServe: START, binding to [http://0.0.0.0:55555/]
```

# Manual Testing

Normally you would write automated tests to ensure the correctness of the API but for now we
will manually test it using [cURL](https://curl.haxx.se/docs/manpage.html)
or if you like, the [Postman](https://www.getpostman.com/) application.

## POST /bookmarks

![](/images/2016-07-15-building-a-haskell-web-api/post.png){.post-img .post-img-small .post-img-limit}

## GET /bookmarks

![](/images/2016-07-15-building-a-haskell-web-api/get_index.png){.post-img .post-img-small .post-img-limit}

## PUT /bookmarks/1

![](/images/2016-07-15-building-a-haskell-web-api/put.png){.post-img .post-img-small .post-img-limit}

## GET /bookmarks/1

![](/images/2016-07-15-building-a-haskell-web-api/get_show.png){.post-img .post-img-small .post-img-limit}

## DELETE /bookmarks/1

![](/images/2016-07-15-building-a-haskell-web-api/delete.png){.post-img .post-img-small .post-img-limit}

## GET /bookmarks

![](/images/2016-07-15-building-a-haskell-web-api/get_index_empty.png){.post-img .post-img-small .post-img-limit}

# Wrap-up

Using Persistent and the
[Snap Haskell web framework](http://snapframework.com/)
, we built a RESTful CRUD API for creating, reading, updating, and deleting
web bookmarks. We followed the
[MVC architecture pattern](https://en.wikipedia.org/wiki/Model%E2%80%93view%E2%80%93controller)
and split our application into `Main.hs`, `Database.hs`, `Model.hs`,
`View.hs`, and `Controller.hs`.
