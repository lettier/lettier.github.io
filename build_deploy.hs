{-
  David Lettier (C) 2016.
  http://www.lettier.com/
-}

{-# LANGUAGE OverloadedStrings #-}

import Data.Text (pack, concat, isSuffixOf)
import Turtle (pwd, format, fp, empty, realpath, rmtree, shell, testdir)
import Control.Monad (when)

main = do
  cwd <- pwd
  let filePath = format fp cwd
  when ("lettier.github.io" `isSuffixOf` filePath) $
    do
      buildAndPush
      deploy
      return ()

  return ()

buildAndPush = do
  rmSiteTree
  cabalBuild
  pushToHakyll

deploy = do
  checkoutMaster
  checkoutHakyllSiteTree
  rmImagesTree
  rmPostsTree
  rmCssTree
  rsyncHakyllSiteTree
  rmSiteTree
  pushToMaster
  checkoutHakyll

-- Build && Push

rmSiteTree = rmTree "./_site"

cabalBuild = shellEmpty "cabal clean && cabal configure && cabal build -j && cabal run rebuild -j"

pushToHakyll = shellEmpty "git add -u && git add -A && git commit --amen && git push -f"

-- Deploy

checkoutMaster = shellEmpty "git checkout master"

checkoutHakyllSiteTree = shellEmpty "git checkout hakyll _site"

rmImagesTree = rmTree "./images"
rmPostsTree = rmTree "./posts"
rmCssTree = rmTree "./css"

rsyncHakyllSiteTree = do
  siteDir <- realpath "./_site"
  cwd <- pwd
  let sitePath = format fp siteDir
  let currentPath = format fp cwd
  shellEmpty (Data.Text.concat [(pack "rsync -a "), sitePath, (pack "/ "), currentPath])

pushToMaster = shellEmpty "git add -u && git add -A && git commit --amen && git push -f"

checkoutHakyll = shellEmpty "git checkout hakyll"

-- Util

rmTree str = do
  exists <- testdir str
  when exists $
    do
      dir <- realpath str
      rmtree dir

shellEmpty str = shell str Turtle.empty
