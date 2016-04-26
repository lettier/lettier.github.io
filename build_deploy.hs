{-
  David Lettier (C) 2016.
  http://www.lettier.com/
-}

{-# LANGUAGE OverloadedStrings #-}

import Data.Text (pack, concat, isSuffixOf, Text, intercalate)
import Turtle (pwd, format, fp, empty, realpath, rmtree, shell, testdir, ExitCode, FilePath)
import Control.Monad (when)

main :: IO ()
main = do
  cwd <- pwd
  let filePath = format fp cwd
  when ("lettier.github.io" `isSuffixOf` filePath) $ do
    buildAndPush
    deploy
    return ()
  return ()

buildAndPush :: IO ExitCode
buildAndPush = do
  rmSiteTree
  cabalBuild
  pushToHakyllBranch

deploy :: IO ExitCode
deploy = do
  checkoutMasterBranch
  checkoutHakyllBranchSiteTree
  rmImagesTree
  rmPostsTree
  rmCssTree
  rsyncHakyllSiteTree
  rmSiteTree
  pushToMaster
  checkoutHakyllBranch

-- Build && Push

rmSiteTree :: IO ()
rmSiteTree = rmTree "./_site"

cabalBuild :: IO ExitCode
cabalBuild = shellEmpty $ combineCommands [clean, configure, build, rebuild]
  where clean = "cabal clean "
        configure = "cabal configure"
        build = "cabal build -j"
        rebuild = "cabal run rebuild -j"

pushToHakyllBranch :: IO ExitCode
pushToHakyllBranch = shellEmpty $ combineCommands [
    update
    , commit
    , push
  ]
  where update = "git add -A"
        commit = "git commit"
        push = "git push"

-- Deploy

checkoutMasterBranch :: IO ExitCode
checkoutMasterBranch = shellEmpty "git checkout master"

checkoutHakyllBranchSiteTree :: IO ExitCode
checkoutHakyllBranchSiteTree = shellEmpty "git checkout hakyll _site"

rmImagesTree :: IO ()
rmImagesTree = rmTree "./images"

rmPostsTree :: IO ()
rmPostsTree = rmTree "./posts"

rmCssTree :: IO ()
rmCssTree = rmTree "./css"

rsyncHakyllSiteTree :: IO ExitCode
rsyncHakyllSiteTree = do
  siteDir <- realpath "./_site"
  cwd <- pwd
  let sitePath = format fp siteDir
  let currentPath = format fp cwd
  shellEmpty (Data.Text.concat [(pack "rsync -a "), sitePath, (pack "/ "), currentPath])

pushToMaster :: IO ExitCode
pushToMaster = shellEmpty $ combineCommands [
    update
    , commit
    , push
  ]
  where update = "git add -A"
        commit = "git comit --amen"
        push = "git push -f"

checkoutHakyllBranch :: IO ExitCode
checkoutHakyllBranch = shellEmpty "git checkout hakyll"

-- Util

rmTree :: Turtle.FilePath -> IO ()
rmTree str = do
  exists <- testdir str
  when exists $ do
    dir <- realpath str
    rmtree dir

shellEmpty :: Text -> IO ExitCode
shellEmpty str = shell str Turtle.empty

combineCommands :: [Text] -> Text
combineCommands [] = error "No commands given."
combineCommands (x:xs) = intercalate " && " $ x : xs
