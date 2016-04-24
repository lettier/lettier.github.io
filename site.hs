{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Data.Maybe (fromMaybe)
import qualified Data.Map as M
import           Hakyll

main :: IO ()
main = hakyll $ do
  match ("images/*" .||. "images/*/*") $ do
    route   idRoute
    compile copyFileCompiler

  match "css/fonts/*" $ do
    route   idRoute
    compile copyFileCompiler

  match "css/*" $ do
    route   idRoute
    compile compressCssCompiler

  match "posts/*" $ do
    route $ setExtension "html"
    compile $ pandocCompiler
      >>= loadAndApplyTemplate "templates/post.html"    postCtx
      >>= saveSnapshot "content"
      >>= loadAndApplyTemplate "templates/default.html" postCtx
      >>= relativizeUrls

  create ["posts.html"] $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll "posts/*"
      let count = length posts
      let postsCtx = mconcat [
              listField "posts" postCtx (return posts)
              , constField "title" ((show count) ++ " Post" ++ (plural count) ++ " and Counting")
              , titleField "title"
              , defaultContext
            ]
      makeItem ""
        >>= loadAndApplyTemplate "templates/posts.html" postsCtx
        >>= loadAndApplyTemplate "templates/default.html" postsCtx
        >>= relativizeUrls

  create ["index.html"] $ do
    route idRoute
    compile $ do
      post <- fmap head . recentFirst =<< loadAllSnapshots "posts/*" "content"
      loadAndApplyTemplate "templates/default.html" postCtx post
        >>= relativizeUrls
        >>= changeIdentifier "index.html"

  match "templates/*" $ compile templateCompiler

changeIdentifier :: Identifier -> Item a -> Compiler (Item a)
changeIdentifier idt item = return (itemSetIdentifier idt item)

itemSetIdentifier :: Identifier -> Item a -> Item a
itemSetIdentifier x (Item _ i) = Item x i

postCtx :: Context String
postCtx = mconcat [
    metadataField
    , dateField "date" "%Y/%m/%d"
    , defaultContext
  ]

plural :: Int -> String
plural count
  | count > 1  = "s"
  | count <= 1 = ""
