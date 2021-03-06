{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Monoid (mappend, (<>))
import Hakyll
import System.Directory
import System.Process

rbrosFeedConfiguration :: FeedConfiguration
rbrosFeedConfiguration = FeedConfiguration
    { feedTitle = "Reichert Brothers Software Development"
    , feedDescription = "Hacking and related"
    , feedAuthorName = "Christopher Reichert"
    , feedAuthorEmail = "christopher@reichertbrothers.com"
    , feedRoot = "http://reichertbrothers.com"
    }

-- There are a few limitations to the Vroom compiler
-- and I have not been able to get it working inside
-- the Hakyll compiler system. This is a very elementary
-- compilation process for Vroom slides.
vroom :: [String] -> IO ()
vroom [] = return ()
vroom (t:ts) = do
        _ <- system $ "vroom html --input=talks/hhug/" ++ t ++ ".vroom"
        _ <- system $ "mkdir -p _site/talks/" ++ t
        _ <- system $ "cp -r html/* _site/talks/" ++ t
        vroom ts

--------------------------------------------------------------------------------
main :: IO ()
main = do
  ts <- getDirectoryContents "talks/hhug"
  vroom $ map (dropEnd 6) $ filter (((==) "vroom") . dropEnd 5) ts
  hakyll $ do

    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "js/*" $ do
        route   idRoute
        compile copyFileCompiler

    tags <- buildTags "blog/posts/*" (fromCapture "tags/*.html")
    tagsRules tags $ \tag pattern -> do
        let title = "Posts tagged \"" ++ tag ++ "\""
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAllSnapshots pattern "content"
            let ctx = constField "title" title <>
                      listField "posts" (postCtx <> teaserField "teaser" "content") (return posts) <>
                      tagsCtx tags
            makeItem ""
                -- Use a more simple post-list template for this.
                >>= loadAndApplyTemplate "templates/blog.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls

    match (fromList ["about.markdown", "services.markdown",
                     "portfolio.markdown", "contact.markdown",
                     "talks.markdown"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "blog/posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/post.html" (tagsCtx tags)
            >>= saveSnapshot "atom"
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    create ["atom.xml"] $ do
        route idRoute
        compile $ do
            let feedCtx = postCtx <> bodyField "description"
            posts <- fmap (take 10) . recentFirst =<<
                loadAllSnapshots "blog/posts/*" "atom"
            renderAtom rbrosFeedConfiguration feedCtx posts

    create ["blog.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAllSnapshots "blog/posts/*" "content"
            let ctx = constField "title" "Blog" <>
                      listField "posts" (teaserField "teaser" "content" <> postCtx) (return posts) <>
                      tagsCtx tags
            makeItem ""
                >>= loadAndApplyTemplate "templates/blog.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls

    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- fmap (take 5) . recentFirst =<< loadAll "blog/posts/*"
            let indexCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Haskell Development and Consulting"  `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateCompiler
  where
    dropEnd n = reverse . drop n . reverse

--------------------------------------------------------------------------------
tagsCtx :: Tags -> Context String
tagsCtx tags = field "taglist" (\_ -> renderTagList tags) <>
               tagsField "tags" tags <>
               postCtx

postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext
