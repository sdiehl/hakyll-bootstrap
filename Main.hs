{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) Stephen Diehl 2013
-- License   :  MIT
-- Maintainer:  stephen.m.diehl@gmail.com
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------

module Main where

import Hakyll
import Text.Pandoc
import Data.Monoid (mappend)
import qualified Data.Map as M

--------------------------------------------------------------------
-- Contexts
--------------------------------------------------------------------

postCtx :: Context String
postCtx =
  dateField "date" "%B %e, %Y"
  `mappend` mathCtx
  `mappend` defaultContext

mathCtx :: Context String
mathCtx = field "mathjax" $ \item -> do
  metadata <- getMetadata $ itemIdentifier item
  return $ if "mathjax" `M.member` metadata
           then "<script type=\"text/javascript\" src=\"http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML\"></script>"
           else ""

archiveCtx posts =
  listField "posts" postCtx (return posts)
  `mappend` constField "title" "Archives"
  `mappend` defaultContext

indexCtx posts =
  listField "posts" postCtx (return posts)
  `mappend` constField "title" "Home"
  `mappend` defaultContext

--------------------------------------------------------------------
-- Rules
--------------------------------------------------------------------

static :: Rules ()
static = do
  match "fonts/*" $ do
    route idRoute
    compile $ copyFileCompiler
  match "img/*" $ do
    route idRoute
    compile $ copyFileCompiler
  match "css/*" $ do
    route idRoute
    compile compressCssCompiler
  match "js/*" $ do
    route idRoute
    compile $ copyFileCompiler

pages :: Rules ()
pages = do
  match "pages/*" $ do
    route $ setExtension "html"
    compile $ getResourceBody
      >>= loadAndApplyTemplate "templates/page.html"    postCtx
      >>= relativizeUrls

posts :: Rules ()
posts = do
  match "posts/*" $ do
    route $ setExtension "html"
    compile $ compiler
      >>= loadAndApplyTemplate "templates/post.html"    postCtx
      >>= relativizeUrls

archive :: Rules ()
archive = do
  create ["archive.html"] $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll "posts/*"
      makeItem ""
        >>= loadAndApplyTemplate "templates/archive.html" (archiveCtx posts)
        >>= relativizeUrls

index :: Rules ()
index = do
  match "index.html" $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll "posts/*"
      getResourceBody
        >>= applyAsTemplate (indexCtx posts)
        >>= relativizeUrls

templates :: Rules ()
templates = match "templates/*" $ compile templateCompiler

--------------------------------------------------------------------
-- Configuration
--------------------------------------------------------------------

compiler :: Compiler (Item String)
compiler = pandocCompilerWith defaultHakyllReaderOptions pandocOptions

pandocOptions :: WriterOptions
pandocOptions = defaultHakyllWriterOptions{ writerHTMLMathMethod = MathJax "" }

cfg :: Configuration
cfg = defaultConfiguration

main :: IO ()
main = hakyllWith cfg $ do
  static
  pages
  posts
  archive
  index
  templates
