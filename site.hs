--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll

--------------------------------------------------------------------------------
main :: IO ()
main = hakyllWith config $ do
    match "templates/*" $ compile templateCompiler

    match (fromList ["stylesheets/stylesheet.css"
                    ,"stylesheets/stylesheet2.css"
                    ,"stylesheets/hall-of-fame.css"]) $ do
        route   idRoute
        compile compressCssCompiler

    match (fromList ["javascripts/game.js"
                    ,"javascripts/lman.js"
                    , "images/alltiles.png"
                    , "images/bkg.png"
                    , "images/blacktocat.png"
                    , "lman.html"
                    , "game.html"
                    , "code/local.gcc"
                    , "code/goto.gcc"
                    , "maps/world-classic.txt"
                    , "maps/world-1.txt"
                    , "maps/world-2.txt"
                    , "hall-of-fame.html"
                    , "javascripts/hall-of-fame.js"]) $ do
        route   idRoute
        compile copyFileCompiler

    match (fromList ["index.markdown"
                    ,"specification.markdown"
                    ,"results.markdown"
                    ,"reference.markdown"
                    ,"faq.markdown"
                    ,"spec-extra.markdown"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls


config :: Configuration
config = defaultConfiguration { deployCommand = "./deploy.sh" }
