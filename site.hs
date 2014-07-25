--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll

--------------------------------------------------------------------------------
main :: IO ()
main = hakyllWith config $ do
    match "templates/*" $ compile templateCompiler

    match (fromList ["stylesheets/stylesheet.css"
                    ,"stylesheets/stylesheet2.css"]) $ do
        route   idRoute
        compile compressCssCompiler

    match (fromList ["javascripts/game.js"
                    ,"javascripts/lman.js"
                    , "images/alltiles.png"
                    , "images/bkg.png"
                    , "images/blacktocat.png"
                    , "lman.html"
                    , "game.html"]) $ do
        route   idRoute
        compile copyFileCompiler

    match (fromList ["index.markdown", "specification.markdown"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls


config :: Configuration
config = defaultConfiguration { deployCommand = "./deploy.sh" }
