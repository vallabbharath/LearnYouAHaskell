{-# LANGUAGE OverloadedStrings #-}
import Control.Monad (forM_)
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Renderer.Utf8 as R

numbers :: Int -> Html
numbers n = docTypeHtml $ do
    H.head $ do
        H.title "Natural numbers"
    body $ do
        p "A list of natural numbers:"
        ul $ forM_ [1 .. n] (li . toHtml)
        
        