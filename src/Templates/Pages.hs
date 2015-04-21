{-# LANGUAGE OverloadedStrings #-}

module Templates.Pages
  (mainPage
  )
where

import Lucid
import Data.Monoid ((<>))

mainPage :: Html ()
mainPage =
  doctypehtml_ $ do
    head_ $
      title_ "One Time Files"
    body_ $ do
      h1_ "Upload a file"
      with form_ [method_ "post",
                  action_ "/",
                  enctype_ "multipart/form-data"
                  ] $ do
        input_ [type_ "file", name_ "file"]
        with button_ [type_ "submit"] "Upload"
