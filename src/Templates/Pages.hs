{-# LANGUAGE OverloadedStrings #-}

module Templates.Pages
  (mainPage
  ,urlPage
  ,fileTooBig
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

urlPage url =
  doctypehtml_ $ do
    head_ $
      title_ "Share your One Time File"
    body_ $ do
      h1_ "Copy the link below"
      input_ [type_ "text", value_ url, autofocus_]

fileTooBig :: Html ()
fileTooBig =
  doctypehtml_ $ do
    head_ $
      title_ "File was too big"
    body_ $ do
      p_ "That file was too big."
      p_ "Please keep files smaller than 10mb for now."
      p_ $ with a_ [href_ "/"] "Upload a different file"
