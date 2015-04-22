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
      title_ "One Time Files - Not yet ready, so please don't use"
    body_ $ do
      h1_ "WARNING: DO NOT USE THIS SITE"
      h1_ "SERIOUSLY, IT'S NOT PRODUCTION READY"
      h1_ "NOR IS IT SECURE IN ANY WAY"
      h5_ "but if you stil want to..."
      h3_ "Upload a file (less than 10mb for now, please)"
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
      h3_ "Copy the link below"
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
