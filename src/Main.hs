{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.Spock.Safe
import qualified Templates.Helpers   as T
import qualified Templates.Pages     as P
import qualified Files.Helpers       as F
import qualified Data.HashMap.Strict as H (lookup)
import Control.Monad.IO.Class (liftIO)
import Data.Monoid ((<>))

main :: IO ()
main =
  runSpock 3000 $ spockT id $ do

    get "/" $
      T.renderHtmlStrict P.mainPage

    post "/" $ do
      file <- files
      case H.lookup "file" file of
        Nothing -> redirect "/"
        Just uf -> do
          loc <- liftIO $
            F.saveUploadedFile
              (uf_tempLocation uf)
              (uf_name uf)
          T.renderHtmlStrict $ P.urlPage $ "http://localhost:3000/" <> loc
