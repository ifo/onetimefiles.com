{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.Spock.Safe
import qualified Templates.Helpers   as T
import qualified Templates.Pages     as P
import qualified Data.HashMap.Strict as H (lookup)
import Data.Text (unpack)
import Control.Monad.IO.Class (liftIO)
import System.Directory (copyFile)
import Data.Maybe (fromJust)
import Data.Monoid ((<>))

main :: IO ()
main =
  runSpock 3000 $ spockT id $ do

    get "/" $
      T.renderHtmlStrict P.mainPage

    post "/" $ do
      file <- files
      -- save the temporary file to its filename
      -- TODO save it to a specific subdirectory or other location
      liftIO $ copyFile
        (uf_tempLocation $ fromJust $ H.lookup "file" file)
        (unpack "tempfiles/" <>
          (unpack $ uf_name $ fromJust $ H.lookup "file" file))
      case H.lookup "file" file of
        Nothing -> redirect "/"
        Just uf -> text "saved"
