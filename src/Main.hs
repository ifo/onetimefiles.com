{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.Spock.Safe
import qualified Templates.Helpers   as T
import qualified Templates.Pages     as P
import qualified Files.Helpers       as F
import qualified Data.HashMap.Strict as H (lookup)
import Data.Text (unpack)
import Control.Monad.IO.Class (liftIO)
import Data.Monoid ((<>))

main :: IO ()
main =
  runSpock 3000 $ spockT id $ do

    liftIO F.setupDirs

    get "/" $
      T.renderHtmlStrict P.mainPage

    post "/f" $ do
      mSize <- header "Content-Length"
      case mSize of
        Nothing -> redirect "/"
        Just size ->
          if (read $ unpack size) > 10485760 then
            T.renderHtmlStrict P.fileTooBig
          else do
            file <- files
            case H.lookup "file" file of
              Nothing -> redirect "/"
              Just uf -> do
                loc <- liftIO $
                  F.saveUploadedFile
                    (uf_tempLocation uf)
                    (uf_name uf)
                T.renderHtmlStrict
                  $ P.urlPage
                  $ "http://localhost:3000/f/" <> loc

    get ("f" <//> var) $ \dir -> do
      retFileTup <- liftIO $ F.prepAndReturnFileTup dir
      case retFileTup of
        Nothing -> redirect "/"
        Just ft -> do
          setHeader
            "Content-Disposition"
            ("attachment; filename=\"" <> fst ft <> "\"")
          file "application/octet-stream" (snd ft)
