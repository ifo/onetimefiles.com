module Files.Helpers
  (saveUploadedFile
  ,deleteDownloadedFile
  )
where

import System.Directory (renameFile
                        ,removeDirectoryRecursive
                        ,doesDirectoryExist
                        ,createDirectory
                        )
import System.Random (newStdGen, randomRs, RandomGen)
import Data.Text as T (pack, unpack, Text)
import Data.Monoid ((<>))

-- TODO turn this into configuration option
tempFileDir :: FilePath
tempFileDir = "tempfiles"

saveUploadedFile :: FilePath -> T.Text -> IO Text
saveUploadedFile tempFile fileName = do
  location <- makeNewFolder
  createDirectory $ tempFileDir <> "/" <> location
  renameFile
    tempFile
    (tempFileDir <> "/" <> location <> "/" <> (unpack fileName))
  return $ T.pack location

deleteDownloadedFile :: FilePath -> IO ()
deleteDownloadedFile = removeDirectoryRecursive

makeNewFolder :: IO FilePath
makeNewFolder = do
  gen <- newStdGen
  let folderName = generateRandomFolderName 64 gen
  exists <- doesDirectoryExist folderName
  case exists of
    False -> return folderName
    True -> makeNewFolder

generateRandomFolderName :: RandomGen g => Int -> g -> String
generateRandomFolderName len gen =
  map numToChar $
  take len $
  randomRs (0, length alphabet - 1) gen
  where
    alphabet :: String
    alphabet = '_' : ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']

    numToChar :: Int -> Char
    numToChar = (!!) alphabet
