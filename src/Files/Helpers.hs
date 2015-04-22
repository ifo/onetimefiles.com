module Files.Helpers
  (saveUploadedFile
  ,deleteDownloadedFile
  ,prepAndReturnFileTup
  )
where

import System.Directory (renameFile
                        ,removeDirectoryRecursive
                        ,doesDirectoryExist
                        ,createDirectory
                        ,getDirectoryContents
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

-- TODO move file to deletion area before returning it
prepAndReturnFileTup :: T.Text -> IO (Maybe (Text, FilePath))
prepAndReturnFileTup tempDir = do
  dirExists <- doesDirectoryExist location
  case dirExists of
    False -> return Nothing
    True -> do
      fileList <- getDirectoryContents location
      let fileName = getFileNameFromList fileList
      return $ Just (T.pack fileName, (location <> "/" <> fileName))
  where
    location :: FilePath
    location = tempFileDir <> "/" <> T.unpack tempDir

-- TODO not just crash and burn when this fails
getFileNameFromList :: [FilePath] -> FilePath
getFileNameFromList [] = error "file not found"
getFileNameFromList (f:fs)
  | f /= "." && f /= ".." = f
  | otherwise             = getFileNameFromList fs

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
