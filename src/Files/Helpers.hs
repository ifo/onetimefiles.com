module Files.Helpers
  (setupDirs
  ,saveUploadedFile
  ,deleteDownloadedFile
  ,prepAndReturnFileTup
  )
where

import System.Directory (renameFile
                        ,renameDirectory
                        ,removeDirectoryRecursive
                        ,doesDirectoryExist
                        ,createDirectory
                        ,getDirectoryContents
                        )
import System.Random (newStdGen, randomRs, RandomGen)
import Data.Text as T (pack, unpack, Text)
import Data.Monoid ((<>))

-- TODO turn these into configuration options
tempFileDir :: FilePath
tempFileDir = "temp"

deleteFileDir :: FilePath
deleteFileDir = "delete"

setupDirs :: IO ()
setupDirs = setupDir tempFileDir >> setupDir deleteFileDir
  where
    setupDir :: FilePath -> IO ()
    setupDir dir = do
      dirExists <- doesDirectoryExist dir
      if dirExists then return () else createDirectory dir

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

prepAndReturnFileTup :: T.Text -> IO (Maybe (Text, FilePath))
prepAndReturnFileTup tempDir = do
  dirExists <- doesDirectoryExist oldLocation
  case dirExists of
    False -> return Nothing
    True -> do
      newLocation <- moveDirForDeletion $ T.unpack tempDir
      fileList <- getDirectoryContents newLocation
      let fileName = getFileNameFromList fileList
      return $ Just (T.pack fileName, (newLocation <> "/" <> fileName))
  where
    oldLocation :: FilePath
    oldLocation = tempFileDir <> "/" <> T.unpack tempDir

-- TODO not just crash and burn when this fails
getFileNameFromList :: [FilePath] -> FilePath
getFileNameFromList [] = error "file not found"
getFileNameFromList (f:fs)
  | f /= "." && f /= ".." = f
  | otherwise             = getFileNameFromList fs

moveDirForDeletion :: FilePath -> IO FilePath
moveDirForDeletion dir = do
  renameDirectory
    (tempFileDir <> "/" <> dir)
    (deleteFileDir <> "/" <> dir)
  return $ deleteFileDir <> "/" <> dir

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
