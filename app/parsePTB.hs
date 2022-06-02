{-# LANGUAGE OverloadedStrings #-}

import System.IO (hPutStrLn,stderr)  --base
import System.FilePath ((</>))       --filepath
import System.FilePath.Posix (takeBaseName) --filepath
import qualified System.Directory as D --directory
import qualified Data.Text as T      --text
import qualified Data.Text.IO as T   --text
import Text.Directory (checkFile,getFileList) --nlp-tools
import Text.PTB (parsePTBfile,isErr)   --nlp-tools

main :: IO ()
main = do
  filePaths <- getFileList "mrg" "/home/bekki/dropbox/Public/Data/PennTreebank/treebank_3/"
  ptbss <- mapM parsePTBfile $ filter (\f -> takeBaseName f /= "readme") filePaths
  let ptbs = concat ptbss
  mapM_ (putStrLn . show) $ filter isErr $ ptbs
  hPutStrLn stderr $ (show (length ptbs)) ++ " mrg files processed." 

  
