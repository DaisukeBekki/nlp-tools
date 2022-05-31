{-# LANGUAGE OverloadedStrings #-}

import System.FilePath ((</>))       --filepath
import qualified System.Directory as D --directory
import qualified Data.Text as T      --text
import qualified Data.Text.IO as T   --text
import Text.Directory (checkFile,getFileList) --nlp-tools
import Text.PTB (parsePTBfile,isErr)   --nlp-tools

main :: IO ()
main = do
--  let ptbSampleFilePath = "/home/bekki/dropbox/Public/Data/PennTreebank/wsj_0001.mrg"
--  ptb <- parsePTBfile ptbSampleFilePath
--  mapM_ (putStrLn . show) ptb
  filePaths <- getFileList "mrg" "/home/bekki/dropbox/Public/Data/"
  ptbs <- mapM parsePTBfile filePaths
  mapM_ (putStrLn . show) $ filter isErr $ concat ptbs

  
