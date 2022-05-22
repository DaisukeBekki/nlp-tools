{-# LANGUAGE OverloadedStrings #-}

import System.FilePath ((</>))       --filepath
import qualified System.Directory as D --directory
import qualified Data.Text as T      --text
import qualified Data.Text.IO as T   --text
import Text.Directory (checkFile)    --nlp-tools
import Text.PTB (ptbParser)           --nlp-tools

main :: IO ()
main = do
  let ptbSampleFilePath = "/home/bekki/dropbox/Public/Data/PennTreebank/wsj_0001.mrg"
  checkFile ptbSampleFilePath
  ptb <- T.readFile ptbSampleFilePath
  mapM_ (putStrLn . show) $ ptbParser ptb
