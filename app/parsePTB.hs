{-# LANGUAGE OverloadedStrings #-}

import System.IO (hPutStrLn,stderr)  --base
import qualified Data.Text as T      --text
import qualified Data.Text.IO as T   --text
import Text.PTB (parsePTBfromDirectory,ptbTree2sentence) --nlp-tools

main :: IO ()
main = do
  --ptbs < parsePTBfromDirectory "/home/bekki/dropbox/Public/Data/ABCbank/treebank" "psd"
  ptbs <- parsePTBfromDirectory "/home/bekki/dropbox/Public/Data/PennTreebank/treebank_3/" "mrg"
  let sample = take 1 $ drop 2 ptbs
  mapM_ (putStrLn . show) sample
  T.putStrLn $ ptbTree2sentence $ head sample
  --hPutStrLn stderr $ (show (length ptbs)) ++ " files processed." 

  
