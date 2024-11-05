{-# LANGUAGE OverloadedStrings #-}

import System.IO (hPutStrLn,stderr)  --base
import qualified System.Environment as E --base
import qualified Data.Text as T      --text
import qualified Data.Text.IO as T   --text
import Text.PTB (parsePTBfromDirectory,ptbTree2sentence) --nlp-tools

-- | Usage: stack run parsePTB -- <path/to/dir> "mrg"

main :: IO ()
main = do
  (filepath:(ext:_)) <- E.getArgs
  ptbs <- parsePTBfromDirectory filepath ext
  let sample = take 1 $ drop 2 ptbs
  mapM_ (putStrLn . show) sample
  T.putStrLn $ ptbTree2sentence $ head sample
  hPutStrLn stderr $ (show (length ptbs)) ++ " files processed." 
  
