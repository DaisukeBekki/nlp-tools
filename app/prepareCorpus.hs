{-# LANGUAGE OverloadedStrings #-}

import System.Environment (getArgs)  --base
import Control.Monad (forM_)         --base
import System.FilePath ((</>))       --filepath
import Shelly (whenM)                --shelly
import qualified System.Directory as D --directory
import qualified Data.Text as T      --text
import qualified Data.Text.IO as T   --text
import Text.Distiller (collectText,canonicalize,juman)  --juman-tools

main :: IO ()
main = do
  (folder:_) <- getArgs
  let canonical_text = folder </> "canonicalized.txt"
      juman_output = folder </> "juman_preprocessed.txt"
  whenM (D.doesFileExist juman_output) $ D.removeFile juman_output
  txts <- collectText folder                             -- :: [Text]
  let ctxts = concat $ map canonicalize txts             -- :: [Text]
  T.writeFile canonical_text $ T.intercalate "\n" ctxts
  jtxts <- mapM juman ctxts                              -- :: [[Text]]
  forM_ jtxts $ \(txt,jtxt) -> do                        -- :: IO ()
    putStr "o"
    T.appendFile juman_output $ T.concat [txt, "\t", T.intercalate "\t" jtxt, "\n"]
