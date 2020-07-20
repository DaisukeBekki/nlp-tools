{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (forM_)             --base
import Data.List (sort)                  --base
import qualified System.Environment as E --base
import System.FilePath ((</>), isExtensionOf) --filepath
import qualified System.Directory as D        --directory
import qualified Text.KNP as K                --juman-tools

main :: IO()
--main = K.callKNP
main = do
  (folder:_) <- E.getArgs
  -- | folderのファイル名のリストを得る
  filenames <- filter (isExtensionOf "txt") <$> D.listDirectory folder
  -- | ファイル名のうち拡張子が"txt"のものについて読み出す
  forM_ (sort filenames) $ \filename -> do
    putStrLn filename
    texts <- K.fromFile $ folder </> filename
    mapM_ (K.printKNPData' . K.knpParser) texts
  
