{-# LANGUAGE OverloadedStrings #-}

import System.FilePath ((</>))        --filepath
import System.Environment (getArgs)   --base
import Control.Monad (forM_)          --base
import Data.List (sortOn)             --base
import qualified Data.Text as T       --text
import qualified Data.Text.IO as T    --text
import NLP.Similarity.VectorSim as N  --chatter
import NLP.Types as N                 --chatter
import Text.Distiller                 --juman-tools

main :: IO ()
main = do
  (folder:_) <- getArgs
  jumantxt <- T.readFile $ folder </> "juman_preprocessed.txt"
  let jtxt = map (T.splitOn "\t") $ T.lines jumantxt            -- [[Text]]
      corpus = N.mkCorpus jtxt                                  -- Corpus
      termVectors = map (\j -> (head j, N.mkVector corpus $ N.mkDocument $ tail j)) jtxt -- [(Text, TmVector)]
  putStrLn "Corpus compiled..."
  queries <- T.readFile $ folder </> "クエリ案.txt"              -- Text
  let qs = filter (/= T.empty) $ T.lines queries                -- [Text]
  jqs <- mapM juman qs                                          -- [[Text]]
  putStrLn "Queries compilied..."
  let queryVectors = map (\(qtxt,jq) -> (qtxt, N.mkVector corpus $ N.mkDocument jq)) jqs -- [([Text], TmVector)]
  forM_ queryVectors $ \(qtxt, queryVec) -> do
    let similarities = for termVectors $ \(txt, tmVec) -> (txt, N.cosVec queryVec tmVec)
    T.putStr $ T.concat ["【", qtxt, "】"]
    --T.putStrLn $ T.intercalate "/" $ N.keys $ queryVec
    --print queryVec
    forM_ (take 10 $ reverse $ sortOn snd $ similarities) $ \(txt, d) -> do
      T.putStr $ T.pack $ show d
      T.putStr ": "
      T.putStrLn txt
    T.putStrLn "---"
  where for = flip map
