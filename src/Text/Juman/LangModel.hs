{- | forked from dynet-tools/Tools/Juman.hs
-}

module Text.Juman.LangModel (
  Kihon,
  Hinsi,
  processTextsByJuman,
--  processTextByJumanOnline,
  buildDictionary
--  cutLessFreqWords
  ) where

import Control.Monad (forM,guard)  --base
import qualified Data.List as L    --base
import qualified Data.Text as T    --text
--import qualified Data.Text.IO as T --text
import qualified Shelly as S       --shelly
import Text.Juman (JumanData(..),file2jumanData)  --juman-tools

type Kihon = T.Text
type Hinsi = T.Text

-- | takes an output filepath and a list of texts,
--   process each text with `callJuman',
--   and write the result in a specified fiie (unless it already exists)
processTextsByJuman :: FilePath -> [FilePath] -> IO [[(Kihon, Hinsi)]]
processTextsByJuman outputFilepath inputFilePaths = S.shelly $ do
  let outputFile = S.fromText $ T.pack outputFilepath
      for = flip map
  exist <- S.test_f outputFile
  if exist -- if the outputfile already exists 
    then do
      S.echo_n_err $ T.pack $ outputFilepath ++ " exits. To re-run juman over the text files, delete " ++ outputFilepath ++ " and execute this program again.\n"
      -- "k1#p1,...,kn#pn \n k1#p1,...,kn#pn \n ..." >>= ["k1#p1,...,kn#pn", "k1#p1,...,kn#pn", ...]
      lns <- T.lines <$> S.readfile outputFile
      return $ for lns $ \ln -> do
                           pairs <- T.split (==',') ln
                           let (k:p:_) = T.split (=='#') pairs
                           return (k,p)
    else do
      S.echo_n_err $ T.pack $ show (length inputFilePaths) ++ " files processed.\n"
      forM inputFilePaths $ \inputFilePath -> do
                     jumanData <- S.shelly $ file2jumanData inputFilePath
                     let jumanPairs = jumanData2Pair jumanData
                     S.appendfile outputFile $ T.snoc (T.intercalate "," $ map encodePair jumanPairs) '\n'
                     S.echo_n_err "o" 
                     return jumanPairs

jumanData2Pair :: [JumanData] -> [(Kihon, Hinsi)]
jumanData2Pair [] = []
jumanData2Pair (jumanData:rest) = case jumanData of
  (JumanWord _ _ kihon hinsi _ _ _ _ _ _ _ _) -> (kihon, hinsi):(jumanData2Pair rest)
  (AltWord _ _ _ _ _ _ _ _ _ _ _ _) -> jumanData2Pair rest
  EOS     -> ("EOS","EOS"):(jumanData2Pair rest)
  Err _ _ -> jumanData2Pair rest

encodePair :: (Kihon, Hinsi) -> T.Text
encodePair (kihon,hinsi) = T.concat [kihon,"#",hinsi]

{-
processTextByJumanOnline :: T.Text -> IO [(Kihon, Hinsi)]
processTextByJumanOnline text =
  S.shelly $ do
             j <- callJuman' text
             return $ map jumanEntry2Pair j
-}

-- | Create a pair (or a tuple) of sorted labels from juman-processed texts
--   ([k1,...,kn],[p1,...,pn])
buildDictionary :: [[(Kihon, Hinsi)]]    -- ^ All the juman output texts: ["k1#p1,...,kn#pn","k1#p1,...,kn#pn",...]
                   -> [Hinsi] -- ^ poss to use.  when posfilter==[], all poss are used.
                   -> ([Kihon],[Hinsi])
buildDictionary allResults posfilter =
  unzip $ do                      -- list monad
          oneResult <- allResults -- [(k1,p1),...,(kn,pn)]
          (k,p)     <- oneResult  -- (k1,p1)
          S.when (posfilter /= []) $ guard $ L.elem p posfilter         -- Only considers elements specified in posfilter
          return (k,p)                           -- [(k1,p1),...,(kn,pn)]

{-
-- | Remove (from a given list) elements whose occurrence is equal or less than the threshold
cutLessFreqWords :: Int      -- ^ The minimum number of occurences of a word (otherwise the word is ignored)
                 -> [T.Text] -- ^ a list of words
                 -> [T.Text]
cutLessFreqWords threshold wds = fst $ unzip $ reverse $ L.sortOn snd $ U.toList $ U.filterHistByValue (>= threshold) $ U.pushWords wds
-}
