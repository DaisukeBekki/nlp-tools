{-# LANGUAGE ExtendedDefaultRules, DeriveGeneric #-}


{- | forked from dynet-tools/Tools/Juman.hs
-}

module Text.Juman.LangModel (
  processTextsByJuman,
--  processTextByJumanOnline,
  buildDictionary,
--  cutLessFreqWords
  DictionaryData(..),
  saveDictionary,
  loadDictionary
  ) where

import System.FilePath ((</>),(<.>),takeBaseName) --filepath
import Control.Exception (throw)        --base
import Control.Monad (when,forM,guard)  --base
import System.Directory (doesFileExist,doesDirectoryExist) --base
import qualified Data.List as L    --base
import qualified GHC.Generics as G --base
import qualified Data.Text as T    --text
import qualified Shelly as S       --shelly
import qualified Data.Aeson            as A --aeson
import qualified Data.ByteString.Char8 as B --bytestring 
import qualified Data.Yaml             as Y --yaml
import Text.Juman (JumanData(..),jumanParser,file2jumanLine)  --juman-tools
import Text.Directory (checkFile) --juman-tools

--instance Show JumanTuple where
--  show (kihon,hinencodePair (kihon,hinsi) = T.concat [kihon,"#",hinsi]

-- | takes an output filepath and a list of texts,
--   process each text with `callJuman',
--   and write the result in a specified fiie (unless it already exists)
processTextsByJuman :: FilePath -> [FilePath] -> IO [[JumanData]]
processTextsByJuman outputDirPath inputFilePaths = S.shelly $ do
  dir_exist <- S.test_d outputDirPath
  when (not dir_exist) $ S.echo_n_err $ T.pack $ outputDirPath ++ " is not a directory." 
  let outputDir = S.fromText $ T.pack outputDirPath
  forM inputFilePaths $ \inputFilePath -> do
    let outputFile = outputDir </> (takeBaseName inputFilePath) <.> "juman"
    file_exist <- S.test_f outputFile
    jumanLines <- if file_exist -- if the output file already exists 
      then do
        --S.echo_n_err $ T.pack $ outputFile
        --                        ++ " exits. To re-run juman over the text files, delete "
        --                        ++ outputFile
        --                        ++ " and execute this program again.\n"
        S.echo_n_err "x"
        S.readfile outputFile
      else do
        jumanLines <- file2jumanLine inputFilePath
        S.writefile outputFile jumanLines 
        S.echo_n_err "o" 
        return jumanLines
    return $ map jumanParser $ filter (/= T.empty) $ T.lines jumanLines

isContent :: JumanData -> Bool
isContent jumanData = case jumanData of
  (JumanWord _ _ kihon hinsi _ _ _ _ _ _ _ _) -> True
  (AltWord _ _ _ _ _ _ _ _ _ _ _ _) -> False
  EOS     -> True
  Err _ _ -> False

type BaseForm = T.Text
type POS = T.Text
type WordInfo = (BaseForm,POS)

jumanData2Tuple :: JumanData -> WordInfo
jumanData2Tuple jumanData = case jumanData of
  (JumanWord _ _ kihon hinsi _ _ _ _ _ _ _ _) -> (kihon, hinsi)
  (AltWord _ _ _ _ _ _ _ _ _ _ _ _) -> ("ALT","ALT")
  EOS     -> ("EOS","EOS")
  Err _ _ -> ("ERR","ERR")

-- | Create a pair (or a tuple) of sorted labels from juman-processed texts
--   ([k1,...,kn],[p1,...,pn])
buildDictionary :: [POS] -- ^ poss to use.  when posfilter==[], all poss are used.
                   -> [JumanData]    -- ^ All the juman output data
                   -> ([BaseForm],[POS])
buildDictionary posfilter jumanData =
  let tuples = map jumanData2Tuple $ filter isContent jumanData in
  unzip $ do                  -- list monad
          (k,p)    <- tuples
          S.when (posfilter /= []) $ guard $ L.elem p posfilter  -- Only considers elements specified in posfilter
          return (k,p)        -- [(k1,p1),...,(kn,pn)]

data DictionaryData = DictionaryData {
  baseForms :: [BaseForm],
  poss :: [POS]
  } deriving (Show, G.Generic)

instance A.FromJSON DictionaryData
instance A.ToJSON DictionaryData

saveDictionary :: FilePath -> DictionaryData -> IO()
saveDictionary = Y.encodeFile 

loadDictionary :: FilePath -> IO(DictionaryData)
loadDictionary filepath = do
  checkFile filepath
  content <- B.readFile filepath
  let parsedDic = Y.decodeEither' content :: Either Y.ParseException DictionaryData
  case parsedDic of
    Left parse_exception -> error $ "Could not parse dic file " ++ filepath ++ ": " ++ (show parse_exception)
    Right dic -> return dic

{-
-- | Remove (from a given list) elements whose occurrence is equal or less than the threshold
cutLessFreqWords :: Int      -- ^ The minimum number of occurences of a word (otherwise the word is ignored)
                 -> [T.Text] -- ^ a list of words
                 -> [T.Text]
cutLessFreqWords threshold wds = fst $ unzip $ reverse $ L.sortOn snd $ U.toList $ U.filterHistByValue (>= threshold) $ U.pushWords wds
-}
