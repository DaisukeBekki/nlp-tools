{-# LANGUAGE ExtendedDefaultRules, DeriveGeneric, TypeApplications, ExplicitForAll, ScopedTypeVariables, AllowAmbiguousTypes #-}

module Text.Brat (
  prepareBratData,
  readBratFile,
  BratLine(..),
  bratLine2Text,
  BratData(..),
  bratData2Text,
  saveBratData,
  loadBratData
  ) where

import GHC.Generics --base
import System.FilePath ((</>),dropExtensions,replaceExtensions) --filepath
import System.FilePath.Posix (splitFileName) --filepath
import Control.Monad (when)
import Control.Exception (throw)   --base
import Data.List (elemIndex)       --base
import qualified Data.Text    as T --text
import qualified Data.Text.IO as T --text
import qualified Data.Aeson            as A --aeson
import qualified Data.ByteString.Char8 as B --bytestring 
import qualified Data.Yaml             as Y --yaml
import Text.Parsec      --parsec
import Text.Parsec.Text --parsec
import Text.Directory (checkFile,checkDir,getFileList) --juman-tools

-- | bratDirにある.annファイルをパーズし、bratSaveFileにBratData形式で保存する。
prepareBratData :: forall lbl. (Eq lbl, Read lbl, A.ToJSON lbl) => Bool -> FilePath -> FilePath -> IO()
prepareBratData showStat bratDir bratSaveFile = do
  annList <- getFileList "ann" bratDir  -- | .annファイルのフルパスのリストを得る
  bratData <- mapM (readBratFile @lbl) annList -- | .annファイルをすべてパーズして[BratData]を得る
  when showStat $ putStrLn $ "Number of Brat files: " ++ (show $ length bratData)
  let nonEmptyBrats = filter (\(BratData _ _ _ _ bl) -> bl /= []) bratData -- | アノテーションが空でないもののみ集める
  when showStat $ putStrLn $ "Number of annotated Brat files: " ++ (show $ length nonEmptyBrats)
  saveBratData bratSaveFile nonEmptyBrats
  putStrLn $ "Brat data saved in: " ++ (show bratSaveFile)

-- | directoryにあるfilename（.annファイル）を行ごとにパーズして[BratData]を得る。
-- | ann_filenameはフルパス
readBratFile :: forall a. (Read a) => FilePath -> IO(BratData a)
readBratFile ann_filename = do
  let (_,filename) = splitFileName ann_filename
  brat <- T.readFile $ ann_filename
  (first, second) <- file2ids filename
  let bratLines = map bratParser $ T.lines brat
      txt_filename = replaceExtensions ann_filename "txt"
  txt <- T.readFile $ txt_filename
  let txt2 = T.lines txt
  when (not $ length txt2 == 2) $ throw (userError $ txt_filename ++ " is invalid.") 
  return $ BratData first second (head txt2) (head $ tail txt2) bratLines

file2ids :: FilePath -> IO (Int,Int)
file2ids filename =
  case elemIndex '_' filename of
    Just i ->  do
               let (first,second) = splitAt i $ dropExtensions filename
               return (read first, read $ tail second)
    Nothing -> throw (userError $ filename ++ " is an invalid filename.")


-- | bratの.annファイルの行のデータ形式
data BratLine a = 
  BratLine String a Int Int T.Text 
  | Err String T.Text
  deriving (Eq, Show, Read, Generic)

instance (A.FromJSON a) => A.FromJSON (BratLine a)
instance (A.ToJSON a) => A.ToJSON (BratLine a)

-- | (id1, id2, sentence, [annot_data])のリスト、を得たい。id1, id2は文書番号(Int)。
data BratData a =
  BratData Int Int T.Text T.Text [BratLine a]
  deriving (Eq, Show, Read, Generic)

instance (A.FromJSON a) => A.FromJSON (BratData a)
instance (A.ToJSON a) => A.ToJSON (BratData a)

saveBratData :: forall a. (A.ToJSON a) => FilePath -> [BratData a] -> IO()
saveBratData = Y.encodeFile 

loadBratData :: forall a. (A.FromJSON a) => FilePath -> IO([BratData a])
loadBratData filepath = do
  checkFile filepath
  content <- B.readFile filepath
  let parsedDic = Y.decodeEither' content :: Either Y.ParseException [BratData a]
  case parsedDic of
    Left parse_exception -> error $ "Could not parse dic file " ++ filepath ++ ": " ++ (show parse_exception)
    Right dic -> return dic

bratLine2Text :: forall a. (Show a) => (BratLine a) -> T.Text
bratLine2Text (BratLine str lbl from to txt) = T.concat [
  "[",
  (T.pack $ show @a lbl),
  " ",
  (T.pack $ show from),
  "-",
  (T.pack $ show to),
  ":",
  txt,
  "]"
  ]
bratLine2Text (Err _ _) = "[Error]"                                              

bratData2Text :: forall a. (Show a) => (BratData a) -> T.Text
bratData2Text (BratData first second txt1 txt2 bratLines) = T.concat $ [
  "BratData ",
  (T.pack $ show first),
  "-",
  (T.pack $ show second),
  ":「",
  txt1,
  "」↔「",
  txt2,
  "」"
  ]
  ++ (map bratLine2Text bratLines) ++ ["\n"]

-- | Brat Parser

-- | bratの.annファイルの行をパーズしてBratLineを得る
bratParser :: forall a. (Read a) => T.Text -> (BratLine a)
bratParser text = 
  case parse bratLine "" text of
    Left e -> Err (show e) text
    Right t -> t

-- | 以下、bratパーザを構成。
bratLine :: forall a. (Read a) => Parser (BratLine a)
bratLine = do
  id <- str
  char '\t'
  lbl <- str
  char ' '
  start <- number
  char ' '
  end <- number
  char '\t'
  fragment <- str
  return $ BratLine id (read @a lbl) start end (T.pack fragment)  

-- | EOS行
--eos :: Parser BratLine
--eos = do
--  _ <- try (string "EOS")
--  return EOS

-- | 空白でない文字列
str :: Parser String
str = many1 $ noneOf " \t"

-- | 数字
number :: Parser Int
number = read <$> (many1 digit)

