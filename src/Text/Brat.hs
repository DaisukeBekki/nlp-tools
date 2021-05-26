{-# LANGUAGE ExtendedDefaultRules, DeriveGeneric #-}

module Text.Brat (
  readBratFile,
  Label(..),
  BratLine(..),
  bratLine2Text,
  BratData(..),
  bratData2Text
  ) where

import qualified GHC.Generics as G --base
import System.FilePath ((</>),dropExtensions,replaceExtensions) --filepath
import Control.Monad (when)
import Control.Exception (throw)   --base
import Data.List (elemIndex)       --base
import qualified Data.Text    as T --text
import qualified Data.Text.IO as T --text
import Text.Parsec      --parsec
import Text.Parsec.Text --parsec

-- | directoryにあるfilename（.annファイル）を行ごとにパーズして[BratData]を得る。
readBratFile :: FilePath -> FilePath -> IO(BratData)
readBratFile data_dir ann_filename = do
  brat <- T.readFile (data_dir </> ann_filename)
  (first, second) <- file2ids ann_filename
  let bratLines = map bratParser $ T.lines brat
      txt_filename = replaceExtensions ann_filename "txt"
  txt <- T.readFile (data_dir </> txt_filename)
  let txt2 = T.lines txt
  when (not $ length txt2 == 2) $ throw (userError $ txt_filename ++ " is invalid.") 
  return $ BratData first second (head txt2) (head $ tail txt2) bratLines

-- | None, Important, Crucialのいずれか。
data Label = None | Important | Crucial deriving (Eq, Show, Read, G.Generic)

-- | bratの.annファイルの行のデータ形式
data BratLine =
  BratLine String Label Int Int T.Text 
  | Err String T.Text
  deriving (Eq, Show)

-- | (id1, id2, sentence, [annot_data])のリスト、を得たい。id1, id2は文書番号(Int)。
data BratData = BratData Int Int T.Text T.Text [BratLine] deriving (Eq, Show, G.Generic)

bratLine2Text :: BratLine -> T.Text
bratLine2Text (BratLine str lbl from to txt) = T.concat [
  "[",
  (T.pack $ show lbl),
  " ",
  (T.pack $ show from),
  "-",
  (T.pack $ show to),
  ":",
  txt,
  "]"
  ]
bratLine2Text (Err _ _) = "[Error]"                                              

bratData2Text :: BratData -> T.Text
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

file2ids :: FilePath -> IO (Int,Int)
file2ids filename =
  case elemIndex '_' filename of
    Just i ->  do
               let (first,second) = splitAt i $ dropExtensions filename
               return (read first, read $ tail second)
    Nothing -> throw (userError $ filename ++ " is an invalid filename.")

-- | bratの.annファイルの行をパーズしてBratLineを得る
bratParser :: T.Text -> BratLine
bratParser text = 
  case parse bratLine "" text of
    Left e -> Err (show e) text
    Right t -> t

-- | 以下、bratパーザを構成。
bratLine :: Parser BratLine
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
  return $ BratLine id ((read lbl)::Label) start end (T.pack fragment)  

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

