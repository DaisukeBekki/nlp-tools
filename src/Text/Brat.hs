{-# LANGUAGE ExtendedDefaultRules, DeriveGeneric #-}

module Text.Brat (
  BratLine(..),
  bratLine2Text,
  BratData(..),
  bratData2Text,
  saveBratData,
  loadBratData,
  prepareBratData,
  readBratFile,
  bratParser
  ) where

import GHC.Generics --base
import System.FilePath (replaceExtensions) --filepath
import Control.Monad (when)
--import Control.Exception (throw)   --base
import qualified Data.Text    as T --text
import qualified Data.Text.IO as T --text
import qualified Data.Aeson            as A --aeson
import qualified Data.ByteString.Char8 as B --bytestring 
import qualified Data.Yaml             as Y --yaml
import Text.Parsec      --parsec
import Text.Parsec.Text --parsec
import Text.Directory (checkFile,getFileList) --nlp-tools

-- | bratの.annファイルの行のデータ形式
data BratLine = 
  Entity Int String Int Int T.Text
  | Attr Int String Int
  | Err String T.Text
  deriving (Eq, Show, Read, Generic)

instance A.FromJSON BratLine
instance A.ToJSON BratLine

-- | bratの.annファイル、.txtファイルのペアに対するデータ形式
data BratData =
  BratData T.Text [BratLine]
  deriving (Eq, Show, Read, Generic)

instance A.FromJSON BratData
instance A.ToJSON BratData

saveBratData :: FilePath -> [BratData] -> IO()
saveBratData = Y.encodeFile 

loadBratData :: FilePath -> IO([BratData])
loadBratData filepath = do
  checkFile filepath
  content <- B.readFile filepath
  let parsedBrat = Y.decodeEither' content :: Either Y.ParseException [BratData]
  case parsedBrat of
    Left parse_exception -> error $ "Could not parse dic file " ++ filepath ++ ": " ++ (show parse_exception)
    Right brats -> return brats

bratLine2Text :: BratLine -> T.Text
bratLine2Text (Entity idn lbl start end txt) = T.concat [
  "T",
  (T.pack $ show idn),
  " ",
  (T.pack $ show lbl),
  " ",
  (T.pack $ show start),
  "-",
  (T.pack $ show end),
  ":",
  txt
  ]
bratLine2Text (Attr idn lbl arg) = T.concat [
  "A",
  (T.pack $ show idn),
  " ",
  (T.pack $ show lbl),
  " ",
  (T.pack $ show arg)
  ]
bratLine2Text (Err _ _) = "[Error]"                                              

bratData2Text :: BratData -> T.Text
bratData2Text (BratData txt bratLines) = T.concat $ (txt : (map bratLine2Text bratLines))

-- | bratDirにある.annファイルをパーズし、bratSaveFileにBratData形式で保存する。
prepareBratData :: Bool -> FilePath -> FilePath -> IO()
prepareBratData showStat bratDir bratSaveFile = do
  annList <- getFileList "ann" bratDir   -- | .annファイルのフルパスのリストを得る
  bratLines <- mapM readBratFile annList -- | .annファイルをすべてパーズして[BratData]を得る
  when showStat $ putStrLn $ "Number of Brat files: " ++ (show $ length bratLines)
  when showStat $ putStrLn $ "Number of annotated Brat files: " ++ (show $ length bratLines)
  saveBratData bratSaveFile bratLines
  putStrLn $ "Brat data saved in: " ++ (show bratSaveFile)

-- | filename（.annファイル）を行ごとにパーズし、対応する.txtファイルの内容と合わせてBratDataを得る。
-- | ann_filenameはフルパス
readBratFile :: FilePath -> IO(BratData)
readBratFile annFilePath = do
  checkFile annFilePath
  ann <- T.readFile annFilePath
  let txtFilePath = replaceExtensions annFilePath "txt"
  checkFile txtFilePath
  txt <- T.readFile txtFilePath
  return $ BratData txt (map bratParser $ T.lines ann)

-- | Brat Parser: 
-- | bratの.annファイルの行をパーズしてBratLineを得る
bratParser :: T.Text -> BratLine
bratParser txt = 
  case parse bratLine "" txt of
    Left e -> Err (show e) txt
    Right t -> t

-- | 以下、bratパーザを構成。
bratLine :: Parser BratLine
bratLine = entityParser <|> attrParser

entityParser :: Parser BratLine
entityParser = do
  _     <- char 'T'
  idn   <- number
  _     <- char '\t'
  lbl   <- str
  _     <- char ' '
  start <- number
  _     <- char ' '
  end   <- number
  _     <- char '\t'
  fragment <- str
  return $ Entity idn lbl start end (T.pack fragment)  

attrParser :: Parser BratLine
attrParser = do
  _     <- char 'A'
  idn   <- number
  _     <- char '\t'
  lbl   <- str
  _     <- char ' '
  _     <- char 'T'
  arg   <- number
  return $ Attr idn lbl arg

-- | 空白でない文字列
str :: Parser String
str = many1 $ noneOf " \t"

-- | 数字
number :: Parser Int
number = read <$> (many1 digit)

-- | EOS行
--eos :: Parser BratLine
--eos = do
--  _ <- try (string "EOS")
--  return EOS

