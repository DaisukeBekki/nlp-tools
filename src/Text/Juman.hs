{-# LANGUAGE OverloadedStrings #-}

module Text.Juman (
  JumanData (..),
  callJuman
  ,fromFile
  ,fromText
  ,text2JumanData
  ,jumanParser
  ,jumanLine
  ,printJumanData
  ) where

import qualified System.Environment as E --base
import qualified Data.Text as T          --text
import qualified Data.Text.IO as T       --text
import qualified Shelly as S             --shelly
import Text.Parsec      --parsec
import Text.Parsec.Text --parsec

-- | コマンドライン引数で指定したファイルをJuman++で解析し、結果を標準出力に返す。
callJuman :: IO()
callJuman = do
  (filepath:_) <- E.getArgs
  texts <- fromFile filepath
  mapM_ (printJumanData . jumanParser) texts

-- | ファイル内のテキストに対しJuman++を呼び出し、分析行のリストを返す。
fromFile :: FilePath -> IO([T.Text])
fromFile filepath = do
  jumanOutput <- S.shelly $ S.silently $ S.escaping False $ S.cmd $ S.fromText $ T.concat ["cat ", T.pack filepath, " | nkf -w | jumanpp "]
  return $ filter (/= T.empty) $ T.lines jumanOutput

-- | テキストに対しJuman++を呼び出し、分析行のリストを返す。
fromText :: T.Text -> IO([T.Text])
fromText text = do
  jumanOutput <- S.shelly $ S.silently $ S.escaping False $ S.cmd $ S.fromText $ T.concat ["echo ", text, " | jumanpp "]
  return $ filter (/= T.empty) $ T.lines jumanOutput

-- | テキストに対しJuman++を呼び出し、分析行のリスト（[JumanData]型）を返す。
text2JumanData :: T.Text -> IO([JumanData])
text2JumanData text = do
  lines <- fromText text
  return $ map jumanParser lines

-- | Juman分析行のためのデータ形式。
-- | 入力形態素 読み 原型 品詞 品詞ID 品詞細分類 細分類ID 活用型 活用型ID 活用形 活用形ID その他の情報
data JumanData =
  JumanWord T.Text T.Text T.Text T.Text Int T.Text Int T.Text Int T.Text Int T.Text
  | AltWord T.Text T.Text T.Text T.Text Int T.Text Int T.Text Int T.Text Int T.Text
  | EOS
  | Err String T.Text
  deriving (Eq, Show)

printJumanData :: JumanData -> IO()
printJumanData (JumanWord nyuryoku yomi genkei hinsi hinsiID hinsiSaibunrui saibunruiID katuyogata katuyogataID katuyokei katuyokeiID sonota) =
  T.putStrLn $ T.intercalate "#" [
    nyuryoku
    ,yomi
    ,genkei
    ,hinsi
    ,T.pack $ show hinsiID
    ,hinsiSaibunrui
    ,T.pack $ show saibunruiID
    ,katuyogata
    ,T.pack $ show katuyogataID
    ,katuyokei
    ,T.pack $ show katuyokeiID
    ,sonota
    ]
printJumanData (AltWord nyuryoku yomi genkei hinsi hinsiID hinsiSaibunrui saibunruiID katuyogata katuyogataID katuyokei katuyokeiID sonota) =
  printJumanData (JumanWord nyuryoku yomi genkei hinsi hinsiID hinsiSaibunrui saibunruiID katuyogata katuyogataID katuyokei katuyokeiID sonota)
printJumanData EOS = putStrLn "EOS"
printJumanData (Err msg text) = do
  putStr $ "Parse Error: " ++ msg ++ " in "
  T.putStrLn text

-- | Jumanの分析行をパーズし、JumanData形式に変換する。
jumanParser :: T.Text -> JumanData
jumanParser text = 
  case parse jumanLine "" text of
    Left e -> Err (show e) text
    Right t -> t

-- | Juman分析行はEOS、@で始まる行、Jumanの語分析行のいずれか。
jumanLine :: Parser JumanData
jumanLine = eos <|> altWord <|> jumanWord

-- | 半角空白
sep :: Parser ()
sep = do
  _ <- char ' '
  return ()

-- | EOS行
eos :: Parser JumanData
eos = do
  _ <- try (string "EOS")
  return EOS

-- | 空白でない文字
nonSep :: Parser T.Text
nonSep = T.pack <$> (many1 $ noneOf " ")

-- | *
asterisk :: Parser T.Text
asterisk = T.singleton <$> char '*'

-- | 数字
number :: Parser Int
number = read <$> (many1 digit)

-- | 入力形態素 読み 原型 品詞 品詞ID 品詞細分類 細分類ID 活用型 活用型ID 活用形 活用形ID その他の情報
jumanWord :: Parser JumanData
jumanWord = do
  nyuryoku <- nonSep
  sep
  yomi <- nonSep
  sep
  genkei <- nonSep
  sep
  hinsi <- nonSep
  sep
  hinsiID <- number
  sep
  hinsiSaibunrui <- nonSep
  sep
  saibunruiID <- number
  sep
  katuyogata <- asterisk <|> nonSep
  sep
  katuyogataID <- number
  sep
  katuyokei <- asterisk <|> nonSep
  sep
  katuyokeiID <- number
  sep
  sonota <- nonSep
  return $ JumanWord nyuryoku yomi genkei hinsi hinsiID hinsiSaibunrui saibunruiID katuyogata katuyogataID katuyokei katuyokeiID sonota

altWord :: Parser JumanData
altWord = do
  _ <- char '@'
  sep
  nyuryoku <- nonSep
  sep
  yomi <- nonSep
  sep
  genkei <- nonSep
  sep
  hinsi <- nonSep
  sep
  hinsiID <- number
  sep
  hinsiSaibunrui <- nonSep
  sep
  saibunruiID <- number
  sep
  katuyogata <- asterisk <|> nonSep
  sep
  katuyogataID <- number
  sep
  katuyokei <- asterisk <|> nonSep
  sep
  katuyokeiID <- number
  sep
  sonota <- nonSep
  return $ AltWord nyuryoku yomi genkei hinsi hinsiID hinsiSaibunrui saibunruiID katuyogata katuyogataID katuyokei katuyokeiID sonota

