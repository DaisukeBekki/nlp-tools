{-# LANGUAGE OverloadedStrings #-}

module Text.KNP (
  KNPData (..)
  ,callKNP
  ,fromFile
  ,fromText
  ,knpParser
  ,knpLine
  ,printKNPData
  ,printKNPData'
  ) where

import qualified System.Environment as E --base
import qualified Data.Text as T    --text
import qualified Data.Text.IO as T --text
import qualified Shelly as S       --shelly
import Text.Parsec                 --parsec
import Text.Parsec.Text            --parsec
import Text.Parsec.Combinator      --parsec
import qualified Text.Juman as J   --juman-tools

callKNP :: IO()
callKNP = do
  (filepath:_) <- E.getArgs
  texts <- fromFile filepath
  mapM_ (printKNPData . knpParser) texts

-- | ファイル内のテキストに対しKNPを呼び出し、分析行のリストを返す。
fromFile :: FilePath -> IO([T.Text])
fromFile filepath = do
  knpOutput <- S.shelly $ S.silently $ S.escaping False $ S.cmd $ S.fromText $ T.concat ["cat ", T.pack filepath, " | nkf -w | jumanpp | knp -simple -anaphora"]
  return $ knpFilter $ T.lines knpOutput

-- | テキストに対しKNPを呼び出し、分析行のリストを返す。
fromText :: T.Text -> IO([T.Text])
fromText text = do
  knpOutput <- S.shelly $ S.silently $ S.escaping False $ S.cmd $ S.fromText $ T.concat ["echo ", text, " | jumanpp | knp -simple -anaphora"]
  return $ knpFilter $ T.lines knpOutput

-- | 空白行、#または*から始まる行は除く。
knpFilter :: [T.Text] -> [T.Text]
knpFilter = filter (\l -> l /= T.empty
                                    && not (T.isPrefixOf "#" l)
                                    && not (T.isPrefixOf "*" l)
                                    ) 

-- | KNP分析行のためのデータ形式。
-- |   KNPNode：+で始まる行。
-- |   Int: 係り先ノード番号（文末ノードはかかり先を持たないので-1）
-- |   Char: D or P or A or I
-- |   Maybe T.Text: 体言か用言
-- |   Int: eid（自ノードの番号）
-- |   Maybe [(T.Text, T.Text, Int)]: 格構造のリスト。例（ガ, 太郎, 32)
data KNPData =
  KNPNode Int Int Char (Maybe T.Text) (Maybe T.Text) (Maybe [(T.Text,T.Text,Int)])
  | Juman J.JumanData
  | Err String T.Text
  deriving (Eq, Show)

-- | KNPDataを印刷する。
printKNPData :: KNPData -> IO()
printKNPData (Juman j) = J.printJumanData j
printKNPData (Err msg text) = do
  putStr $ "Parse Error: " ++ msg ++ " in "
  T.putStrLn text
printKNPData (KNPNode id dest nodeType cat1 cat2 caseFrame) = do
  T.putStr $ T.concat [T.pack $ show id, "#", T.pack $ show dest, "#", T.singleton nodeType, "#"]
  case caseFrame of
    Just cf -> mapM_ printCaseFrame cf
    Nothing -> return ()
  T.putStr "\n"

-- | エラーのみ表示
printKNPData' :: KNPData -> IO()
printKNPData' (Juman _) = return ()
printKNPData' (Err msg text) = do
  putStr $ "Parse Error: " ++ msg ++ " in "
  T.putStrLn text
printKNPData' (KNPNode _ _ _ _ _ _) = return ()

printCaseFrame :: (T.Text,T.Text,Int) -> IO()
printCaseFrame (c, n, i) = T.putStr $ T.concat ["[", c, "=", n, "]"]

-- | KNPの分析行をパーズし、KNPData形式に変換する。
knpParser :: T.Text -> KNPData
knpParser text =
  case parse knpLine "" text of
    Left e -> Err (show e) text
    Right t -> t

knpLine :: Parser KNPData
knpLine = knpNode <|> jumanLine

jumanLine :: Parser KNPData
jumanLine = Juman <$> J.jumanLine

-- | 半角空白パーザ。
sep :: Parser ()
sep = do
  _ <- char ' '
  return ()

-- | 
knpNode :: Parser KNPData
knpNode = do
  char '+'
  sep
  dest <- (try (string "-1")) <|> (many1 digit)
  nodeType <- (try (char 'D')) <|> (try (char 'P')) <|> (try (char 'A')) <|> (char 'I')
  sep
  cat1 <- optionMaybe $ try knpCategory
  cat2 <- optionMaybe $ try knpCategory
  optionMaybe $ try ne
  optionMaybe $ try wiki
  optionMaybe $ try ne
  id <- eid
  casfram <- optionMaybe $ try caseFrames
  spaces
  return $ KNPNode id (read dest) nodeType cat1 cat2 casfram

knpCategory :: Parser (T.Text)
knpCategory = do
  try (char '<')
  s <- (string "体") <|> (string "用")
  cat <- many $ noneOf ">"
  char '>'
  return $ T.pack $ s ++ cat

-- | Named Entityとしての情報（？）
ne :: Parser ()
ne = do
  try $ string "<NE:"
  many1 $ noneOf ">"
  char '>'
  return ()

-- | Wikipedia上での情報（？）
wiki :: Parser T.Text
wiki = do
  try $ string "<Wikipediaエントリ:"
  entry <- many1 $ noneOf ">"
  char '>'
  return $ T.pack entry

-- | 数字
number :: Parser Int
number = read <$> (many1 digit)

-- | ノード番号
eid :: Parser Int
eid = do
  try $ string "<EID:"
  i <- number
  char '>'
  return i

caseFrames :: Parser [(T.Text, T.Text, Int)]
caseFrames = do
  try $ string "<項構造:"
  frames <- caseFrame `sepBy1` (char ';')
  char '>'
  return frames

caseFrame :: Parser (T.Text, T.Text, Int)
caseFrame = do
  cas <- many1 $ noneOf "/"
  char '/'
  noun <- many1 $ noneOf "/"
  char '/'
  i <- number
  return (T.pack cas, T.pack noun, i)


