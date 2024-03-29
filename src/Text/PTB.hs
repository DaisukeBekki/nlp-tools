{-# LANGUAGE DeriveGeneric #-}

module Text.PTB (
  PTBTree(..),
  ptbTree2sentence,
  parsePTBtext,
  parsePTBfromFile,
  parsePTBfromDirectory,
  isErr
  ) where

import GHC.Generics --base
import System.FilePath.Posix (takeBaseName) --filepath
import qualified Data.Text    as T --text
import qualified Data.Text.IO as T --text
import qualified Data.Aeson            as A --aeson
import qualified Data.ByteString.Char8 as B --bytestring 
import qualified Data.Yaml             as Y --yaml
import Text.Parsec      --parsec
import Text.Parsec.Text --parsec
import Text.Directory (checkFile,checkDir,getFileList) --nlp-tools

data PTBTree =
  Word T.Text T.Text -- surface form and POS
  | Phrase String [PTBTree]
  | Err String T.Text
  deriving (Eq, Show, Read, Generic)

instance A.FromJSON PTBTree
instance A.ToJSON PTBTree

ptbTree2sentence :: PTBTree -> T.Text
ptbTree2sentence (Word word _) = word
ptbTree2sentence (Phrase labl dtrs) = T.intercalate " " $ map ptbTree2sentence dtrs
ptbTree2sentence (Err _ text) = T.concat ["(Err ", text, ")"]

isErr :: PTBTree -> Bool
isErr ptb = case ptb of
  Word _ _ -> False
  Phrase _ _ -> False
  Err _ _ -> True

-- | PTB (parsed)のためのパーザ
parsePTBtext :: T.Text -> [PTBTree]
parsePTBtext mrg =
  case parse ptbTreesParser "" mrg of
    Left err -> [Err (show err) mrg]
    Right trees -> trees

-- | 空白または改行
blank :: Parser String
blank = many1 $ oneOf " \t\n"

-- | 空白や括弧でない文字列
str :: Parser String
str = many1 $ noneOf " \t\n()/"

-- | 数字
number :: Parser Int
number = read <$> (many1 digit)

ptbTreesParser :: Parser [PTBTree]
ptbTreesParser = do
  _ <- many commentParser
  _ <- optional blank
  trees <- sepBy1' ptbTreeParser blank
  return trees

-- | PTBの構文木は必ず一番外側に余計な()がつく。
ptbTreeParser :: Parser PTBTree
ptbTreeParser = do
  _ <- char '('
  _ <- optional blank
  tree <- (phraseParser <|> wordParser)
  _ <- char ')' <|> (blank >> char ')')
  return tree

wordParser :: Parser PTBTree
wordParser = do
  word <- str
  _ <- char '/'
  pos <- str
  return $ Word (T.pack word) (T.pack pos)
  
-- | 
phraseParser :: Parser PTBTree
phraseParser = do
  _ <- char '('
  syncat <- str
  _ <- blank
  ptbs <- sepBy1' (phraseParser <|> wordParser) blank
  _ <- char ')' <|> (blank >> char ')')
  return $ Phrase syncat ptbs

commentParser :: Parser ()
commentParser = do
  _ <- string "*x"
  _ <- many $ noneOf "\n"
  _ <- char '\n'
  return ()

sepBy1' :: (Stream s m t) => ParsecT s u m a -> ParsecT s u m sep -> ParsecT s u m [a]
{-# INLINABLE sepBy1' #-}
sepBy1' p sep = do 
  x <- p
  xs <- many $ try (sep >> p)
  return $ x:xs

-- | PTBパーザ
parsePTBfromFile :: FilePath -> IO [PTBTree]
parsePTBfromFile ptbFilePath = do
  checkFile ptbFilePath
  ptb <- T.readFile ptbFilePath
  return $ parsePTBtext ptb

parsePTBfromDirectory :: FilePath -> String -> IO [PTBTree]
parsePTBfromDirectory ptbDirectoryPath ext = do
  checkDir ptbDirectoryPath
  filePaths <- getFileList ext ptbDirectoryPath
  ptbss <- mapM parsePTBfromFile $ filter (\f -> takeBaseName f /= "readme") filePaths
  return $ concat ptbss

