{-# LANGUAGE DeriveGeneric #-}

module Text.PTB (
  ptbParser
  ) where

import GHC.Generics --base
import qualified Data.Text    as T --text
import qualified Data.Text.IO as T --text
import qualified Data.Aeson            as A --aeson
import qualified Data.ByteString.Char8 as B --bytestring 
import qualified Data.Yaml             as Y --yaml
import Text.Parsec      --parsec
import Text.Parsec.Text --parsec
import Text.Directory (checkFile,getFileList) --nlp-tools

data PTBTree =
  Word String
  | Phrase String [PTBTree]
  | Err String T.Text
  deriving (Eq, Show, Read, Generic)

instance A.FromJSON PTBTree
instance A.ToJSON PTBTree

{-
ptbTree2Text :: PTBTree -> String
ptbTree2Text (Word word) = word
ptbTree2Tex (Phrase labl dtrs) = T.concat []
-}

-- | PTB (parsed)のためのパーザ
ptbParser :: T.Text -> [PTBTree]
ptbParser mrg =
  case parse ptbTreesParser "" mrg of
    Left err -> [Err (show err) mrg]
    Right trees -> trees

-- | 空白または改行
blank :: Parser String
blank = many1 $ oneOf " \t\n"

-- | 空白や括弧でない文字列
str :: Parser String
str = many1 $ noneOf " \t\n()"

-- | 数字
number :: Parser Int
number = read <$> (many1 digit)

ptbTreesParser :: Parser [PTBTree]
ptbTreesParser = do
  _ <- optional blank
  trees <- sepBy1' ptbTreeParser blank
  return trees

-- | PTBの構文木は必ず一番外側に余計な()がつく。
ptbTreeParser :: Parser PTBTree
ptbTreeParser = do
  _ <- char '('
  _ <- blank
  tree <- (phraseParser <|> wordParser)
  _ <- string ")" <|> string " )"
  return tree

wordParser :: Parser PTBTree
wordParser = do
  word <- str
  return $ Word word
  
-- | 
phraseParser :: Parser PTBTree
phraseParser = do
  _ <- char '('
  syncat <- str
  _ <- blank
  ptbs <- sepBy1' (phraseParser <|> wordParser) blank
  _ <- string ")" <|> string " )"
  return $ Phrase syncat ptbs

sepBy1' :: (Stream s m t) => ParsecT s u m a -> ParsecT s u m sep -> ParsecT s u m [a]
{-# INLINABLE sepBy1' #-}
sepBy1' p sep = do 
  x <- p
  xs <- many $ try (sep >> p)
  return $ x:xs
