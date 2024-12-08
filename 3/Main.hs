import Control.Applicative.Combinators (optional)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

import Control.Applicative hiding (many, some)
import Data.Functor (void, ($>))
import Data.Maybe (catMaybes)
import Data.Void (Void)
import Data.List.Split

type Parser = Parsec Void String

number :: Parser Int
number = read <$> some digitChar

mulParser :: Parser Int
mulParser = do
  void $ string "mul("
  x <- number
  void $ char ','
  y <- number
  void $ char ')'
  pure $ x * y

mainParser :: Parser Int
mainParser = sum <$> some (try mulParser <|> takeP Nothing 1 $> 0)

main :: IO ()
main = do
  input <- getContents
  print $ parse $ getDos input
  where
    parse = runParser mainParser "test"

getDos :: String -> String
getDos = concatMap (head . splitOn "don't()") . splitOn "do()"
