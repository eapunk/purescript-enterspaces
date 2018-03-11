-- | Parsers for JSON.

module Text.Parsing.Parser.Json where

import Prelude
import Control.Alt ((<|>))
import Data.Char as C
import Data.Char.Unicode as U
import Data.Int as Int
import Data.List as L
import Data.List.Lazy as LL
import Data.Maybe (Maybe(..))
import Data.String as Str
import Text.Parsing.Parser (ParserT, fail)
import Text.Parsing.Parser.String (class StringLike, char, satisfy)

-- | Parser for JSON string.
jsonString :: forall s m. StringLike s => Monad m => ParserT s m String
jsonString = do
    cs <- char '"' *> L.many jsonStringChar <* char '"'
    pure $ Str.fromCharArray $ L.toUnfoldable cs

jsonStringChar :: forall s m. StringLike s => Monad m => ParserT s m Char
jsonStringChar = (satisfy \c -> c /= '"' && c /= '\\') <|> jsonEscapedChar

jsonEscapedChar :: forall s m. StringLike s => Monad m => ParserT s m Char
jsonEscapedChar =
    char '\\' >>= \_ ->
        char '\\'
    <|> char '"'
    <|> char '/'
    <|> char 'b' $> '\b'
    <|> char 'f' $> '\f'
    <|> char 'n' $> '\n'
    <|> char 'r' $> '\r'
    <|> char 't' $> '\t'
    <|> do
        _ <- char 'u'
        cs <- LL.replicateM 4 $ satisfy U.isHexDigit
        let s = Str.fromCharArray $ LL.toUnfoldable cs
        case Int.fromStringAs Int.hexadecimal s of
            Just n -> pure $ C.fromCharCode n
            Nothing -> fail "Invalid hex string"
