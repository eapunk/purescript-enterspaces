module Test.Text.Parsing.Parser.Json where

import Prelude
import Data.Either
import Test.Assert (assert')
import Text.Parsing.Parser (runParser)
import Text.Parsing.Parser.Json

main = do
    assert' "jsonString 1A" (runParser "\"\\uD834\\uDD1E\"" jsonString == Right "𝄞")
    assert' "jsonString 1a" (runParser "\"\\ud834\\udd1e\"" jsonString == Right "𝄞")
    assert' "jsonString 2" (runParser "\"Строка\"" jsonString == Right "Строка")
    assert' "jsonString 3" (runParser "\" \\\\ \\\" \\/ \\b \\f \\n \\r \\t \"" jsonString == Right " \\ \" / \b \f \n \r \t ")
    assert' "jsonString 4" (runParser "\"\\u04441\"" jsonString == Right "ф1")
    assert' "jsonString 100" (isLeft $ runParser "\"\\u044\"" jsonString)
    assert' "jsonString 101" (isLeft $ runParser "123" jsonString)
    assert' "jsonString 102" (isLeft $ runParser "\"123" jsonString)
