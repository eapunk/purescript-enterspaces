module Test.Main where

import Prelude
import Test.Text.Show.Pretty as Pretty
import Test.Text.Parsing.Parser.Json as Json

main = do
    Pretty.main
    Json.main
