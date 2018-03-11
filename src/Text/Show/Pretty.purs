-- | Pretty debug trace

module Text.Show.Pretty
    ( prettify
    , FoldingOption(..)
    ) where

import Prelude hiding (between)
import Data.Either
import Data.Foldable (for_)
import Data.List
import Data.List.Lazy as LL
import Data.Maybe
import Data.String as Str
import Data.Tuple
import Control.Alt ((<|>))
import Control.Lazy (fix)
import Text.Parsing.Parser (Parser, runParser)
import Text.Parsing.Parser.Combinators (between, notFollowedBy, sepBy, try)
import Text.Parsing.Parser.Language (haskell)
import Text.Parsing.Parser.String (anyChar, char, satisfy, skipSpaces, string)
import Text.Parsing.Parser.Json (jsonString)
import Control.Monad.RWS


-- | Folding option for show.
data FoldingOption = UnfoldLevel Int | FoldToWidth Int

-- | Parse string and show it with options.
prettify :: {folding :: FoldingOption} -> String -> String
prettify options str = case runParser str exprsParser of
    Right expr -> prettyShow options expr
    _ -> str


-- | Expresssion data type. Representation of parsed text.
data Expr =
      Raw String
    | StringExpr String
    | CharExpr Char
    | ListExpr String String (List (List Expr))

-- | Parser for expressions.
exprsParser :: Parser String (List Expr)
exprsParser =
    many $ (try $ fix \p ->
            listExpr "{" "}" p
        <|> listExpr "[" "]" p
        <|> listExpr "(" ")" p
        <|> StringExpr <$> (try haskell.stringLiteral <|> jsonString)
        <|> CharExpr <$> try haskell.charLiteral
        <|> Raw <$> Str.singleton <$>
                (notFollowedBy (skipSpaces *> (satisfy \c -> c == '}' || c == ']' || c == ')' || c == ',')) *> anyChar)
        ) <|> (Raw <$> Str.singleton <$> anyChar)
    where
    listExpr leftBracket rightBracket p = ListExpr leftBracket rightBracket <$>
        between (string leftBracket <* skipSpaces) (skipSpaces *> string rightBracket)
            (many p `sepBy` (try $ skipSpaces *> char ',' <* skipSpaces))


type PrintState = {
    modeStack :: List (Maybe Mode),
    column :: Int,
    commitingOneLineMode :: Boolean,
    stage' :: Maybe {tokens :: List ShowToken, column :: Int} }

notPrinted :: PrintState
notPrinted = {
    modeStack: Nil,
    column: 0,
    commitingOneLineMode: false,
    stage': Nothing }

data Mode = OneLineMode | MultiLineMode
derive instance eqMode :: Eq Mode

data ShowToken = Regular String | OnlyIn Mode String | NewMode | EndMode
derive instance eqShowToken :: Eq ShowToken

-- | Show parsed expressions.
prettyShow :: {folding :: FoldingOption} -> List Expr -> String
prettyShow options exprs =
    let Tuple _ w = execRWS (prettyWrite exprs *> commit OneLineMode) options notPrinted
    in w
    where
    prettyWrite :: List Expr -> RWS {folding :: FoldingOption} String PrintState Unit
    prettyWrite Nil = pure unit
    prettyWrite (expr:restExprs) = do
        case expr of
            ListExpr leftBracket rightBracket (Nil:Nil) -> write_ leftBracket *> write_ rightBracket
            ListExpr leftBracket rightBracket ess -> do
                write_ leftBracket
                write NewMode
                nestingLevel <- getNestingLevel
                forL_ ess \{elem: es, isLast} -> do
                    write $ "\n" `onlyIn` MultiLineMode
                    void $ LL.replicateM nestingLevel $ write $ "    " `onlyIn` MultiLineMode
                    prettyWrite es
                    unless isLast do
                        write_ ","
                        write $ " " `onlyIn` OneLineMode
                write $ " " `onlyIn` MultiLineMode
                write_ rightBracket
                write EndMode
            Raw s -> write_ s
            StringExpr s -> do
                write_ "\""
                write_ $
                    Str.replaceAll (Str.Pattern "\"") (Str.Replacement "\\\"") $
                    Str.replaceAll (Str.Pattern "\\") (Str.Replacement "\\\\") s
                write_ "\""
            CharExpr c -> write_ "'" *> write_ (Str.singleton c) *> write_ "'"
        prettyWrite restExprs

    onlyIn str mode = OnlyIn mode str

    write_ str = write $ Regular str

    write token = do
        mode <- getMode
        let matched = case mode of
                Nothing -> true
                Just m -> case token of
                    OnlyIn m' _ -> m == m'
                    _ -> true
        when matched do
            state <- get
            let str = case token of
                    Regular s -> s
                    OnlyIn _ s -> s
                    _ -> ""
            if isNothing mode || isJust state.stage' && str /= "\n"
                then do
                    let engaged = case token of
                            Regular _ -> true
                            OnlyIn OneLineMode _ -> true
                            OnlyIn MultiLineMode _ -> case mode of
                                Just MultiLineMode -> true
                                _ -> false
                            _ -> false
                    let len = if engaged then Str.length str else 0
                    let stage = fromMaybe {tokens: Nil, column: state.column} state.stage'
                        stage2 = stage {tokens = token : stage.tokens, column = stage.column + len}
                    put state {stage' = Just stage2}
                    {folding} <- ask
                    case folding of
                        FoldToWidth maxWidth ->
                            when (stage2.column > maxWidth) do
                                commit MultiLineMode
                        _ -> pure unit
                else do
                    if str == "\n"
                        then commit OneLineMode *> modify \st -> st {column = 0}
                        else modify \st -> st {column = st.column + Str.length str}
                    tell str
            when (token == NewMode) do
                modeAfter mode >>= pushMode
            when (token == EndMode) do
                popMode

    modeAfter (Just OneLineMode) = pure $ Just OneLineMode
    modeAfter (Just MultiLineMode) = do
        {folding} <- ask
        case folding of
            UnfoldLevel maxLevel -> do
                nestingLevel <- getNestingLevel
                pure $ Just if nestingLevel >= maxLevel then OneLineMode else MultiLineMode
            FoldToWidth 0 -> pure $ Just MultiLineMode
            FoldToWidth _ -> do
                {commitingOneLineMode} <- get
                pure if commitingOneLineMode then Just OneLineMode else Nothing
    modeAfter Nothing = pure Nothing

    getNestingLevel = get >>= \st -> pure $ length st.modeStack

    getMode = get >>= \st -> pure $ fromMaybe (Just MultiLineMode) $ head st.modeStack

    pushMode mode = modify \st -> st {modeStack = mode : st.modeStack}

    popMode = do
        {modeStack} <- get
        let m = fromMaybe {head: (Just MultiLineMode), tail: Nil} (uncons modeStack)
        modify \st -> st {modeStack = m.tail}

    commit mode = do
        {stage'} <- get
        case stage' of
            Nothing -> pure unit
            Just stage -> do
                modify \st -> st {modeStack = dropWhile isNothing st.modeStack, stage' = Nothing}
                let tokens = reverse stage.tokens
                when (mode == OneLineMode) do
                    modify \st -> st {commitingOneLineMode = true}
                pushMode $ Just mode
                for_ tokens
                    write
                modify \st -> st {commitingOneLineMode = false}

-- | Traverse with information about position, ignoring the final result.
forL_ :: forall a b m. Applicative m => List a -> ({elem :: a, isLast :: Boolean} -> m b) -> m Unit
forL_ Nil _ = pure unit
forL_ (x:Nil) f = void $ f {elem: x, isLast: true}
forL_ (x:xs) f = f {elem: x, isLast: false} *> forL_ xs f
