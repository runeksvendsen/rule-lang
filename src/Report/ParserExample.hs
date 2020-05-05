module Report.ParserExample where


import LangPrelude

import Control.Applicative (many, (<|>))
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char
import qualified Data.Text as T


type Parser = M.Parsec Void Text

pNewline :: Parser ()
pNewline = void $ Text.Megaparsec.Char.char '\n'

data Command = Hello | Exit
    deriving Show

pHello :: Parser Command
pHello = M.chunk "hello" *> return Hello

pExit :: Parser Command
pExit = M.chunk "exit" *> return Exit

pCommand :: Parser Command
pCommand = pHello <|> pExit

pLine :: Parser Command
pLine = pCommand <* pNewline

pSource :: Parser [Command]
pSource = many pLine

document :: Text
document = T.unlines
    [ "hello"
    , "hello"
    , "exit"
    , "hello"
    , "exit"
    ]

p :: Text -> Either (M.ParseErrorBundle Text Void) [Command]
p = M.parse pSource ""
