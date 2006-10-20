#!/opt/local/bin/runhugs
{-
parser for a simple template language.

(mostly useful a haskell/parsec learning device for me)
-} 
import Text.ParserCombinators.Parsec (Parser, GenParser, many1, sepBy1, spaces, alphaNum, char, string, choice, between)
import qualified Text.ParserCombinators.Parsec as Parsec

import Control.Monad

type ListExpr = [ExprAtom]

data ExprAtom = String | ListExpr 
                deriving (Show)

data Atom = Lookup String
          | Command [String]
          | TextBlock String
          deriving (Show)

type Template = [Atom] 

parseIdentifier = many1 (choice [alphaNum, char '_'])
parseIdentifiers = sepBy1 parseIdentifier spaces

openLookup = string "{{"
closeLookup = string "}}"
closeCmd = string "%}"
openCmd = string "{%"

parseCommand = between openCmd closeCmd parseIdentifiers

parseLookup = Parsec.between openLookup closeLookup parseIdentifier 

parseTemplate::Parsec.GenParser Char b Template
parseTemplate = do
                  x <- Parsec.many1 parseAtom
                  Parsec.eof
                  return x

parseAtom::GenParser Char a Atom
parseAtom = do
              x <- Parsec.try(liftM Command parseCommand) 
                   Parsec.<|> do { 
              Parsec.try(Lookup `liftM` parseLookup) Parsec.<|> (liftM TextBlock parseTextBlock)  }
              return x

notOpen = Parsec.noneOf "{"

parseTextBlock = Parsec.many1 notOpen

run :: Show a => Parsec.Parser a -> String -> IO ()
run p input
        = case (Parsec.parse p "" input) of
            Left err -> do putStr ("parse error at " ++ (show err))
            Right x  -> print x


main = do
	document = parseTemplate "{%x%}{%y%}hi there{{whatsup}}"
	run parseTemplate " what do you think? {%hi%} i like {{glue}}"
	run parseTemplate "{%x%}{%y%}hi there{{whats up}}"
