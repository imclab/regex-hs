module Text.Regex.Parse (Regex, parseRegex) where
import System.Environment
import Text.Parsec
import Text.Parsec.Combinator
import Text.Parsec.Language
import Text.Parsec.String
import qualified Text.Parsec.Token as T

data Regex = Star Regex
           | Plus Regex
           | Question Regex
           | Union Regex Regex
           | Concat Regex Regex
           | Group Regex
           | Char Char
  deriving (Show)

showRegex                :: Regex -> String
showRegex (Star r)       = showRegex r ++ "*"
showRegex (Plus r)       = showRegex r ++ "+"
showRegex (Question r)   = showRegex r ++ "?"
showRegex (Union r1 r2)  = showRegex r1 ++ "|" ++ showRegex r2
showRegex (Concat r1 r2) = showRegex r1 ++ showRegex r2
showRegex (Group r)      = "(" ++ showRegex r ++ ")"
showRegex (Char c)       = [c]

main = do args <- getArgs
          let results = parse exprP "" (head args)
          case results of
            Left e  -> print e
            Right d -> do print d
                          putStrLn (showRegex d)

parseRegex :: String -> Either ParseError Regex
parseRegex = parse exprP ""

exprP :: Parser Regex
exprP = chainl1 concatP (symbol "|" >> return Union)

concatP :: Parser Regex
concatP = chainl1 basicsP (return Concat)

basicsP :: Parser Regex
basicsP = (charP <|> groupP) >>= postOpP

postOpP     :: Regex -> Parser Regex
postOpP reg =  starP reg <|> plusP reg <|> questionP reg <|> return reg

postfixOpGen                    :: String -> (Regex -> Regex) -> Regex -> Parser Regex
postfixOpGen sym construcor reg = symbol sym >> return (construcor reg)

starP :: Regex -> Parser Regex
starP = postfixOpGen "*" Star

plusP :: Regex -> Parser Regex
plusP = postfixOpGen "+" Plus

questionP :: Regex -> Parser Regex
questionP = postfixOpGen "?" Question

groupP :: Parser Regex
groupP = fmap Group (parens exprP)

charP :: Parser Regex
charP = fmap Char (upper <|> digit)

lexer  = T.makeTokenParser javaStyle
parens = T.parens lexer
symbol = T.symbol lexer
