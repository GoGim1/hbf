module Parser ( parseP ) where

import Instruction ( Instruction(..) )
import Text.ParserCombinators.Parsec
    ( Parser, oneOf, between, char, many, (<|>), eof, parse )

simpleOp :: Parser Instruction
simpleOp = charToIns <$> oneOf "<>+-.," 
    where charToIns '<' = Prev
          charToIns '>' = Next
          charToIns '+' = Inc
          charToIns '-' = Dec
          charToIns '.' = Write
          charToIns ',' = Read
          charToIns _ = error "unexpected error"

loop :: Parser Instruction
loop = Loop <$> between (char '[') (char ']') operation

operation :: Parser [Instruction]
operation = many $ simpleOp <|> loop

program :: Parser [Instruction]
program = operation <* eof

parseP :: String -> [Instruction]
parseP input = case parse program "bf interpreter" input of
                Left err -> error . show $ err
                Right ins -> ins