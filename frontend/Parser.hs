{-# LANGUAGE OverloadedStrings, FlexibleInstances, FlexibleContexts #-}
module Parser (exprParser) where

import Text.Parsec
import Data.Text (Text, pack)
import qualified Data.Text as T
import Data.Monoid
import Types
import Control.Lens hiding ((:<))

type Parser a = Parsec Text () a

appendParser :: Monoid a => Parser a -> Parser a -> Parser a
appendParser l r = do
    lhs <- l
    rhs <- r
    return $ lhs <> rhs

instance (Monoid a) => Monoid (Parser a) where
    mempty = pure mempty
    mappend = appendParser

underscore :: Parser Char
underscore = char '_'

toList :: a -> [a]
toList = return

ident :: Parser String
ident = do
    a <- letter <|> underscore
    rest <- many $ letter <|> underscore <|> digit
    return $ a:rest

lowercaseIdent :: Parser Text
lowercaseIdent = pack <$> (toList <$> lower <|> toList <$> underscore) <> (many $ letter <|> underscore <|> digit)

uppercaseIdent :: Parser Text
uppercaseIdent = pack <$> (toList <$> upper) <> (many $ letter <|> underscore <|> digit)

letExpr :: Parser Expr
letExpr = do
    string "let"
    spaces
    pat <- patternExpr
    spaces
    char '='
    spaces
    body <- Parser.expr
    spaces
    return $ Let pat body

typeExpr :: Parser TypeExpr
typeExpr = do
    theType <- uppercaseIdent
    return $ TypeAtom theType

letRecExpr :: Parser Expr
letRecExpr = do
    string "let"
    spaces
    string "rec"
    spaces
    pat <- patternExpr
    spaces
    char '='
    spaces
    body <- Parser.expr
    spaces
    return $ LetRec pat body

integerLiteral :: Parser Int
integerLiteral = read <$> (try $ many1 digit)
-- read <$> (try (toList <$> char '-') <> many1 digit <|> many1 digit)

integer = IntVal <$> integerLiteral
integerExpr = Constant <$> integer

term :: Parser Expr
term = try funApply <|> try parenExpr <|> try varExpr <|> integerExpr

varExpr = Var <$> varSym

funApply :: Parser Expr
funApply = do
    spaces
    fname <- varSym
    spaces
    args <- sepEndBy1 (try parenExpr <|> try varExpr <|> try integerExpr) spaces
    spaces
    return $ FunApply fname args

mulClassOp, addClassOp, compClassOp :: Parser InfixOp
mulClassOp = try (spaces >> char '*' >> spaces >> return Mul) <|>
             try (spaces >> char '/' >> spaces >> return Div) <|>
             try (spaces >> string "*." >> spaces >> return MulDot) <|>
             try (spaces >> string "/." >> spaces >> return DivDot)

addClassOp = try (spaces >> char '+' >> spaces >> return Plus) <|>
             try (spaces >> char '-' >> spaces >> return Minus) <|>
             try (spaces >> string "+." >> spaces >> return PlusDot) <|>
             try (spaces >> string "-." >> spaces >> return MinusDot)
compClassOp = try (spaces >> string "<" >> spaces >> (return $ Compare LessThan)) <|>
              try (spaces >> string "<=" >> spaces >> (return $ Compare LessThanEq)) <|>
              try (spaces >> string ">" >> spaces >> (return $ Compare GreaterThan)) <|>
              try (spaces >> string ">=" >> spaces >> (return $ Compare GreaterThanEq))

mathExpr :: Parser Expr
mathExpr = term `chainl1` (mkOpExpr =<< mulClassOp) `chainl1` (mkOpExpr =<< addClassOp) `chainl1` (mkOpExpr =<< compClassOp)
  where
    mkOpExpr :: InfixOp -> Parser (Expr -> Expr -> Expr)
    mkOpExpr op = try (spaces *> (return $ flip InfixOpExpr op) <* spaces)



patternExpr :: Parser Pattern
patternExpr = try funcPattern <|> try constantPattern <|> try varPattern <|> try parenPattern <|> try orPattern <|> try listPattern
  where
    constantPattern, varPattern, parenPattern, parenPatternWithType, orPattern :: Parser Pattern
    constantPattern = ConstantPattern Nothing . IntVal <$> integerLiteral
    varPattern = VarPattern Nothing <$> varSym
    parenPattern = try (between (char '(') (char ')') patternExpr)
                   <|> parenPatternWithType
    parenPatternWithType = between (char '(') (char ')') $ do
        spaces
        pat <- patternExpr
        spaces
        typeexp <- char ':' >> spaces >> typeExpr
        spaces
        return $ ParenPattern (Just typeexp) (pat & patType .~ Just typeexp)
    funcPattern = do
        spaces
        funcName <- varSym
        spaces
        args <- sepEndBy1 varSym spaces
        spaces
        return $ FuncPattern Nothing funcName args
    orPattern = error "orPattern is not yet!"
    listPattern = error "listPattern is not yet!"

parenExpr :: Parser Expr
parenExpr = do
    spaces
    char '('
    spaces
    ex <- expr
    spaces
    char ')'
    spaces
    return $ Paren ex


reservedWords :: [Text]
reservedWords = ["and", "as", "assert", "asr", "begin", "class", "closed", "constraint", "do", "done", "downto", "else", "end", "exception", "external", "false", "for", "fun", "function", "functor", "if", "in", "include", "inherit", "land", "lazy", "let", "lor", "lsl", "lsr", "lxor", "match", "method", "mod", "module", "mutable", "new", "of", "open", "or", "parser", "private", "rec", "sig", "struct", "then", "to", "true", "try", "type", "val", "virtual", "when", "while", "with"]
varSym :: Parser Sym
varSym = do
    name <- lowercaseIdent
    let isreserved = elem name reservedWords
    return . Sym $ name

ifThenElseExpr :: Parser Expr
ifThenElseExpr = try $ do
    string "if"
    spaces
    ifexp <- expr
    spaces
    string "then"
    spaces
    thenexp <- expr
    spaces
    string "else"
    spaces
    elseexp <- expr
    spaces
    return $ IfThenElse ifexp thenexp elseexp
        

expr :: Parser Expr
expr = spaces *> try ifThenElseExpr <|> try letRecExpr <|> try letExpr <|> try parenExpr <|> try mathExpr <|> try varExpr <* spaces

exprParser :: Parser Expr
exprParser = do
    x <- expr
    spaces
    eof
    return x
    
