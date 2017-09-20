{-# LANGUAGE OverloadedStrings, FlexibleInstances, FlexibleContexts #-}
module Parser (exprParser) where

import Text.Parsec
import Data.Text (Text, pack)
import qualified Data.Text as T
import Data.Monoid
import Types
import Control.Lens

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
    return $ LetRec pat body

integerLiteral :: Parser Int
integerLiteral = read <$> (try (toList <$> char '-') <> many1 digit <|> many1 digit)

term :: Parser Expr
term = (try $ Constant . IntVal . read <$> many1 digit) <|> try parenExpr

mulClassOp, addClassOp :: Parser InfixOp
mulClassOp = (spaces >> char '*' >> spaces >> return Mul)
             <|> (spaces >> char '/' >> spaces >> return Div)
             <|> (spaces >> string "*." >> spaces >> return MulDot)
             <|> (spaces >> string "/." >> spaces >> return DivDot)

addClassOp = (spaces >> char '+' >> spaces >> return Plus)
             <|> (spaces >> char '-' >> spaces >> return Minus)
             <|> (spaces >> string "+." >> spaces >> return PlusDot)
             <|> (spaces >> string "-." >> spaces >> return MinusDot)

mathExpr :: Parser Expr
mathExpr = term `chainl1` (mkOpExpr =<< mulClassOp) `chainl1` (mkOpExpr =<< addClassOp)
  where
    mkOpExpr :: InfixOp -> Parser (Expr -> Expr -> Expr)
    mkOpExpr op = return $ flip InfixOpExpr op

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
        args <- many1 varSym
        return $ FuncPattern Nothing funcName args
        
    orPattern = error "orPattern is not yet!" -- TODO
    listPattern = error "listPattern is not yet!"

parenExpr :: Parser Expr
parenExpr = do
    char '('
    spaces
    ex <- expr
    spaces
    char ')'
    return $ Paren ex

varSym :: Parser Sym
varSym = do
    name <- lowercaseIdent
    return . Sym $ name

varExpr = Var <$> varSym

expr :: Parser Expr
expr = try letRecExpr <|> try letExpr <|> try parenExpr <|> try mathExpr <|> try varExpr 

exprParser :: Parser Expr
exprParser = do
    x <- expr
    eof
    return x
    
