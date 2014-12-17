module Lambda.Parser (
    Lambda.Parser.parse,
    Lambda.Parser.parseRaw
) where

import Text.Parsec
import Text.Parsec.String

import Lambda.Variable
import Lambda.Engine

lambdaExprParser :: Parser Term
lambdaExprParser =
    do
        expr <- lambdaExpr
        eof
        return expr

lambdaExpr :: Parser Term
lambdaExpr =
    do
        char 'λ'
        var <- many1 alphaNum
        char '.'
        expr <- lambdaExpr
        return $ Lambda (Variable var) expr
    <|> do
        apps <- many1 lambdaTerm
        return $ foldl1 Apply apps

lambdaTerm :: Parser Term
lambdaTerm =
    do
        var <- many1 alphaNum
        return $ VarTerm $ Variable var 
    <|> do
        char '('
        expr <- lambdaExpr
        char ')'
        return expr

parse :: String -> Maybe Term
parse input =
    case Text.Parsec.parse lambdaExprParser "" input of
        Left _     -> Nothing --trace (show err) Nothing
        Right term -> Just term

parseRaw :: String -> Term
parseRaw input =
    case Text.Parsec.parse lambdaExprParser "" input of
        Left err   -> error $ show err
        Right term -> term
