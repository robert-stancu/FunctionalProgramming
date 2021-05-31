module Parser where


import qualified Data.Bifunctor as BF
import Data.Char
import Control.Applicative


newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

parse :: Parser a -> String -> Maybe a
parse = (fmap fst .) . runParser


instance Functor Parser where
    fmap f (Parser p) = Parser (fmap (BF.first f) . p)


instance Applicative Parser where
    pure = success

    Parser p <*> Parser p' =
        Parser $ \s -> case p s of
            Just (f, s') -> BF.first f <$> p' s'
            Nothing      -> Nothing


instance Alternative Parser where
    empty = failure

    Parser p <|> Parser p' = Parser $ \s -> p s <|> p' s


failure :: Parser a
failure = Parser $ const Nothing

success :: a -> Parser a
success r = Parser $ \s -> Just (r, s)

spot :: (Char -> Bool) -> Parser Char
spot p = Parser f
  where
    f "" = Nothing
    f (x : xs)
        | p x       = Just (x, xs)
        | otherwise = Nothing

token :: Char -> Parser Char
token = spot . (==)

infixl 4 <?
(<?) :: (Applicative f, Alternative f) => f a -> f b -> f a
x <? y = (x <* y) <|> x
