{-# LANGUAGE InstanceSigs #-}

----------------------------------------------------------------------
-- |
--
-- CIS 194 Spring 2013: Homework 10
--
----------------------------------------------------------------------

module AParser where

-- base
import Control.Applicative
import Data.Char


newtype Parser a =
  Parser { runParser :: String -> Maybe (a, String) }


-- |
--
-- >>> runParser (satisfy isUpper) "ABC"
-- Just ('A',"BC")
-- >>> runParser (satisfy isUpper) "abc"
-- Nothing

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f []          = Nothing
    f (x:xs)
      | p x       = Just (x, xs)
      | otherwise = Nothing


-- |
--
-- >>> runParser (char 'x') "xyz"
-- Just ('x',"yz")

char :: Char -> Parser Char
char c = satisfy (== c)


posInt :: Parser Integer
posInt = Parser f
  where
    f xs
      | null ns   = Nothing
      | otherwise = Just (read ns, rest)
      where (ns, rest) = span isDigit xs


----------------------------------------------------------------------
-- Exercise 1
----------------------------------------------------------------------

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap func parser = Parser (fmap (first func) . runParser parser)

first :: (a -> b) -> (a, c) -> (b, c)
first func (a, c) = (func a, c)

----------------------------------------------------------------------
-- Exercise 2
----------------------------------------------------------------------

instance Applicative Parser where
  pure :: a -> Parser a
  pure x = Parser (\s -> Just (x, s))

  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  (<*>) pF pA =
      Parser func
        where
            func s =
                case runParser pF s of
                  Nothing -> Nothing
                  Just (f, s1) ->
                      case runParser pA s1 of
                        Nothing -> Nothing
                        Just (a, s2) -> Just (f a, s2)

----------------------------------------------------------------------
-- Exercise 3
----------------------------------------------------------------------

-- |
--
-- >>> runParser abParser "abcdef"
-- Just (('a','b'),"cdef")
-- >>> runParser abParser "aebcdf"
-- Nothing

abParser :: Parser (Char, Char)
abParser = undefined


-- |
--
-- >>> runParser abParser_ "abcdef"
-- Just ((),"cdef")
-- >>> runParser abParser_ "aebcdf"
-- Nothing

abParser_ :: Parser ()
abParser_ = undefined


-- |
--
-- >>> runParser intPair "12 34"
-- Just ([12,34],"")

intPair :: Parser [Integer]
intPair = undefined


----------------------------------------------------------------------
-- Exercise 4
----------------------------------------------------------------------

instance Alternative Parser where
  empty :: Parser a
  empty = undefined

  (<|>) :: Parser a -> Parser a -> Parser a
  (<|>) = undefined


----------------------------------------------------------------------
-- Exercise 5
----------------------------------------------------------------------

-- |
--
-- >>> runParser intOrUppercase "342abcd"
-- Just ((),"abcd")
-- >>> runParser intOrUppercase "XYZ"
-- Just ((),"YZ")
-- >>> runParser intOrUppercase "foo"
-- Nothing

intOrUppercase :: Parser ()
intOrUppercase = undefined
