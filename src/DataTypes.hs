{-# LANGUAGE ViewPatterns, NamedFieldPuns, RecordWildCards #-}

module DataTypes where

import Data.Char

data Client = GovOrg      String
            | Company     String Integer Person String
            | Individual  Person Bool
            deriving Show

data Person = Person String String
            deriving Show

data Gender = Male | Female | Unknown
            deriving Show

clientName :: Client -> String
clientName client = case client of
                      GovOrg  name                       -> name
                      Company name id person resp        -> name
                      Individual (Person fName lName _) _ -> fName ++ " " ++ lName

responsibility :: Client -> String
responsibility (Company _ _ _ r) = r
responsibility _                 = "Unknown"

specialClient :: Client -> Bool
specialClient (clientName -> "Mr. Alejandro") = True
specialClient (responsibility -> "Director")  = True
specialClient _

companyName :: Client -> Maybe String
companyName client = case client of
                       Company name _ _ _ -> Just name
                       _                  -> Nothing

fibonacci :: Integer -> Integer
-- LONG VERSION
-- fibonacci n = case n of
--                 0 -> 0
--                 1 -> 1
--                 _ -> fibonacci (n-1) + fibonacci (n-2)
-- SHORT VERSION
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n-1) + fibonacci (n-2)

f :: Client -> String
f client = case client of
             Company _ _ (Person name _ _) "Boss" -> name ++ " is the boss"
             _                                  -> "There is no boss"

g :: Client -> String
g client = case client of
             Company _ _ (Person name _ _) pos ->
               case pos of "Boss" -> name ++ " is the boss"
             _                               -> "There is no boss"

(+++) :: [a] -> [a] -> [a]
[]     +++ list2 = list2
(x:xs) +++ list2 = x:(xs +++ list2)
{-list1 +++ list2 = case list1 of
                    []   -> list2
                    x:xs -> x:(xs +++ list2)-}

sorted :: [Integer] -> Bool
sorted []  = True
sorted [_] = True
sorted (x : r@(y:_)) = x < y && sorted r
