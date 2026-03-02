module Bob (responseFor) where

import Data.Char

-- Return whether all elements are capital A-Z
-- This is kinda weird, because we actually want is 
--   does this have ANY caps?
--   AND are all the alpha in this caps?
isAllCaps :: String -> Bool
isAllCaps str = 
  any isAlpha str
  && all isUpper (filter isAlpha str)

-- Get the last element of a list if it exists
lastElem :: [a] -> Maybe a
lastElem = foldl (\_ x -> Just x) Nothing

-- Return whether a string is all whitespace
isAllWhitespace :: String -> Bool
isAllWhitespace = all isSpace

-- Return whether the last element of the given string is '?', if it exists
isQuestion :: String -> Bool
isQuestion = maybe False (== '?') . lastElem

-- "" => "Fine. Be that way!"
-- isQuestion && isAllCaps => "Calm down, I know what I'm doing!"
-- isQuestion && not isAllCaps =>  "Sure."
-- not isQuestion && isAllCaps => "Whoa, chill out!"
-- otherwise = "Whatever."
responseFor :: String -> String
responseFor xs 
  | isAllWhitespace xs          = "Fine. Be that way!"
  | question && yelling         = "Calm down, I know what I'm doing!"
  | question                    = "Sure."
  | yelling                     = "Whoa, chill out!"
  | otherwise                   = "Whatever."
  where
    nonspace = filter (not . isSpace) xs
    question = isQuestion nonspace
    yelling = isAllCaps nonspace

