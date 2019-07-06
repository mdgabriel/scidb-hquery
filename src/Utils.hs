{-# LANGUAGE Safe #-}
module Utils (checkMajorVersion
             ,cleanDoubleQuotes
             ,deEscapeSingleQuotes
             ,dot
             ,escapeSingleQuotes
             ,nolines
             ,replace
             ,strip
             ,stripUsing
             ,toDoubleQuotedStr
             ,toSingleQuotedStr
             ,wrap
             ,wrapUsing
             )
where

import Data.Char                (isSpace)
import Data.List                (findIndices,intercalate)
import Data.List.Split          (splitOn)
import Data.Maybe               (fromMaybe)
import Safe                     (lastDef,readMay)

-- | Given a comparison and a string return true or false:
--
-- > *Utils> checkMajorVersion (18>=) "18.1.0"
-- > True
-- > *Utils> checkMajorVersion (18>=) "19.3"
-- > False
-- > *Utils> checkMajorVersion (>=19) "19.3"
-- > True
-- > *Utils> checkMajorVersion (>=19) "18.1.0"
-- > False
--
checkMajorVersion :: (Int -> t) -> String -> t
checkMajorVersion f s = f (fromMaybe 0 (readMay (takeWhile (/='.') s) :: Maybe Int))

-- | 'strip' away leading and trailing space.
strip :: String -> String
strip = stripUsing isSpace

-- | If at all possible, 'wrap' a string @s@ into strings of length less
-- than or equal to @l@ breaking with 'isSpace'.
wrap :: Int -> String -> [String]
wrap = wrapUsing isSpace

-- | 'stripUsing' predicate @p@ leading and trailing @p x@ true.
stripUsing :: (a -> Bool) -> [a] -> [a]
stripUsing p = dropWhile p . reverse . dropWhile p . reverse

-- | 'wrapUsing' predicate @p@ a list @xs@ into lists of length less
-- than or equal to @l@ breaking on @p x@ true.
wrapUsing :: (a -> Bool) -> Int -> [a] -> [[a]]
wrapUsing p l s =
           if length s' < l || null is
           then [s']
           else hd:wrapUsing p l tl
    where s'       = stripUsing p s
          is       = findIndices p s'
          (hd, tl) = splitAt i s'
          i        = (lastDef (head is) . takeWhile (<=l)) is

-- | 'dot' = \".\"
dot    :: FilePath
dot     = "."

-- | Make a double quoted string from a single quoted string, a SciDB
-- string.

toDoubleQuotedStr :: String -> String
toDoubleQuotedStr = replace "\"" "'" . stripUsing (=='\'') . replace "\\'" "\""

-- | Make a single quoted string which is a SciDB string.

toSingleQuotedStr :: String -> String
toSingleQuotedStr s = "'" ++ escapeSingleQuotes s ++ "'"

-- | De-Escape single quotes.
deEscapeSingleQuotes :: String -> String
deEscapeSingleQuotes = replace "\\'" "'"

-- | Escape single quotes.
escapeSingleQuotes :: String -> String
escapeSingleQuotes = replace "'" "\\'" 

-- | Replaces newlines (\\n) with a space ( ).

nolines :: String -> String
nolines = replace "\n" " "

-- | Replace needle in a haystack with replacement.

replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace needle replacement haystack =
    intercalate replacement (splitOn needle haystack)

-- | Replace escaped double quotes (\") by single quotes (')
-- and remove double qoutes (").

cleanDoubleQuotes :: String -> String
cleanDoubleQuotes = replace "\"" "" . replace "\\\"" "'"
