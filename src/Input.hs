module Input
  ( readProgramFromFile
  ) where

import           Data.Char

c x = ord x - ord '0'

negateHead []     = []
negateHead (x:xs) = -x : xs

parse acc []        = [acc]
parse acc ('\n':xs) = parse acc []
parse acc (' ':xs)  = parse acc xs
parse acc ('\t':xs) = parse acc xs
parse acc (',':xs)  = acc : parse 0 xs
parse acc ('-':xs)  = negateHead $ parse 0 xs
parse acc (x:xs)    = parse (acc * 10 + c x) xs

readProgramFromFile :: string -> IO [Int]
readProgramFromFile filename = do
  programString <- readFile "input.txt"
    -- print programString
  return (parse 0 programString)
