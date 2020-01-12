module Input (readProgramFromFile) where

import Data.Char

c x = ord x - ord '0'

parse acc []          = [acc]
parse acc ('\n' : xs) = parse acc []
parse acc (',' : xs)  = acc : parse 0 xs
parse acc (x   : xs)  = parse (acc * 10 + (c x)) xs

readProgramFromFile :: string -> IO [Int]
readProgramFromFile filename = do
    programString <- readFile "input.txt"
    -- print programString
    return (parse 0 programString)


