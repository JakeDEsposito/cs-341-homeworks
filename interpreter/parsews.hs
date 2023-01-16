module ParseWS (parsews) where
import Text.ParserCombinators.ReadP
import Data.Char

parse :: ReadP a -> String -> a
parse p s
  | null parses        = error "There are no parses"
  | length parses > 1  = error "There is more than one parse"
  | otherwise          = head parses
    where parses = [x | (x,"") <- readP_to_S p s]

parsews :: ReadP a -> String -> a
parsews p s = parse p [c | c <- s, not (isSpace c)]