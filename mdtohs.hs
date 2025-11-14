{-

This Haskell program converts an md file to an hs by removing
everything that is not Haskell code.

  * Lines between "```haskell" and "```" are copied.

  * However, lines between "```hs" and "```" are not copied,
    deliberately. This is to be able to control what to copy or not,
    while still retaining syntax highlighting in both cases.

-}

import Data.Char

begin = "```haskell"
end   = "```"

mdtohs :: [String] -> String
mdtohs [] = []
mdtohs (xs:xss)
  | take (length begin) (dropWhile isSpace xs) == begin  = copy (length (takeWhile isSpace xs)) xss
  | otherwise                                            = mdtohs xss

copy :: Int -> [String] -> String
copy i [] = []
copy i (xs:xss)
  | take (length end) (dropWhile isSpace xs) == end  = "\n" ++ mdtohs xss
  | otherwise                                        = drop i xs ++ "\n" ++ copy i xss

main :: IO()
main = interact (mdtohs.lines)
