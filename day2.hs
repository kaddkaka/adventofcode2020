import Text.Regex.Posix

getGroups :: String -> [String]
getGroups s = groupMatches
    where (_, _, _, groupMatches) = s =~ "([0-9]*)-([0-9]*) (.): ([a-z]*)" :: (String, String, String, [String])

parse :: [String] -> (Int, Int, Char, String)
parse (lo:hi:char:string:_) = (read lo, read hi, head char, string)

count :: Eq a => (a -> Bool) -> [a] -> Int
count pred list = length $ filter pred list

isValid :: (Int, Int, Char, String) -> Bool
isValid (lo, hi, char, password) = lo <= num && num <= hi
    where num = count (==char) password

xor False True = True
xor True False = True
xor _ _ = False

isTOPValid :: (Int, Int, Char, String) -> Bool
isTOPValid (fst, snd, char, string) = xor (char1 == char) (char2 == char)
    where char1 = string !! (fst-1)
          char2 = string !! (snd-1)

main = do
    contents <- getContents
    let parsed = map (parse . getGroups) (lines contents)
    putStrLn $ "The number of valid passwords are: " ++ (show $ count isValid parsed)
    putStrLn $ "The number of valid passwords are: " ++ (show $ count isTOPValid parsed)
    putStrLn "done!"
