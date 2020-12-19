import Data.List

pairs l = [[x,y] | (x:ys) <- tails l, y <- ys]
triplets l = [[x,y,z] | (x:xs) <- tails l, (y:ys) <- tails xs, z <- ys]

sumIs2020 = (== 2020) . sum
prod = foldl (*) 1

main = do
    contents <- getContents
    let numbers = map read (lines contents) :: [Int]

let wanted = filter sumIs2020 (pairs numbers) !! 0
    putStrLn $ "The wanted pair is: " ++ (show wanted)
    putStrLn $ "And the product is: " ++ (show $ prod wanted)

let wanted = filter sumIs2020 (triplets numbers) !! 0
    putStrLn $ "The wanted triplet is: " ++ (show wanted)
    putStrLn $ "And the product is: " ++ (show $ prod wanted)

putStrLn "done!\n"