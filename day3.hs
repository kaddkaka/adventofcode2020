
treeValue :: String -> Int -> Int
treeValue treeline pos
    | (cycle treeline) !! pos == '#' = 1
    | otherwise = 0

treeAcc step (acc, pos) treeline = (new_acc, new_pos)
    where new_acc = acc + (treeValue treeline pos)
          new_pos = pos + step

numTrees grid step = foldl (treeAcc step) (0, 0) grid

second (x:y:xs) = x : second xs;
second (x:xs) = x : second xs;
second _ = []

prod_fst = foldl f 1 where
    f acc (tree, _) = acc * tree

main = do
    contents <- getContents
    let grid = lines contents
    let found_trees = map (numTrees grid) [1, 3, 5, 7] ++ map (numTrees $ second grid) [1]
    let result = prod_fst found_trees
    print $ result
    putStrLn "done!"
