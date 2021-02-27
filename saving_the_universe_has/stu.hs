main = interact $ unlines . zipWith (++)
   ["Case #" ++ show t ++ ": " | t <- [1..]] . solve . lines

solve (x:xs) = [show $ countEngines xs | x <- [1..(read x :: Int)]]

splitCases xs = [take (caseLen xs) xs, drop (caseLen xs) xs]

possible :: [String] -> [String]
possible (x:xs) = take (read x :: Int) xs

actual :: [String] -> [String] -> [String]
actual p = tail . drop (length p) 
-- actual p all = tail $ drop (length p) all

count x = length . filter (x==)

countEngines xs = [ count e a | e <- p ]
                    where
                            p = possible xs
                            a = actual p xs

caseLen arr = let x = (read (head arr) :: Int)
                in x + (read (arr !! (1 + x)) :: Int) + 2


