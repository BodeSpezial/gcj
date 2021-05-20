import Data.List
import Maybes

elemIndex' xs a = elemIndex a xs

elemIndices' xs ys = map (fromMaybe (99)) $ map (elemIndex' xs) ys

getShortestTail occurences xs
  | (maximum occurences) == 0 = []
  | otherwise = snd $ splitAt (maximum occurences + 1) xs

getShortestTail' possible actual = getShortestTail (elemIndices' actual possible) actual

f1 _ [] = []
f1 [] _ = []
f1 poss xs = xs : f1 poss (getShortestTail' poss xs)

-- checkLastLength :: [a] -> [a] -> [a]
checkLastLength poss comp
  | (length . last $ comp) > (length poss) = init comp
  | otherwise = comp

main :: IO ()
main = undefined



{-
import Control.Monad
main = do
        (\_ -> do cases <- getLine
        csL <- forM [1..(read cases :: Int)] (\_ -> do
                px <- scoopCases
                ax <- scoopCases
                return [px, ax])
        let
                switches = [countEngines x | x <- csL]
                minimums = map minimum switches
         mapM putStrLn ["Case #" ++ show t ++ ": " | t <- minimums])

scoopCases = do
        caseLen <- getLine
        cases <- forM [1..(read caseLen :: Int)] getCaseList
        return cases
        
getCaseList _ = do
        x <- getLine
        return x

countEngines [xs, ys] = [ count e ys | e <- xs ]

count x = length . filter (x==)

minL x [] = x
minL y (x:xs) = minL (min x y) xs
-}





--main = interact $ unlines . zipWith (++)
 --  ["Case #" ++ show t ++ ": " | t <- [1..]] . solve . lines

{-|
solve (x:xs) = [show $ countEngines xs | x <- [1..(read x :: Int)]]

splitCases xs = [take (caseLen xs) xs, drop (caseLen xs) xs]

possible :: [String] -> [String]
possible (x:xs) = take (read x :: Int) xs

actual :: [String] -> [String] -> [String]
actual p = tail . drop (length p) 
-- actual p all = tail $ drop (length p) all

caseLen arr = let x = (read (head arr) :: Int)
                in x + (read (arr !! (1 + x)) :: Int) + 2
-}

