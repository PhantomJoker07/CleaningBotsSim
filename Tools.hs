module Tools where

import System.Random
import Data.Array.ST
import Control.Monad
import Control.Monad.ST
import Data.STRef

--Creates a list of size "n" with the value "v" in all the positions
list :: Int -> a -> [a]
list n v | n == 0 = [] | otherwise = v:list (n-1) v

--Returns a copy of the original list with the value of the ith position changed to "value"
setList :: [a] -> Int -> a -> [a]
setList (x:xs) i value | i == 0 = (value:xs) | otherwise = x:(setList xs (i-1) value)

--Returns a copy of the original list with all the values equal to "y" removed
removeFromList::Eq a => [a] -> a -> [a]
removeFromList [] y = []
removeFromList (x:xs) y | x == y = xs | otherwise = (x:(removeFromList xs y))

--Creates a matrix (a list of lists) of size "n * m" with the value "v" in all the positions
matrix :: Int -> Int -> a -> [[a]]
matrix n m v | n == 0 = [] | otherwise = list m v: matrix(n-1) m v

--Returns a copy of the original matrix with the value of the position (i,j) changed to "value"
setMatrix :: [[a]] -> Int -> Int -> a -> [[a]]
setMatrix (xs:xxs) i j value | i == 0 = (setList xs j value):xxs | otherwise = xs:(setMatrix xxs (i-1) j value)

--Returns a list of all the values contained in the original matrix
matrixToList :: [[a]] -> [a] -> Int -> Int -> [a]
matrixToList mtx list i j  =
    let
        n = length mtx
        m = length (head mtx)
        dat = mtx !! i !! j
        (_i,_j)
            | i < n - 1 = (i + 1, j)
            | j < m - 1 = (0, j + 1)
            | otherwise = (- 1, - 1)
    in
        if _i == -1 then [dat]
        else matrixToList mtx (dat:list) _i _j 

--Returns a copy of the original matrix with the values of the positions indicated in the list ¨positions" changed to "value" 
setPositionsInMatrix :: [[a]] -> a ->  [(Int,Int)] -> Int -> [[a]]
setPositionsInMatrix matrix value positions index | index < 0 = matrix | otherwise =
    let
        (x,y) = positions !! index
        boardNew = setPositionsInMatrix matrix value positions (index-1)
    in 
        setMatrix boardNew x y value

--Returns a list with the positions in the original matrix wich contained a value listed in "cellType"
getPositionsInMatrix :: Eq a => [[a]] -> [a] -> Int -> Int -> [(Int,Int)]
getPositionsInMatrix matrix cellType i j  =
    let
        n = length matrix
        m = length (head matrix)
        dat = matrix !! i !! j
        (_i,_j)
            | i < n - 1 = (i + 1, j)
            | j < m - 1 = (0, j + 1)
            | otherwise = (- 1, - 1)
    in 
        if _i == -1 then
            if dat `elem` cellType then [(i,j)]
            else []
        else if dat `elem` cellType then (i,j): getPositionsInMatrix matrix cellType _i _j 
        else getPositionsInMatrix matrix cellType _i _j

--Tells if the position (x,y) is inside the given matrix
isInsideMatrix :: [[a]] -> Int -> Int -> Bool
isInsideMatrix matrix x y
    | x >= 0 && x < length matrix && y >= 0 && y < length (head matrix) = True
    | otherwise = False

--Get the vectors representing 4 avaliable directions
getDirection:: Int -> (Int,Int)
getDirection val 
    | val == 0 = (0,1)
    | val == 1 = (0,-1)
    | val == 2 = (1,0)
    | otherwise = (-1,0)

--Returns copy of the original matrix switching the position of the value located in "source" to "destination" while leaving in "source" the value "emptyValue"
moveMatrixValue :: [[a]] -> (Int,Int) -> (Int,Int) -> a -> [[a]]
moveMatrixValue matrix source destination emptyValue =
    let
        (srcX,srcY) = source
        (destX,destY) = destination
        value = matrix !! srcX !! srcY
        tempMatrix = setMatrix matrix destX destY value
        ansMatrix = setMatrix tempMatrix srcX srcY emptyValue
    in 
        ansMatrix 

--Returns copy of the original matrix with a number equal to "quantity" of values set to "cellType" in random positions of the board that had a value listed in "validCells"
generateObjectsRandomly::Eq a => [[a]] -> Int -> a -> [a] -> StdGen-> ([[a]], StdGen)
generateObjectsRandomly board quantity cellType validCells gen | quantity == 0 = (board,gen) | otherwise =
    let
        n = length board
        m = length (head board)
        (boardNew,gen2) = generateObjectsRandomly board (quantity-1) cellType validCells gen
        options =  [(i-1,j-1) | i <- [1..n], j <- [1..m], elem ((boardNew !! (i-1)) !! (j-1)) validCells]
        (pos,gen3) = randomR (0, length options - 1) gen2::(Int, StdGen)
        (x, y) = options !! pos
    in
        (setMatrix boardNew x y cellType, gen3)

--Returns a copy of the original list with the position of the values switched randomly
shuffleList :: [a] -> StdGen -> ([a],StdGen)
shuffleList xs gen = runST (do
        g <- newSTRef gen
        let randomRST lohi = do
              (a,s') <- liftM (randomR lohi) (readSTRef g)
              writeSTRef g s'
              return a
        ar <- newArray n xs
        xs' <- forM [1..n] $ \i -> do
                j <- randomRST (i,n)
                vi <- readArray ar i
                vj <- readArray ar j
                writeArray ar j vi
                return vj
        gen' <- readSTRef g
        return (xs',gen'))
  where
    n = length xs
    newArray :: Int -> [a] -> ST s (STArray s Int a)
    newArray n xs =  newListArray (1,n) xs

--Implementation of a for cicle
for :: Int -> Int -> Int -> a -> (a -> a) -> a
for val_init val_end inc input body =
    if val_init < val_end then
        for (val_init + inc) val_end inc (body input) body
    else input

--Percent of x in y in a float type
percent :: Int -> Int -> Float
percent x y =   100 * ( a / b )
  where a = fromIntegral x :: Float
        b = fromIntegral y :: Float

--Returns the mean pf a list of floats
mean :: [Float] -> Float
mean list = sum list / fromIntegral (length list)