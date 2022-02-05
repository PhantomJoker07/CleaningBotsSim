module Kids where

import System.Random
import Representations
import Tools

     
isValidDirectionForMoveObstacles:: [[String]] -> [[Bool]] -> [[Bool]] -> Int -> Int -> Int -> Int -> Bool
isValidDirectionForMoveObstacles board corrals dirt i j dirI dirJ 
    | not (isInsideMatrix board (i+dirI) (j+dirJ)) = False
    | corrals !! (i+dirI) !! (j+dirJ) = False
    | dirt !! (i+dirI) !! (j+dirJ) = False
    | board !! (i+dirI) !! (j+dirJ) == emptyRep = True
    | not (board !! (i+dirI) !! (j+dirJ) == obstacleRep) = False
    | otherwise = isValidDirectionForMoveObstacles board corrals dirt (i+dirI) (j+dirJ) dirI dirJ


moveObstacles:: [[String]] -> (Int,Int) -> (Int,Int) -> [[String]]
moveObstacles board source direction =
    let
        (i,j) = source
        (dirI,dirJ) = direction
        newBoard
            | board !! i !! j == emptyRep = setMatrix board i j obstacleRep
            | board !! i !! j == kidRep = 
                let
                    tempBoard = moveMatrixValue board source (i+dirI,j+dirJ) emptyRep
                in
                    moveObstacles tempBoard (i+2*dirI,j+2*dirJ) (dirI,dirJ)
            | otherwise =  moveObstacles board (i+dirI,j+dirJ) (dirI,dirJ)
    in
        newBoard


cuadricleVal :: Int -> Int -> (Int,Int) -> (Int,Int)
cuadricleVal cuadricleIndex index position =
    let 
        cVals = [(0,0),(0,1),(0,2),(1,0),(1,1),(1,2),(2,0),(2,1),(2,2)]
        (iRef,jRef) = cVals !! cuadricleIndex
        (i,j) = cVals !! index
        (iPos,jPos) = position
        value = ((iPos + i - iRef),(jPos + j - jRef))
    in
        value


getCuadricleVals:: Int -> Int -> (Int,Int) -> [(Int,Int)] -> [(Int,Int)]
getCuadricleVals cuadricleIndex index position directions
    | index == 9 = directions
    | otherwise = getCuadricleVals cuadricleIndex (index+1) position ((cuadricleVal cuadricleIndex index position):directions)


generateDirtRandomly :: [[String]] -> [[Bool]] -> [[Bool]] -> (Int,Int) -> (Int,Int) -> StdGen -> ([[Bool]],StdGen)
generateDirtRandomly board corrals dirt kidOldPos kidNewPos gen =
    let
        (cuadricleIndex,gen0) = randomR (0, 8) gen::(Int, StdGen)  --determine wich cuadricle of size 3x3 we use to generate dirt
        cuadricleVals= getCuadricleVals cuadricleIndex 0 kidOldPos [] --get the positions in the board corresponding to the selected cuadricle
        kidsPos = getPositionsInMatrix board [kidRep] 0 0
        cuadricleKidsPos = filter (\x -> not (x == kidNewPos ) && (elem x cuadricleVals)) kidsPos --get the positions of the kids in the cuadricle
        cuadricleKidsCount = length cuadricleKidsPos
        maxDirtDef  --max dirt we can generate in said cuadricle
            | cuadricleKidsCount == 0 = 1
            | cuadricleKidsCount == 1 = 3
            | otherwise = 6

        --determine how many dirt will be generated taking in count the avaliable cells and de max dirt determined above
        tempValidPosToGenerate = getPositionsInMatrix board [emptyRep] 0 0
        temp2ValidPosToGenerate = filter (\x -> let (i,j) = x in not (dirt !! i !! j) && not (corrals !! i !! j)) tempValidPosToGenerate
        validPosToGenerate = filter (\x -> elem x cuadricleVals) temp2ValidPosToGenerate
        maxDirtToGenerate = minimum [length validPosToGenerate,maxDirtDef]
        (dirtToGenerateCount,gen1) = randomR (0, maxDirtToGenerate) gen0::(Int, StdGen)
        
        --generate the new dirt
        tempBoard = setPositionsInMatrix board specialRep validPosToGenerate (length validPosToGenerate - 1)
        (tempBoard2,newGen) = generateObjectsRandomly tempBoard dirtToGenerateCount dirtRep [specialRep] gen1 
        newPos = getPositionsInMatrix tempBoard2 [dirtRep] 0 0
        newDirt = setPositionsInMatrix dirt True newPos (length newPos - 1)
    in
        (newDirt,newGen)



moveSelectedKidsRandomly:: [[String]] -> [[Bool]] -> [[Bool]] -> [(Int,Int)] -> Int -> StdGen -> ([[String]], [[Bool]], StdGen)
moveSelectedKidsRandomly board corrals dirt kToMoveList index gen
    | index < 0 = (board,dirt,gen)
    | otherwise = 
        let
            (i,j) = kToMoveList !! index
            (rand,gen0) = randomR (0, 4) gen::(Int, StdGen)
            (dirI,dirJ) = getDirection rand
            _i = i+dirI
            _j = j+dirJ
            directionContent
                | not (isInsideMatrix board _i _j) = specialRep --Said direction is outside of the matrix, mark it as invalid
                | not (corrals !! _i !! _j) && not (dirt !! _i !! _j) = board !! _i !! _j  --There is no corral or a dirt in said direction, save the content
                | otherwise = specialRep
            isMoveObstacles
                | directionContent == obstacleRep = isValidDirectionForMoveObstacles board corrals dirt i j dirI dirJ --If the value in said direction is an obstacle check if it can be moved
                | otherwise = False
            (newBoard,itMoved)
                | directionContent == obstacleRep && isMoveObstacles = (moveObstacles board (i,j) (dirI,dirJ),False)--Said direction has a movable obstacle, move it
                | directionContent == emptyRep = (moveMatrixValue board (i,j) (_i,_j) emptyRep, True)--Said direction is empty, move towards it
                | otherwise = (board,False)
            (newDirt,newGen)
                | itMoved = generateDirtRandomly newBoard corrals dirt (i,j) (_i,_j) gen0 --If the kid moved proceed to generate the dirt randomly
                | otherwise = (dirt,gen0)
        in
            moveSelectedKidsRandomly newBoard corrals newDirt kToMoveList (index-1) newGen


moveRandomKids:: [[String]] -> [[Bool]] -> [[Bool]] -> Int -> StdGen -> ([[String]], [[Bool]], StdGen)
moveRandomKids board corrals dirt kidsCount gen =
    let
        (maxKidsToMoveCount,gen0) = randomR (0, kidsCount) gen::(Int, StdGen)
        tempKToMoveList = getPositionsInMatrix board [kidRep] 0 0
        corralsPos = getPositionsInMatrix corrals [True] 0 0
        kToMoveList =  filter (\x -> not (elem x corralsPos) ) tempKToMoveList
        (randKToMoveList,gen1) = shuffleList kToMoveList gen0
        kToMoveCount = minimum [length kToMoveList, maxKidsToMoveCount] - 1
        (newBoard, newDirt, newGen) = moveSelectedKidsRandomly board corrals dirt randKToMoveList kToMoveCount gen1
    in
        (newBoard, newDirt, newGen)