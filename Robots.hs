module Robots where

import System.Random
import Bfs
import Tools
import Representations


robotMoveAction :: [[String]] -> [[Int]] -> (Int,Int) -> (Int,Int) -> Int -> Int -> Bool -> [[String]]
robotMoveAction board distances target actualPos stepSize actionType isCleaning =
    let
        n = length board
        m = length (head board)
        (_i,_j) = actualPos
        path = getShortestPath distances [target]
        nextCell = getNextCell path stepSize
        tar = path !! (length path - 1) 
        (x,y) = nextCell
        leftType
            | actionType == 2 = kidRep
            | otherwise = emptyRep
        newBoard 
        -- Target is NOT reached in this turn
            | tar /= nextCell = 
                let 
                    newType1
                        | actionType == 1 = robotLoadedRep
                        | otherwise = robotRep
                    nB1 = setMatrix board _i _j newType1
                in
                    moveMatrixValue nB1 actualPos nextCell leftType
                                        
        -- Target is reached in this turn
            | otherwise = 
                let          
                    newType2
                        | actionType == 1 && not isCleaning = robotAndKidRep
                        | actionType == 1 && isCleaning = robotLoadedRep
                        | (board !! x !! y) /= kidRep = robotRep
                        | otherwise = robotLoadedRep
                    nB2 = setMatrix board _i _j newType2
                in 
                    moveMatrixValue nB2 actualPos nextCell leftType
    in
        newBoard


robotCleanAction :: [[String]] -> [[Bool]] -> (Int,Int) -> Int -> Int ->  Int ->[(Int,Int)] -> ([[String]],[[Bool]],[(Int,Int)])
robotCleanAction board dirt robotPos stepSize actionType rType takenTargets =
    let
        n = length board
        m = length (head board)
        (_i,_j) = robotPos
        dirtPos = getPositionsInMatrix dirt [True] 0 0
        validPos = filter (\x -> not (elem x takenTargets)) dirtPos
        (newBoard,newDirt,newTaken)
            --There is no avaliable dirty cell, stand by
            | length validPos == 0 = (board,dirt,takenTargets)
            --Robot is standing in a dirty cell, clean it 
            | elem robotPos dirtPos = let (i,j) = robotPos in (board, setMatrix dirt i j False,takenTargets)       
            | otherwise =
                let
                    distances = bfsInMatrix board [obstacleRep,robotAndKidRep,robotRep,robotLoadedRep,kidRep,specialRep] robotPos
                    target = getClosestTarget distances validPos
                    nTaken 
                        | rType == 0 = takenTargets
                        | otherwise = target:takenTargets
                    (a,b,c)
                        --There is no route to a dirt cell, stand by
                        | target == (-1,-1) = (board,dirt,takenTargets)    

                        --Go to the closest dirt to clean it up
                        | otherwise = (robotMoveAction board distances target robotPos stepSize actionType True, dirt, nTaken)
                in 
                    (a,b,c)
    in 
        (newBoard,newDirt,newTaken)


robotTargetAction :: [[String]] -> [[Bool]] -> [(Int,Int)] -> (Int,Int) -> Int -> Int-> Int -> [(Int,Int)] -> ([[String]],[[Bool]],[(Int,Int)])
robotTargetAction board dirt validCells actualPos stepSize actionType rType takenTargets 
                --No target avaliable go to clean:
                | length validCells == 0 = robotCleanAction board dirt actualPos stepSize actionType rType takenTargets

                --Search for the closest target:
                | otherwise = 
                    let
                        n = length board
                        m = length (head board)
                        (_i,_j) = actualPos
                        obstacles 
                            | actionType == 1 = [obstacleRep,robotAndKidRep,robotRep,robotLoadedRep,kidRep,specialRep]
                            | otherwise = [obstacleRep,robotAndKidRep,robotRep,robotLoadedRep,specialRep]
                        distances =  bfsInMatrix board obstacles actualPos
                        target = getClosestTarget distances validCells
                        nTaken 
                            | rType == 0 = takenTargets
                            | otherwise = target:takenTargets
                        (newBoard,newDirt,newTaken)
                            --There is no route to a target, go to clean:
                            | target == (-1,-1) = robotCleanAction board dirt actualPos stepSize actionType rType takenTargets

                            --Go to the closest target:
                            | otherwise =   (robotMoveAction board distances target actualPos stepSize actionType False, dirt, nTaken)
                    in
                        (newBoard,newDirt,newTaken)


robotsAloneActions :: [[String]] -> [[Bool]] -> [[Bool]] -> [(Int,Int)] -> Int -> Int -> [(Int,Int)] -> ([[String]],[[Bool]],[(Int,Int)])
robotsAloneActions board corrals dirt robotsAlone index rType takenTargets
    | index == length robotsAlone = (board,dirt,takenTargets)
    | otherwise =
        let
            robotPos = robotsAlone !! index
            kidsPos = getPositionsInMatrix board [kidRep] 0 0
            corralsPos = getPositionsInMatrix corrals [True] 0 0
            validKids = filter (\x -> not (elem x corralsPos) && not (elem x takenTargets)) kidsPos
            markedKids = filter (\x -> elem x corralsPos) kidsPos
            tempBoard = setPositionsInMatrix board specialRep markedKids (length markedKids - 1)
            -- Try to catch a kid first
            (tempBoard2,newDirt, newTaken) = robotTargetAction tempBoard dirt validKids robotPos robotAloneSteps 0 rType takenTargets
            newBoard = setPositionsInMatrix tempBoard2 kidRep markedKids (length markedKids - 1)
        in 
            robotsAloneActions newBoard corrals newDirt robotsAlone (index+1) rType newTaken


robotsLoadedActions :: [[String]] -> [[Bool]] -> [[Bool]] -> [(Int,Int)] -> Int -> Int -> [(Int,Int)]-> ([[String]],[[Bool]],[(Int,Int)])
robotsLoadedActions board corrals dirt robotsLoaded index rType takenTargets
    | index == length robotsLoaded = (board,dirt,takenTargets)
    | otherwise =
        let
            robotLoadedPos = robotsLoaded !! index
            (_i,_j) = robotLoadedPos
            corralsPos = getPositionsInMatrix corrals [True] 0 0
            validCorrals = filter (\x -> let (i,j) = x in (board !! i !! j) == emptyRep && not (elem x takenTargets)) corralsPos
            -- Try to carry the kid to a corral first
            (newBoard, newDirt, newTaken) = robotTargetAction board dirt validCorrals robotLoadedPos robotLoadedSteps 1 rType takenTargets
        in
            robotsLoadedActions newBoard corrals newDirt robotsLoaded (index+1) rType newTaken


robotsAndKidsActions :: [[String]] -> [[Bool]] -> [[Bool]] -> [(Int,Int)] -> Int -> Int -> [(Int,Int)] -> ([[String]],[(Int,Int)])
robotsAndKidsActions board corrals dirt robotsAndKids index rType takenTargets
    | index == length robotsAndKids = (board,takenTargets)
    | otherwise =
        let
            robotPos = robotsAndKids !! index
            kidsPos = getPositionsInMatrix board [kidRep] 0 0
            corralsPos = getPositionsInMatrix corrals [True] 0 0
            validKids = filter (\x -> not (elem x corralsPos) && not (elem x takenTargets)) kidsPos
            markedKids = filter (\x -> elem x corralsPos) kidsPos
            tempBoard = setPositionsInMatrix board specialRep markedKids (length markedKids - 1)
            -- Try to catch a kid first
            (tempBoard2,newDirt,newTaken) = robotTargetAction tempBoard dirt validKids robotPos robotAloneSteps 2 rType takenTargets
            newBoard = setPositionsInMatrix tempBoard2 kidRep markedKids (length markedKids - 1)
        in 
            robotsAndKidsActions newBoard corrals newDirt robotsAndKids (index+1) rType newTaken


robotActions :: [[String]] -> [[Bool]] -> [[Bool]] -> Int -> StdGen -> ([[String]],[[Bool]],StdGen)
robotActions board corrals dirt rType gen = 
    let
        tempRobotsAlone = getPositionsInMatrix board [robotRep] 0 0
        tempRobotsAndKids =  getPositionsInMatrix board [robotAndKidRep] 0 0
        tempRobotsLoaded = getPositionsInMatrix board [robotLoadedRep] 0 0

        (robotsAlone,gen0)
            | rType == 0 = shuffleList tempRobotsAlone gen
            | otherwise = (tempRobotsAlone,gen)
        (robotsAndKids,gen1)
            | rType == 0 = shuffleList tempRobotsAndKids gen0
            | otherwise = (tempRobotsAndKids,gen0)
        (robotsLoaded,gen2)
            | rType == 0 =  shuffleList tempRobotsLoaded gen1
            | otherwise = (tempRobotsLoaded,gen1)
        
        (boardA, dirtA, takenA) = robotsAloneActions board corrals dirt robotsAlone 0 rType []
        (boardB,takenB) = robotsAndKidsActions boardA corrals dirtA robotsAndKids 0 rType takenA
        (newBoard,newDirt,takenC) = robotsLoadedActions boardB corrals dirtA robotsLoaded 0 rType takenB
        newGen = gen
    in
        (newBoard,newDirt,newGen)