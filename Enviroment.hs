module Enviroment where

import System.Random
import Representations
import Tools
import Kids
import Robots


getN:: Int -> Int
getN value | value < 0 = 0 | otherwise = value


initEnviroment::Int -> Int -> Int -> Int -> Int -> Int -> Int -> (Enviroment,StdGen,Int) --(Enviroment, StdGen)
initEnviroment  n m t kidsCount robotCount robotType seed =
    let
        (_,gen) = random (mkStdGen seed)::(Int, StdGen)
        boardBoolean = matrix n m False
        board = matrix n m emptyRep
        
        --Generate kids
        (kidsBoard,gen0) = generateObjectsRandomly board kidsCount kidRep [emptyRep] gen

        --Generate robots
        (robotBoard,gen1) = generateObjectsRandomly kidsBoard robotCount robotRep [emptyRep] gen0

        --Generate corral
        (corralBoard,gen2) = generateObjectsRandomly boardBoolean kidsCount True [False] gen1
        corralTiles = getPositionsInMatrix corralBoard [True] 0 0
        corralBoardX = setPositionsInMatrix robotBoard specialRep corralTiles (length corralTiles - 1)

        --Generate obstacles
        posMax = n * m - kidsCount * 2 - robotCount - 1
        obstaclesMax = getN (minimum [posMax,(kidsCount * 2 + robotCount)])
        (obstaclesCount,gen3) = randomR (0, obstaclesMax) gen2::(Int, StdGen)
        (obstaclesBoardX,gen4) = generateObjectsRandomly corralBoardX obstaclesCount obstacleRep [emptyRep] gen3
        obstaclesTiles = getPositionsInMatrix obstaclesBoardX [obstacleRep] 0 0
        obstaclesBoard = setPositionsInMatrix robotBoard obstacleRep obstaclesTiles (length obstaclesTiles - 1)

        --Generate dirt
        dirtMax = getN (posMax - obstaclesCount)
        (dirtCount,gen5) = randomR (0, dirtMax) gen4::(Int, StdGen)
        (dirtBoardDX,gen6) = generateObjectsRandomly obstaclesBoardX dirtCount dirtRep [emptyRep,robotRep] gen5
        dirtTiles = getPositionsInMatrix dirtBoardDX [dirtRep] 0 0
        dirtBoard = setPositionsInMatrix boardBoolean True dirtTiles (length dirtTiles - 1)

        --Generate enviroment
        env = Enviroment n m t kidsCount robotCount robotType obstaclesBoard corralBoard dirtBoard
        (_,newGen) = random gen6::(Int, StdGen)
    in
        (env,newGen,dirtCount)


changeEnviroment:: Enviroment -> StdGen -> Int -> (Enviroment,StdGen,Int)
changeEnviroment env gen turn =
    let
        _n = n env
        _m = m env
        _t = t env
        _kidsCount = kidsCount env
        _robotCount = robotCount env
        _robotType = robotType env
        _board = board env
        _corrals = corrals env
        _dirt = dirt env

        isChangeTurn
            | mod turn _t == 0 = True
            | otherwise = False

        (tempBoard, tempDirt, gen0) = robotActions _board _corrals _dirt _robotType gen
        (newBoard,newDirt,newGen)
            | isChangeTurn = moveRandomKids tempBoard _corrals tempDirt _kidsCount gen0
            | otherwise =  (tempBoard, tempDirt, gen0)

        dirtCount = length (getPositionsInMatrix newDirt [True] 0 0)
        newEnv = Enviroment _n _m _t _kidsCount _robotCount _robotType newBoard _corrals newDirt
    in
        (newEnv,newGen,dirtCount)