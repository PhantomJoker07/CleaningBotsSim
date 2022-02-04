module Simulation where

import System.IO.Unsafe
import System.Random
import Enviroment
import Representations
import Visualization
import Tools


runSimulation::Enviroment -> StdGen -> Int -> Int -> [Float]-> IO()    
runSimulation env gen index turnsMax dirtStatus = 
    let
        turn = (turnsMax - index)
        (newEnv,newGen,dirtCount) = changeEnviroment env gen turn
        envSize = (n env) * (m env) + 1
        dirS = percent dirtCount envSize
    in do
        printTurn turn
        printEnviroment env
        printDirtStatus dirS
        if index > 0 then do
            runSimulation newEnv newGen (index-1) turnsMax (dirS:dirtStatus)
        else do
            printEnd (dirS:dirtStatus)


initSimulation::Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int-> IO()
initSimulation n m t kidsCount robotCount robotType seed turnsCount= 
    let
        seedMaxBound = maxBound::Int
        simulationSeed::Int
        simulationSeed 
            | seed == 0 = unsafePerformIO (getStdRandom (randomR (0, seedMaxBound)))
            | otherwise = seed
        (env, gen, dirtCount) = initEnviroment n m t kidsCount robotCount robotType simulationSeed
        dirS = percent dirtCount (n*m)
    in do  
        printLegend 
        runSimulation env gen turnsCount turnsCount [dirS]