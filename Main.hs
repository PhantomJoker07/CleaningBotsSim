module App where

import System.IO.Unsafe
import System.Random
import Simulation

main::Int -> Int -> Int -> Int -> Int -> Int-> Int -> Int -> IO()
main n m t kidsCount robotCount robotType seed turnsCount = do
    initSimulation n m t kidsCount robotCount robotType seed turnsCount

--Example
test:: IO()
test = do
    main 10 10 2 15 5 1 2 100
    