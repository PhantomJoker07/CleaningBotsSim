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
    main 5 5 2 4 3 1 7 10