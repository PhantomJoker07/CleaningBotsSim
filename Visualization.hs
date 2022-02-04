module Visualization where

import Representations
import Tools

--  R_K  ->  robot carring a kid
--  R|K  ->  robot and kid
--   R   ->  robot
--   K   ->  kid
--  [O]  ->  obstacle
 
--    dirt            corral           normal    
--    _ _ _            _ _ _            _ _ _    
--  |       |        |C     C|        |       | 
--  |  R_K  |        |   K   |        |  [O]  | 
--  |*_ _ _*|        |C_ _ _C|        | _ _ _ | 


getCeiling:: Int -> String
getCeiling n
    | n == 0 = ""
    | otherwise = "  _____ " ++ getCeiling (n-1)


getCellStr:: String -> String
getCellStr str
    | str == emptyRep = "   "
    | str == kidRep = " k "
    | str == robotRep = " R "
    | str == robotLoadedRep = "R_k"
    | str == robotAndKidRep = "R|k"
    | str == obstacleRep = "[O]"
    | otherwise  = "#%!"


fillCell:: String -> String
fillCell content =
    let
        fillerA = "  "
        fillerB = "  |"
    in
        fillerA ++ content ++ fillerB


getTileUpperStr:: String -> String
getTileUpperStr _type
    | _type == "corral" = "C     C|"
    | _type == "dirt" = "       |"
    | _type == "normal" = "       |"
    | otherwise = "X     X|"


getTileDownerStr:: String -> String
getTileDownerStr _type
    | _type == "corral" = "C_____C|"
    | _type == "dirt" = "*_____*|"
    | _type == "normal" = " _____ |"
    | otherwise = "X_____X|"


getRowUpperStr:: [Bool] -> [Bool] -> Int -> Int-> String
getRowUpperStr corrals dirt index limit
    | index == limit = ""
    | corrals !! index = getTileUpperStr "corral" ++ getRowUpperStr corrals dirt (index+1) limit
    | dirt !! index = getTileUpperStr "dirt" ++ getRowUpperStr corrals dirt (index+1) limit
    | otherwise = getTileUpperStr "normal" ++ getRowUpperStr corrals dirt (index+1) limit


getRowDownerStr:: [Bool] -> [Bool] -> Int -> Int -> String
getRowDownerStr corrals dirt index limit
    | index == limit = ""
    | corrals !! index = getTileDownerStr "corral" ++ getRowDownerStr corrals dirt (index+1) limit
    | dirt !! index = getTileDownerStr "dirt" ++ getRowDownerStr corrals dirt (index+1) limit
    | otherwise = getTileDownerStr "normal" ++ getRowDownerStr corrals dirt (index+1) limit


getRowContent:: [String] -> Int -> Int -> String
getRowContent board index limit
    | index == limit = ""
    | otherwise = fillCell (getCellStr (board !! index)) ++ getRowContent board (index+1) limit


printRows::[[String]] -> [[Bool]] -> [[Bool]] -> Int -> Int -> Int -> IO()
printRows board corrals dirt index n m
    | index == n = putStrLn ""
    | otherwise =
        let 
            up = "|" ++ getRowUpperStr (corrals !! index) (dirt !! index) 0 m
            center = "|" ++ getRowContent (board !! index) 0 m
            down = "|" ++ getRowDownerStr (corrals !! index) (dirt !! index) 0 m
        in do
            putStrLn up
            putStrLn center
            putStrLn down
            printRows board corrals dirt (index+1) n m


printEnviroment:: Enviroment -> IO()
printEnviroment env =
    let
        _board = board env
        _corrals = corrals env
        _dirt = dirt env 
        _n = n env
        _m = m env
        ceiling = getCeiling _m
    in do
        putStrLn ceiling
        printRows _board _corrals _dirt 0 _n _m


printLegend:: IO()
printLegend = do
    putStr "\r\n"
    print ("LEGEND:")
    putStr "\r\n\n"
    print ("Cell contents: ")
    putStr "\r\n"
    print((getCellStr emptyRep) ++ " -> " ++ "Empty Cell")
    print((getCellStr kidRep) ++ " -> " ++ "Kid")
    print((getCellStr robotRep) ++ " -> " ++ "Robot")
    print((getCellStr robotLoadedRep) ++ " -> " ++ "Robot carring a kid")
    print((getCellStr robotAndKidRep) ++ " -> " ++ "Robot and kid")
    print((getCellStr obstacleRep) ++ " -> " ++ "Obstacle")
    putStr "\r\n\n"
    print ("Cell types:")
    putStr "\r\n"
    print("    dirt            corral           normal   ") 
    print("    _____            _____            _____   " )
    print("  |       |        |C     C|        |       | " )
    print("  |       |        |       |        |       | " )
    print("  |*_____*|        |C_____C|        | _____ | " )
    putStr "\r\n\n"


printTurn:: Int -> IO()
printTurn turn = do
    putStr "\r\n\n"
    print ("Turn # " ++ show turn)


printDirtStatus :: Float-> IO()
printDirtStatus dirtS =
    print ("Dirt: " ++ show (dirtS) ++ " %")


printEnd:: [Float] -> IO()
printEnd dirtStatus = 
    let
        turns = length dirtStatus - 2
        sturns = show turns
        meanPercent = show (mean dirtStatus)
        minPercent = show (minimum dirtStatus)
        maxPercent = show (maximum dirtStatus)
        overThreshold = filter(\x -> x > 60) dirtStatus
        percentThreshold = show (percent (length overThreshold) (turns + 1))
    in do
        putStr "\r\n\n"
        print ("Simulation Ended after " ++ sturns ++ " turns")
        putStr "\r\n"
        print ("Average dirt: " ++ meanPercent ++ " %")
        print ("Maximum dirt: " ++ maxPercent ++ " %")
        print ("Minimum dirt: " ++ minPercent ++ " %")
        print ("Turns over threshold: " ++  percentThreshold ++ " %")