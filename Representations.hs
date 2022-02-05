module Representations where


kidRep :: String
kidRep = "K"

robotRep :: String
robotRep = "R"

robotAndKidRep :: String
robotAndKidRep = "RK"

robotLoadedRep :: String
robotLoadedRep = "RL"

dirtRep :: String
dirtRep = "D"

corralRep :: String
corralRep = "C"

obstacleRep :: String
obstacleRep = "O"

specialRep :: String
specialRep = "X"

emptyRep :: String
emptyRep = ""

robotAloneSteps :: Int
robotAloneSteps = 1

robotLoadedSteps :: Int
robotLoadedSteps = 2

data Enviroment = Enviroment{
    n::Int,
    m::Int,
    t::Int,
    kidsCount::Int,
    robotCount::Int,
    robotType::Int,
    board::[[String]],
    corrals::[[Bool]],
    dirt::[[Bool]]
} deriving (Show)
