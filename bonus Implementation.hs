type Cell = (Int, Int)
data MyState = Null | S Cell [Cell] String MyState deriving (Show, Eq)

up :: MyState -> MyState
up (S (0, y) list string ms) = Null
up (S (x, y) list string ms) = (S (x - 1, y) list "up" (S (x, y) list string ms))

down :: MyState -> MyState
down (S (x, y) list string ms) = (S (x + 1, y) list "down" (S (x, y) list string ms))

left :: MyState -> MyState
left (S (x, 0) list string ms) = Null
left (S (x, y) list string ms) = (S (x, y - 1) list "left" (S (x, y) list string ms))

right :: MyState -> MyState
right (S (x, y) list string ms) = (S (x, y + 1) list "right" (S (x, y) list string ms))

collect :: MyState -> MyState
collect (S cell list string ms) = if not (any (== cell) list) then Null
                    else (S cell ([t | t <- list, not (t == cell)]) "collect" (S cell list string ms))

nextMyStates :: MyState -> [MyState]
nextMyStates ms = [t | t <- [up ms, down ms, left ms, right ms, collect ms], not (t == Null)]

isGoal :: MyState -> Bool
isGoal (S cell list string ms) = if length list == 0 then True else False

getDistances :: Cell -> [Cell] -> [Int] -- This is a helper method
getDistances x [] = []
getDistances (w, x) ((y, z) : t) = [abs (w - y) + abs (x - z)] ++ getDistances (w, x) t

help :: [Cell] -> [Int] -> Cell -> Int -> Cell -- This is a helper method
help [] [] acc1 acc2 = acc1
help (h1 : t1) (h2 : t2) acc1 acc2 = if h2 <= acc2 then (help t1 t2 h1 h2) else help t1 t2 acc1 acc2

getClosestCell :: [Cell] -> [Int] -> Cell -- This is a helper method
getClosestCell (h1 : t1) (h2 : t2) = help t1 t2 h1 h2

checkOpposites :: MyState -> Bool -- this is a helper method
checkOpposites (S cell cellList "" Null) = False
checkOpposites (S a t string (S b t2 string2 ms)) = (string == "up" && string2 == "down") || (string == "down" && string2 == "up") || (string == "right" && string2 == "left") || (string == "left" && string2 == "right")

search :: [MyState] -> MyState
search [] = Null
search (h:t) = if isGoal h then h else search (t ++ [x | x <- nextMyStates h, not (checkOpposites x)])
--search (h:t) = if isGoal h then h else search (t ++ removeRepeat (nextMyStates h))

constructSolution :: MyState -> [String]
constructSolution (S cell list "" Null) = []
constructSolution (S cell list string ms) = (constructSolution ms) ++ [string]

getPossibilities :: MyState -> [String] -- This is a helper method
getPossibilities ms = constructSolution (search [ms])

solveHelp :: Cell -> [Cell] -> Cell -> [String] -- This is a helper method
solveHelp cell [] cell2 = []
solveHelp cell cellList cell2 = getPossibilities (S cell [cell2] "" Null) 
    ++ solveHelp cell2 [t | t <- cellList, not (t == cell2)] (
        getClosestCell [t | t <- cellList, not (t == cell2)] (
            getDistances cell2 [t | t <- cellList, not (t == cell2)]))

solve :: Cell -> [Cell] -> [String]
solve cell cellList = solveHelp cell cellList (getClosestCell cellList (getDistances cell cellList))