module Main where

type Coord = (Int, Int)

getNeightbors :: Coord -> [Coord]
getNeightbors (x, y) = [(x - 1, y - 1), (x, y - 1), (x + 1, y - 1),
                        (x - 1, y    ),             (x + 1, y    ),
                        (x - 1, y + 1), (x, y + 1), (x + 1, y + 1)]

getLongNeightbors :: Board -> Coord -> [Coord]
getLongNeightbors board (x,y) = validCoords board $ map (getCandidates (x,y)) directions
    where
        dimensions = getDimension board
        directions = filter (\x -> case x of
            (0,0) -> False
            _ -> True) [(i,j) | i <- [-1..1] , j <- [-1..1]]
        getCandidates (x, y) (dx, dy)
            | newX < 0 || newX >= fst dimensions = (newX, newY)
            | newY < 0 || newY >= snd dimensions = (newX, newY)
            | getState board (newX,newY) /= '.'  = (newX, newY)
            | otherwise                          = getCandidates (newX, newY) (dx, dy)
            where
                newX = x + dx
                newY = y + dy

type Dimension = (Int, Int)

getCoords :: Board -> [[Coord]]
getCoords board = generateCoords $ getDimension board
    where generateCoords (x,y) = [[(i,j) | i <- [0..x-1]] | j <- [0..y-1]]

getDimension :: [[a]] -> Dimension
getDimension xs = (length $ head xs, length xs)

validCoords :: Board -> [Coord] -> [Coord]
validCoords board coords = filter areValid coords
    where
        areValid (x,y) = validX x && validY y
        validX x = x >= 0 && x < fst dimensions
        validY y = y >= 0 && y < snd dimensions
        dimensions = getDimension board

type Board = [String]

getState :: Board -> Coord -> Char
getState board (x,y) = head $ drop x $ head $ drop y board

evolveBoard :: Board -> Board
evolveBoard origBoard = map (map (\p -> newState (getValidNeightbors p) ( getState origBoard p))) coords
    where
        coords = getCoords origBoard
        getValidNeightbors p = validCoords origBoard $ getNeightbors p
        newState :: [Coord] -> Char -> Char
        newState neightbors state = case state of
            'L' -> if allEmpty then '#' else 'L'
            '#' -> if fourOrMoreOccupied then 'L' else '#'
            otherwise -> '.'
            where
                allEmpty = countStates 'L' + countStates '.' == length neightbors
                fourOrMoreOccupied = countStates '#' >= 4
                countStates state = sum $ map (fromEnum . (state ==) . (getState origBoard)) $ neightbors

evolveBoard2 :: Board -> Board
evolveBoard2 origBoard = map (map (\p -> newState (getValidNeightbors p) ( getState origBoard p))) coords
    where
        coords = getCoords origBoard
        getValidNeightbors = getLongNeightbors origBoard
        newState :: [Coord] -> Char -> Char
        newState neightbors state = case state of
            'L' -> if allEmpty then '#' else 'L'
            '#' -> if fiveOrMoreOccupied then 'L' else '#'
            otherwise -> '.'
            where
                allEmpty = countStates 'L' == length neightbors
                fiveOrMoreOccupied = countStates '#' >= 5
                countStates state = sum $ map (fromEnum . (state ==) . (getState origBoard)) $ neightbors

equalBoards :: Board -> Board -> Bool
equalBoards x y = elem 0 equalElements == False
    where
        equalElements = map product equalMatrix
        equalMatrix = map (map equalValues) (getCoords x)
        equalValues p = fromEnum (getState x p == getState y p)

iterateBoard :: Board -> Board
iterateBoard board = untilEquals board (newBoard board)
    where
        newBoard board = (evolveBoard2 board)
        untilEquals :: Board -> Board -> Board
        untilEquals x y | equalBoards x y = x
        untilEquals x y = untilEquals y $ newBoard y

occupiedSeats :: Board -> Int
occupiedSeats board = sum equalElements
    where
        equalElements = map sum equalMatrix
        equalMatrix = map (map equalValues) (getCoords board)
        equalValues = (fromEnum . ('#'==) . (getState board))

main :: IO ()
main = do
    raw <- readFile "input.txt"
    let board = lines raw
    let finalBoard = iterateBoard board
    let num = occupiedSeats finalBoard

    print num

