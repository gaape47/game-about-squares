{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE MultiParamTypeClasses,
             TypeSynonymInstances, FlexibleInstances #-}

module GAS where

import ProblemState

import qualified Data.Map.Strict as M

import qualified Data.Maybe as DM

{-
    Pozițiile tablei de joc, în formă (linie, coloană), unde ambele coordonate
    pot fi negative.
-}
type Position = (Int, Int)

{-
    Culorile pătratelor și cercurilor.
-}
data Color = Red | Blue | Gray
    deriving (Eq, Ord, Show)

{-
    Orientările pătratelor și săgeților.
-}
data Heading = North | South | East | West
    deriving (Eq, Ord)

instance Show Heading where
    show North = "^"
    show South = "v"
    show East  = ">"
    show West  = "<"

{-
    *** TODO ***

    Un obiect de pe tabla de joc: pătrat/ cerc/ săgeată.
-}
data Object = Square Color Heading | Circle Color | Arrow Heading
    deriving (Eq, Ord)

{-
    *** TODO ***

    Reprezetarea textuală a unui obiect.
-}
instance Show Object where
    show (Square Red h) = "R" ++ show h
    show (Square Blue h) = "B" ++ show h
    show (Square Gray h) = "G" ++ show h
    show (Circle Red) = "r"
    show (Circle Blue) = "b"
    show (Circle Gray) = "g"
    show (Arrow h) = show h

{-
    *** TODO ***

    Un nivel al jocului.

    Recomandăm Data.Map.Strict.
-}
data Level = CLevel (M.Map Position [Object])
    deriving (Eq, Ord)

minRow :: Level -> Int
minRow (CLevel lvl) = foldr (\a acc -> if (fst a) < acc then (fst a) else acc) (fst $ head $ M.keys lvl) (M.keys lvl)
minCol :: Level -> Int
minCol (CLevel lvl) = foldr (\a acc -> if (snd a) < acc then (snd a) else acc) (snd $ head $ M.keys lvl) (M.keys lvl)
maxRow :: Level -> Int
maxRow (CLevel lvl) = foldr (\a acc -> if (fst a) < acc then acc else (fst a)) (fst $ head $ M.keys lvl) (M.keys lvl)
maxCol :: Level -> Int
maxCol (CLevel lvl) = foldr (\a acc -> if (snd a) < acc then acc else (snd a)) (snd $ head $ M.keys lvl) (M.keys lvl)

getObj :: Level -> Int -> Int -> [Object]
getObj (CLevel lvl) i j
    | M.lookup (i, j) lvl == Nothing = []
    | otherwise = DM.fromJust (M.lookup (i, j) lvl)

getNrObj :: Level -> Int -> Int -> Int
getNrObj (CLevel lvl) i j = length $ getObj (CLevel lvl) i j

shwObj :: Level -> Int -> Int -> String
shwObj (CLevel lvl) i j
    | getNrObj (CLevel lvl) i j == 2 =
        show (head $ tail $ getObj (CLevel lvl) i j) ++ show (head $ getObj (CLevel lvl) i j)
    | getObj (CLevel lvl) i j == [Square Red North] = "R^ "
    | getObj (CLevel lvl) i j == [Square Red South] = "Rv "
    | getObj (CLevel lvl) i j == [Square Red East] = "R> "
    | getObj (CLevel lvl) i j == [Square Red West] = "R< "
    | getObj (CLevel lvl) i j == [Square Blue North] = "B^ "
    | getObj (CLevel lvl) i j == [Square Blue South] = "Bv "
    | getObj (CLevel lvl) i j == [Square Blue East] = "B> "
    | getObj (CLevel lvl) i j == [Square Blue West] = "B< "
    | getObj (CLevel lvl) i j == [Square Gray North] = "G^ "
    | getObj (CLevel lvl) i j == [Square Gray South] = "Gv "
    | getObj (CLevel lvl) i j == [Square Gray East] = "G> "
    | getObj (CLevel lvl) i j == [Square Gray West] = "G< "
    | getObj (CLevel lvl) i j == [Circle Red] = "  r"
    | getObj (CLevel lvl) i j == [Circle Blue] = "  b"
    | getObj (CLevel lvl) i j == [Circle Gray] = "  g"
    | getObj (CLevel lvl) i j == [Arrow North] = "  ^"
    | getObj (CLevel lvl) i j == [Arrow South] = "  v"
    | getObj (CLevel lvl) i j == [Arrow East] = "  >"
    | getObj (CLevel lvl) i j == [Arrow West] = "  <"
    | otherwise = "   "


{-
    *** TODO ***

    Reprezetarea textuală a unui nivel.
-}
instance Show Level where
    show (CLevel lvl) = foldr (\i acc1 ->
                                  if i == maxr then
                                  foldr (\j acc2 ->
                                             if j == maxc then shwObj (CLevel lvl) i j ++ acc2
                                             else shwObj (CLevel lvl) i j ++ "|" ++ acc2)
                                         ""
                                         [minc .. maxc] ++ acc1
                                  else foldr (\j acc2 ->
                                                  if j == maxc then shwObj (CLevel lvl) i j ++ acc2
                                                  else shwObj (CLevel lvl) i j ++ "|" ++ acc2)
                                              "\n"
                                              [minc .. maxc] ++ acc1)
                              ""
                              [minr .. maxr]
                        where minr = minRow (CLevel lvl)
                              minc = minCol (CLevel lvl)
                              maxr = maxRow (CLevel lvl)
                              maxc = maxCol (CLevel lvl)

{-
    *** TODO ***

    Nivelul vid, fără obiecte.
-}
emptyLevel :: Level
emptyLevel = CLevel (M.fromList [])

{-
    *** TODO ***

    Adaugă un pătrat cu caracteristicile date la poziția precizată din nivel.
-}
addSquare :: Color -> Heading -> Position -> Level -> Level
addSquare c h (i, j) (CLevel lvl)
    | M.member (i, j) lvl && (show $ head $ getObj (CLevel lvl) i j) == "^" =
          let newObj = reverse $ (Square c North) : objs; objs = getObj (CLevel lvl) i j
          in CLevel $ M.insert (i, j) newObj lvl
    | M.member (i, j) lvl && (show $ head $ getObj (CLevel lvl) i j) == "v" =
          let newObj = reverse $ (Square c South) : objs; objs = getObj (CLevel lvl) i j
          in CLevel $ M.insert (i, j) newObj lvl
    | M.member (i, j) lvl && (show $ head $ getObj (CLevel lvl) i j) == ">" =
          let newObj = reverse $ (Square c East) : objs; objs = getObj (CLevel lvl) i j
          in CLevel $ M.insert (i, j) newObj lvl
    | M.member (i, j) lvl && (show $ head $ getObj (CLevel lvl) i j) == "<" =
          let newObj = reverse $ (Square c West) : objs; objs = getObj (CLevel lvl) i j
          in CLevel $ M.insert (i, j) newObj lvl
    | otherwise =
          let newObj = reverse $ (Square c h) : objs; objs = getObj (CLevel lvl) i j
          in CLevel $ M.insert (i, j) newObj lvl

{-
    *** TODO ***

    Adaugă un cerc cu caracteristicile date la poziția precizată din nivel.
-}
addCircle :: Color -> Position -> Level -> Level
addCircle c (i, j) (CLevel lvl) = CLevel $ M.insert (i, j) newObj lvl
                                  where newObj = (Circle c) : objs
                                        objs = getObj (CLevel lvl) i j

{-
    *** TODO ***

    Adaugă o săgeată cu caracteristicile date la poziția precizată din nivel.
-}
addArrow :: Heading -> Position -> Level -> Level
addArrow h (i, j) (CLevel lvl) = CLevel $ M.insert (i, j) newObj lvl
                                 where newObj = (Arrow h) : objs
                                       objs = getObj (CLevel lvl) i j

{-
    *** TODO ***

    Mută pătratul de la poziția precizată din nivel. Dacă la poziția respectivă
    nu se găsește un pătrat, întoarce direct parametrul.
-}
move :: Position  -- Poziția
     -> Level     -- Nivelul inițial
     -> Level     -- Nivelul final
move (i, j) (CLevel lvl)
    | hasSquare (CLevel lvl) (i, j) && (getHeading $ getSquare $ getObj (CLevel lvl) i j) == North =
          let nr = findSquares (CLevel lvl) (i, j)
          in foldr (\a l -> moveSquare (a, j) North l) (CLevel lvl) (reverse [i - nr + 1 .. i])
    | hasSquare (CLevel lvl) (i, j) && (getHeading $ getSquare $ getObj (CLevel lvl) i j) == South =
          let nr = findSquares (CLevel lvl) (i, j)
          in foldr (\a l -> moveSquare (a, j) South l) (CLevel lvl) [i .. i + nr - 1]
    | hasSquare (CLevel lvl) (i, j) && (getHeading $ getSquare $ getObj (CLevel lvl) i j) == East =
          let nr = findSquares (CLevel lvl) (i, j)
          in foldr (\a l -> moveSquare (i, a) East l) (CLevel lvl) [j .. j + nr - 1]
    | hasSquare (CLevel lvl) (i, j) && (getHeading $ getSquare $ getObj (CLevel lvl) i j) == West =
          let nr = findSquares (CLevel lvl) (i, j)
          in foldr (\a l -> moveSquare (i, a) West l) (CLevel lvl) (reverse [j - nr + 1 .. j])
    | otherwise = (CLevel lvl)

moveSquare :: Position -> Heading -> Level -> Level
moveSquare (i, j) h (CLevel lvl)
    -- daca exista numai patrat pe pozitia (i, j)
    | hasSquare (CLevel lvl) (i, j) && getNrObj (CLevel lvl) i j == 1 =
          addSquare (getColor (head $ getObj (CLevel lvl) i j))
                    (getHeading (head $ getObj (CLevel lvl) i j))
                    (getPosition (i, j) h)
                    (CLevel (M.delete (i, j) lvl))
    -- patrat si cerc
    | hasSquare (CLevel lvl) (i, j) && hasCircle (CLevel lvl) (i, j) =
          addSquare (getColor (head $ tail $ getObj (CLevel lvl) i j))
                    (getHeading (head $ tail $ getObj (CLevel lvl) i j))
                    (getPosition (i, j) h)
                    (addCircle (getColor (head $ getObj (CLevel lvl) i j)) (i, j) (CLevel (M.delete (i, j) lvl)))
    -- patrat si sageata
    | hasSquare (CLevel lvl) (i, j) && hasArrow (CLevel lvl) (i, j) =
          addSquare (getColor (head $ tail $ getObj (CLevel lvl) i j))
                    (getHeading (head $ getObj (CLevel lvl) i j))
                    (getPosition (i, j) h)
                    (addArrow (getHeading (head $ getObj (CLevel lvl) i j)) (i, j) (CLevel (M.delete (i, j) lvl)))
    | otherwise = (CLevel lvl)


findSquares :: Level -> Position -> Int
findSquares (CLevel lvl) (i, j)
    | hasSquare (CLevel lvl) (i, j) && (getHeading $ getSquare $ getObj (CLevel lvl) i j) == North =
          let minr = minRow (CLevel lvl)
          in foldr (\a b -> if hasSquare (CLevel lvl) (a, j) && (a == i - b) then b + 1 else b) 0 [minr .. i]
    | hasSquare (CLevel lvl) (i, j) && (getHeading $ getSquare $ getObj (CLevel lvl) i j) == South =
          let maxr = maxRow (CLevel lvl)
          in foldr (\a b -> if hasSquare (CLevel lvl) (a, j) && (a == i + b) then b + 1 else b) 0 (reverse [i .. maxr])
    | hasSquare (CLevel lvl) (i, j) && (getHeading $ getSquare $ getObj (CLevel lvl) i j) == East =
          let maxc = maxCol (CLevel lvl)
          in foldr (\a b -> if hasSquare (CLevel lvl) (i, a) && (a == j + b) then b + 1 else b) 0 (reverse [j .. maxc])
    | hasSquare (CLevel lvl) (i, j) && (getHeading $ getSquare $ getObj (CLevel lvl) i j) == West =
          let minc = minCol (CLevel lvl)
          in foldr (\a b -> if hasSquare (CLevel lvl) (i, a) && (a == j - b) then b + 1 else b) 0 [minc .. j]
    | otherwise = 0

getSquare :: [Object] -> Object
getSquare lst
    | length lst == 2 = head $ tail $ lst
    | length lst == 1 && (length $ show $ head $ lst) == 2 = head $ lst
    | otherwise = (Square Red North)

hasSquare :: Level -> Position -> Bool
hasSquare (CLevel lvl) (i, j)
    | getNrObj (CLevel lvl) i j == 2 = True
    | getNrObj (CLevel lvl) i j == 1 && (length $ show $ head $ getObj (CLevel lvl) i j) == 2 = True
    | otherwise = False

hasCircle :: Level -> Position -> Bool
hasCircle (CLevel lvl) (i, j)
    | getNrObj (CLevel lvl) i j > 0 && (show $ head $ getObj (CLevel lvl) i j) `elem` ["r", "b", "g"] = True
    | otherwise = False

hasArrow :: Level -> Position -> Bool
hasArrow (CLevel lvl) (i, j)
    | getNrObj (CLevel lvl) i j > 0 && (show $ head $ getObj (CLevel lvl) i j) `elem` ["^", "v", ">", "<"] = True
    | otherwise = False

getColor :: Object -> Color
getColor (Square c _) = c
getColor (Circle c) = c
getColor _ = undefined

getHeading :: Object -> Heading
getHeading (Square _ h) = h
getHeading (Arrow h) = h
getHeading _ = undefined

getPosition :: Position -> Heading -> Position
getPosition (i, j) North = (i - 1, j)
getPosition (i, j) South = (i + 1, j)
getPosition (i, j) East = (i, j + 1)
getPosition (i, j) West = (i, j - 1)

--pentru un patrat (dintr-un nivel, avand o anumita culoare si pozitie), determinam distanta pana la pozitia finala a acestuia in cadrul jocului, adica pana la cercul de aceeasi culoare
getCircleDist :: Level -> Color -> Position -> Int
getCircleDist (CLevel lvl) c (a, b) =
    foldr (\i acc1 ->
        foldr (\j acc2 ->
                  if hasCircle (CLevel lvl) (i, j) &&
                      (getColor $ head $ getObj (CLevel lvl) i j) == c
                  then abs (a - i) + abs (b - j) + acc2
                  else acc2)
        0
        [minc .. maxc] + acc1)
    0
    [minr .. maxr]
    where minr = minRow (CLevel lvl)
          minc = minCol (CLevel lvl)
          maxr = maxRow (CLevel lvl)
          maxc = maxCol (CLevel lvl)

{-
    *** TODO ***

    Instanțiați clasa `ProblemState` pentru jocul nostru.
-}
instance ProblemState Level Position where
    successors (CLevel lvl) = foldr (\i acc1 ->
                                        foldr (\j acc2 ->
                                                  if hasSquare (CLevel lvl) (i, j)
                                                  then [((i, j), move (i, j) (CLevel lvl))] ++ acc2
                                                  else acc2)
                                              []
                                              [minc .. maxc] ++ acc1)
                                    []
                                    [minr .. maxr]
                              where minr = minRow (CLevel lvl)
                                    minc = minCol (CLevel lvl)
                                    maxr = maxRow (CLevel lvl)
                                    maxc = maxCol (CLevel lvl)

    isGoal (CLevel lvl) = foldr (\i acc1 -> 
                                    foldr (\j acc2 ->
                                              if hasSquare (CLevel lvl) (i, j) then
                                                  if hasCircle (CLevel lvl) (i, j)
                                                      then True && acc2
                                                      else False
                                              else acc2)
                                          True
                                          [minc .. maxc] && acc1)
                                True
                                [minr .. maxr]
                          where minr = minRow (CLevel lvl)
                                minc = minCol (CLevel lvl)
                                maxr = maxRow (CLevel lvl)
                                maxc = maxCol (CLevel lvl)

    -- Doar petru BONUS
    heuristic (CLevel lvl) = foldr (\i acc1 ->
                                       foldr (\j acc2 ->
                                                 if hasSquare (CLevel lvl) (i, j)
                                                 then getCircleDist (CLevel lvl) (getColor $ getSquare $ getObj (CLevel lvl) i j) (i, j) + acc2
                                                 else acc2)
                                             0
                                             [minc .. maxc] + acc1)
                                   0
                                   [minr .. maxr]
                             where minr = minRow (CLevel lvl)
                                   minc = minCol (CLevel lvl)
                                   maxr = maxRow (CLevel lvl)
                                   maxc = maxCol (CLevel lvl)
