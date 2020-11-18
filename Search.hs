{-# OPTIONS_GHC -Wall #-}

module Search where

import ProblemState

import qualified Data.Set as S

import qualified Data.List as DL

{-
    *** TODO ***

    Tipul unei nod utilizat în procesul de căutare. Recomandăm reținerea unor
    informații legate de:

    * stare;
    * acțiunea care a condus la această stare;
    * nodul părinte, prin explorarea căruia a fost obținut nodul curent;
    * adâncime.
-}
--data Node s a = UndefinedNode
data Node s a = Nil s | CNode s a (Node s a) Int
    deriving (Eq, Show, Ord)

{-
    *** TODO ***

    Întoarce starea stocată într-un nod.
-}
nodeState :: Node s a -> s
nodeState (Nil s) = s
nodeState (CNode s _ _ _) = s

nodeParent :: Node s a -> Node s a
nodeParent (Nil _) = undefined
nodeParent (CNode _ _ par _) = par

nodeDepth :: Node s a -> Int
nodeDepth (Nil _) = 0
nodeDepth (CNode _ _ _ h) = h

--determinam copii unui nod dat
getChildNodes :: (ProblemState s a, Ord s) => (Node s a) -> [Node s a]
getChildNodes (Nil s) =
    foldr (\p l -> (CNode (snd p) (fst p) (Nil s) 1) : l) [] (successors s)
getChildNodes (CNode s a parent h) =
    foldr (\p l -> (CNode (snd p) (fst p) (CNode s a parent h) (h + 1)) : l) [] (successors s)

{-
    *** TODO ***

    Întoarce lista nodurilor rezultate prin parcurgerea limitată în adâncime
    a spațiului stărilor, pornind de la starea dată ca parametru.

    Pentru reținerea stărilor vizitate, recomandăm Data.Set. Constrângerea
    `Ord s` permite utilizarea tipului `Set`.

    În afara BONUS-ului, puteți ignora parametrul boolean. Pentru BONUS, puteți
    sorta lista succesorilor folosind `sortBy` din Data.List.
-}
limitedDfs :: (ProblemState s a, Ord s)
           => s           -- Starea inițială
           -> Bool        -- Pentru BONUS, `True` dacă utilizăm euristica
           -> Int         -- Adâncimea maximă de explorare
           -> [Node s a]  -- Lista de noduri

limitedDfs s ok depth = iter [Nil s] ok S.empty [] depth

--primim o stiva, variabila booleana (pentru bonus), un set de stari, o lista de noduri (asociate starilor) si adancimea
iter :: (ProblemState s a, Ord s) => [Node s a] -> Bool -> S.Set s -> [Node s a] -> Int -> [Node s a]
iter [] _ _ visited_l _ = visited_l
iter (node : rest) ok visited_s visited_l depth
    | S.member (nodeState node) visited_s = iter rest ok visited_s visited_l depth
    | otherwise = iter lst ok new_s new_l depth
    where
        lst = if ((nodeDepth node) < depth) then
                  if ok == True then
                      (DL.sortBy my_compare [(CNode s0 a0 node ((nodeDepth node) + 1)) | (a0, s0) <- successors (nodeState node)]) ++ rest
                      else [(CNode s0 a0 node ((nodeDepth node) + 1)) | (a0, s0) <- successors (nodeState node)] ++ rest
                  else rest
        new_s = S.insert (nodeState node) visited_s
        new_l = visited_l ++ [node]

my_compare :: (ProblemState s a, Ord s) => (Node s a) -> (Node s a) -> Ordering
my_compare n1 n2
    | heuristic (nodeState n1) < heuristic (nodeState n2) = LT
    | heuristic (nodeState n1) > heuristic (nodeState n2) = GT
    | otherwise = EQ

{-
    *** TODO ***

    Explorează în adâncime spațiul stărilor, utilizând adâncire iterativă,
    pentru determinarea primei stări finale întâlnite.

    Întoarce o perche între nodul cu prima stare finală întâlnită și numărul
    de stări nefinale vizitate până în acel moment.

    În afara BONUS-ului, puteți ignora parametrul boolean.
-}
iterativeDeepening :: (ProblemState s a, Ord s)
    => s                -- Starea inițială
    -> Bool             -- Pentru BONUS, `True` dacă utilizăm euristica
    -> (Node s a, Int)  -- (Nod cu prima stare finală,
                        --  număr de stări nefinale vizitate)
iterativeDeepening s ok
    | isGoal s = (Nil s, 0)
    | otherwise = search (Nil s) ok 1

--
search :: (ProblemState s a, Ord s) => Node s a -> Bool -> Int -> (Node s a, Int)
search start ok depth
    | (snd $ fst $ (find start ok depth)) > 0 = fst $ (find start ok depth)
    | otherwise = search start ok (depth + 1)

--pentru un nod si o adancime, cautam prin toti succesorii pana la acea adancime o stare finala
--fiecare stare nefinala va fi cu atat mai vizitata cu cat are adancimea mai mica
--pentru calcularea starilor nefinale vizitate, vom aduna depth + 1 - node.depth pentru fiecare stare parcursa (mai putin cea finala)
find :: (ProblemState s a, Ord s) => Node s a -> Bool -> Int -> ((Node s a, Int), Int)
find start ok depth = foldl (\(p, idx) node -> if (isGoal $ nodeState node) && (snd p == 0)
                                               then ((node, idx), idx + depth + 1 - (nodeDepth node))
                                               else (p, idx + depth + 1 - (nodeDepth node)))
                            ((start, 0), 0)
                            (limitedDfs (nodeState start) ok depth)

{-
    *** TODO ***

    Pornind de la un nod, reface calea către nodul inițial, urmând legăturile
    către părinți.

    Întoarce o listă de perechi (acțiune, stare), care se încheie în starea
    finală, dar care EXCLUDE starea inițială.
-}
extractPath :: Node s a -> [(a, s)]
extractPath (Nil _) = []
extractPath (CNode s a parent _) = extractPath parent ++ [(a, s)]

{-
    Poate fi utilizată pentru afișarea fiecărui element al unei liste
    pe o linie separată.
-}
printSpacedList :: Show a => [a] -> IO ()
printSpacedList = mapM_ (\a -> print a >> putStrLn (replicate 20 '*'))
