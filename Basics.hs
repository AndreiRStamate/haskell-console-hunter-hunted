{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE EmptyDataDecls, MultiParamTypeClasses,
             TypeSynonymInstances, FlexibleInstances,
             InstanceSigs #-}

module Basics where
{-
    Expune funcțiile necesare reprezentării jocului.
-}

import ProblemState
import Data.List

{-
    Sinonim tip de date pentru reprezetarea unei perechi (Int, Int)
    care va reține coordonatele celulelor de pe tabla de joc.
    Colțul stânga-sus este (0, 0).
-}
type Position = (Int, Int)

{-
    Tip de date pentru reprezentarea Target-urilor.
    Acestea conțin informații atât despre poziția curentă a
    Target-ului cât și despre comportamentul acestuia.
    Tipul Behavior este definit mai jos.
-}
data Target = Target {
    position :: Position,
    behavior :: Behavior
}

instance Eq Target where
    Target p1 _ == Target p2 _ = p1 == p2

instance Ord Target where
    Target p1 _ <= Target p2 _ = p1 <= p2

{-
    Tip de date pentru reprezentarea comportamentului unui Target.
    Tipul Behavior este utilizat pentru a modela tranziția Target-urilor
    din starea curentă în starea următoare. Primul parametru este poziția
    actuală a target-ului, iar al doilea, starea curentă a jocului.
    Tipul Game este definit mai jos.
    
    Observați că, din moment ce un Behavior produce un Target nou,
    acesta din urmă ar putea fi caracterizat de un alt Behavior
    decât cel anterior.
-}
type Behavior = Position -> Game -> Target

{-
    Direcțiile de deplasare pe tablă
-}
data Direction = North | South | West | East
    deriving (Eq, Show)

{-
    *** TODO ***
    
    Tip de date pentru reprezentarea stării jocului, la un anumit
    moment. Completați-l cu orice informație aveți nevoie pentru
    stocarea stării jocului (hunter, target, obstacole, gateways).
-}
data Game = Game {
    nLines :: Int,
    mRows :: Int,
    hunter :: Position,
    target :: [Target],
    obstacole :: [Position],
    gateways :: [(Position, Position)] -- ([1,1], [2,2]), ([3,3], [4,4])
}  deriving (Eq, Ord)

{-
    *** Optional *** 
  
    Dacă aveți nevoie de o funcționalitate particulară,
    instantiați explicit clasele Eq și Ord pentru Game.
    În cazul acesta, eliminați deriving (Eq, Ord) din Game.
-}

{-
    *** TODO ***

    Reprezentați starea jocului ca șir de caractere, pentru afișarea
    la consolă.
    
    Atenție! Fiecare linie, mai puțin ultima, este urmată de \n.
    Celule goale vor fi reprezentate ca ' '.
    Hunter-ul va fi reprezentat ca '!'.
    Target-urile vor fi reprezentate ca '*'
    Gateways-urile vor fi reprezentate ca '#'.
    Obstacolele vor fi reprezentate de '@'.

    Hint: S-ar putea să vă fie utile list comprehensions,
    precum și funcțiile elem, any și intercalate din Data.List.
-}

printCharacter :: Game -> Position -> Char
printCharacter game pos
    | (snd pos) == (mRows game) = '\n' 
    | hunter game == pos = '!'
    | pos `elem` pozitiiTarget = '*'
    | (pos `elem` pozitiiIntrareGateway) || (pos `elem` pozitiiIesireGateway)  = '#'
    | pos `elem` (obstacole game) = '@'
    | otherwise = ' '
    where
        pozitiiTarget = map position (target game)
        pozitiiIntrareGateway = map fst (gateways game)
        pozitiiIesireGateway = map snd (gateways game)

gameAsString :: Game -> String
gameAsString game = [printCharacter game (x,y) | x <- [0..(nLines game)-1], y <- [0..(mRows game)], x /= (nLines game)-1 || y /= (mRows game)]

instance Show Game where
    show = gameAsString

{-
    *** TODO ***
    
    Primește numărul de linii și numărul de coloane ale tablei de joc.
    Intoarce un obiect de tip Game în care tabla conține spații goale în interior, fiind
    împrejmuită de obstacole pe toate laturile. Implicit, colțul din stânga sus este (0,0),
    iar Hunterul se găsește pe poziția (1, 1).
-}
emptyGame :: Int -> Int -> Game
emptyGame n m = Game n m (1,1) [] pozitiiObstacole []
    where
        pozitiiObstacoleSus = [(0, y) | y <- [0..m-1]]
        pozitiiObstacoleJos = [(n-1, y) | y <- [0..m-1]]
        pozitiiObstacoleStanga = [(x, 0) | x <- [0..n-1]]
        pozitiiObstacoleDreapta = [(x, m-1) | x <- [0..n-1]]
        pozitiiObstacole = 
            pozitiiObstacoleSus ++ 
            pozitiiObstacoleJos ++ 
            pozitiiObstacoleStanga ++ 
            pozitiiObstacoleDreapta

{-
    *** TODO ***

    Primește o poziție și un joc și întoarce un nou joc, cu Hunter-ul pus
    pe poziția specificată.
    Parametrul Position reprezintă poziția de pe hartă la care va fi adaugat Hunter-ul
    Daca poziția este invalidă (ocupată sau în afara tablei de joc) se va întoarce
    același joc.
-}
addHunter :: Position -> Game -> Game
addHunter pos game = Game (nLines game) (mRows game) (fst posChecked, snd posChecked) (target game) (obstacole game) (gateways game)
    where
        x = (fst pos)
        y = (snd pos)
        posChecked = if (x > 0 && x < (nLines game)-1 && y > 0 && y < (mRows game) - 1 && printCharacter game pos == ' ') then pos else (hunter game)

{-
    *** TODO ***

    Primește un comportament, o poziție și un joc și întoarce un nou joc, în care a fost
    adăugat Target-ul descris de comportament și poziție.
    Parametrul Behavior reprezintă comportamentul Hunter-ului care va fi adăugat.
    Parametrul Position reprezintă poziția de pe hartă la care va fi adăugat Target-ul.
-}
addTarget :: Behavior -> Position -> Game -> Game
addTarget beh pos game = Game (nLines game) (mRows game) (hunter game) ((target game) ++ [newTarget]) (obstacole game) (gateways game)
    where
        newTarget = Target pos beh

{-
    *** TODO ***

    Primește o pereche de poziții și un joc și întoarce un nou joc, în care au fost adăugate
    cele două gateway-uri interconectate.
    Parametrul (Position, Position) reprezintă pozițiile de pe hartă la care vor fi adăugate 
    cele două gateway-uri interconectate printr-un canal bidirecțional.
-}
addGateway :: (Position, Position) -> Game -> Game
addGateway pos game = Game (nLines game) (mRows game) (hunter game) (target game) (obstacole game) ((gateways game) ++ [pos])

{-
    *** TODO ***

    Primește o poziție și un joc și întoarce un nou joc, în care a fost adăugat un obstacol
    la poziția specificată.
    Parametrul Position reprezintă poziția de pe hartă la care va fi adăugat obstacolul.
-}
addObstacle :: Position -> Game -> Game
addObstacle pos game = Game (nLines game) (mRows game) (hunter game) (target game) ((obstacole game) ++ [pos]) (gateways game)

{-
    *** TODO ***
    
    Primește o poziție destinație înspre care vrea să se deplaseze o entitate (Hunter sau Target)
    și verifică daca deplasarea este posibilă, întorcând noua poziție, luând în considerare
    și Gateway-urile.
    Avem următoarele cazuri:
    - dacă poziția corespunde unui spațiu gol, se întoarce acea poziție;
    - dacă poziția corespunde unui gateway, se întoarce poziția gateway-ului pereche;
    - dacă poziția corespunde unui obstacol, se întoarce Nothing.
    Parametrul Position reprezintă poziția destinație.
-}

getExitGateway :: [(Position, Position)] -> Position -> Position
getExitGateway [] _ = (0,0)
getExitGateway gs pos = 
    if ( (fst (head gs)) == pos ) then (snd (head gs)) 
    else if ( (snd (head gs)) == pos ) then (fst (head gs))
    else getExitGateway (tail gs) pos

attemptMove :: Position -> Game -> Maybe Position
attemptMove pos game
    | printCharacter game pos == ' ' = Just pos
    | printCharacter game pos == '#' = Just (getExitGateway (gateways game) pos)
    | printCharacter game pos == '@' = Nothing
    | otherwise = Nothing
            

{-
    *** TODO ***

    Comportamentul unui Target de a se deplasa cu o casuță înspre est. 
    Miscarea se poate face doar daca poziția este validă (se află pe tabla de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul va rămâne 
    pe loc.
    
    Conform definiției, tipul Behavior corespunde tipului funcție
    Position -> Game -> Target.
    
    Având în vedere că cele patru funcții definite în continuare (goEast, goWest,
    goNorth, goSouth) sunt foarte similare, încercați să implementați o funcție
    mai generală, pe baza căreia să le definiți apoi pe acestea patru.
-}

valid :: Position -> Game -> (Bool, Bool)
valid pos game = 
    if ((fst pos) > 0 && (fst pos) < (nLines game)-1 && (snd pos) > 0 && (snd pos) < (mRows game) - 1 && printCharacter game pos /= '@' && printCharacter game pos /= '#') then (True, True)
    else if ((fst pos) > 0 && (fst pos) < (nLines game)-1 && (snd pos) > 0 && (snd pos) < (mRows game) - 1 && printCharacter game pos /= '@' && printCharacter game pos == '#') then (True, False)
    else (False, False)

goDirection :: Position -> Position -> Game -> Target
goDirection posInc initialPos game = Target posChecked (behavior (head [el | el <- (target game), initialPos == (position el)]))
    where
        x = (fst posInc)
        y = (snd posInc)
        newPos = ((fst initialPos) + x, (snd initialPos) + y)
        posChecked = 
            if (valid newPos game == (True, True)) then newPos 
            else if (valid newPos game == (True, False)) then getExitGateway (gateways game) newPos
            else initialPos

goEast :: Behavior
goEast = goDirection(0,1)

{-
    *** TODO ***

    Comportamentul unui Target de a se deplasa cu o casuță înspre vest. 
    Miscarea se poate face doar daca poziția este validă (se află pe tabla de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul va rămâne 
    pe loc.
-}
goWest :: Behavior
goWest = goDirection(0,-1)

{-
    *** TODO ***

    Comportamentul unui Target de a se deplasa cu o casuță înspre nord. 
    Miscarea se poate face doar daca poziția este validă (se află pe tabla de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul va rămâne 
    pe loc.
-}
goNorth :: Behavior
goNorth = goDirection(-1,0)

{-
    *** TODO ***

    Comportamentul unui Target de a se deplasa cu o casuță înspre sud. 
    Miscarea se poate face doar daca poziția este validă (se află pe tabla de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul va rămâne 
    pe loc.
-}
goSouth :: Behavior
goSouth = goDirection(1,0)

{-
    *** TODO ***

    Comportamentul unui Target de a-și oscila mișcarea, când înspre nord, când înspre sud. 
    Mișcarea se poate face doar dacă poziția este validă (se află pe tablă de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul iși va schimba
    direcția de mers astfel:
    - daca mergea inspre nord, își va modifica direcția miscării înspre sud;
    - daca mergea inspre sud, își va continua mișcarea înspre nord.
    Daca Target-ul întâlneste un Gateway pe traseul său, va trece prin acesta,
    către Gateway-ul pereche conectat și își va continua mișcarea în același sens la ieșire
    din acesta.
    Puteți folosit parametrul Int pentru a surprinde deplasamentul Target-ului (de exemplu,
    1 pentru sud, -1 pentru nord).
-}

goDirectionChecked :: Position -> Position -> Game -> Target
goDirectionChecked posInc initialPos game = Target posChecked (behavior (head [el | el <- (target game), initialPos == (position el)]))
    where
        x = (fst posInc)
        y = (snd posInc)
        newPos = ((fst initialPos) + x, (snd initialPos) + y)
        newPosInv = ((fst initialPos) - x, (snd initialPos) - y)
        posChecked = 
            if (valid newPos game == (True, True)) then newPos 
            else if (valid newPos game == (True, False)) then getExitGateway (gateways game) newPos
            else newPosInv

bounce :: Int -> Behavior
bounce dir 
    | dir == 1 = goDirectionChecked (1,0)
    | otherwise = goDirectionChecked (-1,0)

{-
    *** TODO ***
    Funcție care mută toate Target-urile din Game-ul dat o poziție, în functie
    de behavior-ul fiecăreia și întoarce noul Game în care pozițiile Target-urilor
    sunt actualizate.
    
-}

newTargets :: [Target] -> [Target] -> Game -> [Target]
newTargets tgets modifiedTgets game
    | null tgets = modifiedTgets
    | otherwise = newTargets (tail tgets) ([(behavior (head tgets)) (position (head tgets)) game] ++ modifiedTgets) game

moveTargets :: Game -> Game
moveTargets game = Game (nLines game) (mRows game) (hunter game) (newTargets (target game) [] game) (obstacole game) (gateways game)

{-
    *** TODO ***

    Verifică dacă Targetul va fi eliminat de Hunter.
    Un Target este eliminat de Hunter daca se află pe o poziție adiacentă
    cu acesta.
    Parametrul Position reprezintă poziția Hunterului pe tabla
    de joc.
    Parametrul Target reprezintă Targetul pentru care se face verificarea.
-}
isTargetKilled :: Position -> Target -> Bool
isTargetKilled pos tar
    | (fst pos) == (fst (position tar)) + 1 && (snd pos) == (snd (position tar)) = True
    | (fst pos) == (fst (position tar)) - 1 && (snd pos) == (snd (position tar)) = True
    | (fst pos) == (fst (position tar)) && ((snd pos) + 1) == (snd (position tar)) = True
    | (fst pos) == (fst (position tar)) && ((snd pos) - 1) == (snd (position tar)) = True
    | otherwise = False


{-
    *** TODO ***

    Avansează starea jocului curent, rezultând starea următoare a jocului.
    Parametrul Direction reprezintă direcția în care se va deplasa Hunter-ul.
    Parametrul Bool specifică dacă, după mutarea Hunter-ului, vor fi
    mutate și Target-urile sau nu, și dacă vor fi eliminate din joc sau nu.
    Este folosit pentru a distinge între desfășurarea reală a jocului (True)
    și planificarea „imaginată” de hunter (False) în partea a doua a temei.

    Avansarea stării jocului respectă următoarea ordine:
    1. Se deplasează Hunter-ul.
    2. În funcție de parametrul Bool, se elimină Target-urile omorâte de către Hunter.
    3. In funcție de parametrul Bool, se deplasează Target-urile rămase pe tablă.
    4. Se elimină Targeturile omorâte de către Hunter și după deplasarea acestora.
    
    Dubla verificare a anihilării Target-urilor, în pașii 2 și 4, îi oferă Hunter-ului
    un avantaj în prinderea lor.
-}
advanceGameState :: Direction -> Bool -> Game -> Game
advanceGameState = undefined

{-
    ***  TODO ***

    Verifică dacă mai există Target-uri pe table de joc.
-}
areTargetsLeft :: Game -> Bool
areTargetsLeft game = null (target game)

{-
    *** BONUS TODO ***

    Comportamentul unui Target de a se deplasa în cerc, în jurul unui Position, având
    o rază fixată.
    Primul parametru, Position, reprezintă centrul cercului.
    Parametrul Int reprezintă raza cercului.
    Puteți testa utilizând terenul circle.txt din directorul terrains, în conjuncție
    cu funcția interactive.
-}
circle :: Position -> Int -> Behavior
circle = undefined

isGoalAux :: Position -> [Target] -> [Bool] -> [Bool]
isGoalAux hunterPos tgets results
    | null tgets = results
    | isTargetKilled hunterPos (head tgets) = isGoalAux hunterPos (tail tgets) ([True] ++ results)
    | otherwise = isGoalAux hunterPos (tail tgets) results

instance ProblemState Game Direction where
    {-
        *** TODO ***
        
        Generează succesorii stării curente a jocului.
        Utilizați advanceGameState, cu parametrul Bool ales corespunzător.
    -}
    successors = undefined

    {-
        *** TODO ***
        
        Verifică dacă starea curentă este un în care Hunter-ul poate anihila
        un Target. Puteți alege Target-ul cum doriți, în prezența mai multora.
    -}

    isGoal game = (not (null (isGoalAux (hunter game) (target game) [])))

    {-
        *** TODO ***
        
        Euristica euclidiană (vezi hEuclidian mai jos) până la Target-ul ales
        de isGoal.
    -}
    h = undefined

{-
     ** NU MODIFICATI **
-}
hEuclidean :: Position -> Position -> Float
hEuclidean (x1, y1) (x2, y2) = fromIntegral $ (x1 - x2) ^ pow + (y1 - y2) ^ pow
  where
    pow = 2 :: Int

{-
    *** BONUS ***

    Acesta reprezintă un artificiu necesar pentru testarea bonusului,
    deoarece nu pot exista două instanțe diferite ale aceleiași clase
    pentru același tip.

    OBSERVAȚIE: Testarea bonusului pentru Seach este făcută separat.
-}

newtype BonusGame = BonusGame Game
    deriving (Eq, Ord, Show)

{-
    *** BONUS TODO ***

    Folosind wrapper-ul peste tipul Game de mai sus instanțiați
    ProblemState astfel încât să fie folosită noua euristică. 
-}
instance ProblemState BonusGame Direction where
    {-
        *** BONUS TODO ***

        Pentru a ne asigura că toțî succesorii unei stări sunt de tipul
        BonusGame și folosesc noua euristică trebuie să aplicăm wrapper-ul
        definit mai sus peste toți succesorii unei stări.

        Hint: Puteți să folosiți funcția fmap pe perechi pentru acest lucru.
        https://wiki.haskell.org/Functor
    -}
    successors = undefined

    {-
        *** BONUS TODO ***

        Definiți funcția isGoal pentru BonusGame.

        Hint: Folosiți funcția isGoal deja implementată pentru tipul Game.
    -}
    isGoal = undefined

    {-
        *** BONUS TODO ***

        Definiți o funcție euristică care este capabilă să găsească un drum mai scurt
        comparativ cu cel găsit de euristica implementată pentru Game.

        ATENȚIE: Noua euristică NU trebuie să fie una trivială.

        OBSERVAȚIE: Pentru testare se va folosi fișierul terrains/game-6.txt.
    -}
    h = undefined
