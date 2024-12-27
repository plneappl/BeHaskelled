
import  Graphics.Gloss hiding (Circle, Blank)
import  Graphics.Gloss.Interface.IO.Game hiding (Circle, Blank)
import  System.Exit
import  Data.Maybe
import  Control.Monad
import  System.Random
import  Control.Arrow
import  qualified Data.Set as DS

data Shape = Triangle | Square | Octa | Pent | Diamond | Hexa | Blank deriving (Show, Eq, Enum, Bounded)
maxShapeIndex :: Int
maxShapeIndex = fromEnum (maxBound :: Shape) 

data Block = Block {
    shape :: Shape
  , selected :: Bool
} 
instance Show Block where
  show b | selected b = "!" ++ show (shape b) ++ "!"
         | otherwise  = show $ shape b
instance Eq Block where
  (==) b1 b2 = shape b1 == shape b2

defBlock :: Shape -> Block
defBlock s = Block { shape = s, selected = False }

type Field = [[Block]]


screenWidth :: Int
screenHeight :: Int
screenWidth = 1366
screenHeight = 768

fps :: Int
fps = 60

fieldGLSideSize :: Float
fieldGLSideSize = let
  sx = fromIntegral screenWidth / fromIntegral xsize
  sy = fromIntegral screenHeight / fromIntegral ysize in
  min sx sy 

upscale :: Picture -> Picture
upscale = 
  Translate (fromRational (- fromIntegral screenWidth / 2)) (fromRational (- fromIntegral screenHeight / 2)) . Scale fieldGLSideSize fieldGLSideSize


pointToCoords :: Point -> (Int, Int)
pointToCoords (posx, posy) = 
  ( head $ mapMaybe (\i -> let posxi = fromIntegral i * fieldGLSideSize - fromIntegral screenWidth  / 2 in if posxi >= posx then Just (i - 1) else Nothing) ([0..]::[Int])
  , head $ mapMaybe (\i -> let posyi = fromIntegral i * fieldGLSideSize - fromIntegral screenHeight / 2 in if posyi >= posy then Just (ysize - i) else Nothing) ([0..]::[Int]))

drawField :: Field -> Picture
drawField fld = combineRows $ reverse $ map drawRow fld

drawRow :: [Block] -> Picture
drawRow blocks = combineBlocks $ map drawBlock blocks

drawBlock :: Block -> Picture
drawBlock b = (if selected b then 
  (\b1 -> Pictures [
    color outlineColor $ Translate 0.5 0.5 $ rectangleSolid 1 1
    --color red $ line [(0, 0), (1, 0), (1, 1), (0, 1), (0, 0)]
    --, Translate 0.01 0.01 $ Scale 0.98 0.98 $ color white $ drawShapeHelper Square
    , b1] ) 
  else
    id) $ drawShape $ shape b

drawShape :: Shape -> Picture
drawShape s = scaleGemDown $ drawShapeHelper s

scaleGemDown :: Picture -> Picture
scaleGemDown = Translate 0.02 0.02 . Scale 0.96 0.96

bgColor :: Color 
bgColor = white

drawShapeHelper :: Shape -> Picture
drawShapeHelper Blank    = color bgColor   $ Polygon                     [(0, 0), (1, 0), (1, 1), (0, 1)]
--drawShapeHelper Triangle = Scale 1 (1/1.4) $ makeGem (dark rose)         [(0, 0), (0.5, 1.4), (1, 0)]
drawShapeHelper Triangle = Scale 1 (1/1.4) $ makeGem (dark rose) [(0.1, 0.17), (0.5, 0), (0.9, 0.17), (0.5, 1.4)]
drawShapeHelper Square   = scaleGemDown    $ makeGem myYellow            [(0, 0), (1, 0), (1, 1), (0, 1)]
drawShapeHelper Octa     =                   makeGem (dim azure)         [(0.25, 0), (0.75, 0), (1, 0.25), (1, 0.75), (0.75, 1), (0.25, 1), (0, 0.75), (0, 0.25)]
drawShapeHelper Pent     = Scale 1 (1/1.1) $ makeGem (dark orange)       [(0.15, 0), (0.85, 0), (1, 0.6), (0.5, 1.1), (0, 0.6)]
drawShapeHelper Diamond  =                   makeGem (dark $ dark white) [(0.5, 0), (0.83, 0.5), (0.5, 1), (0.17, 0.5)]
drawShapeHelper Hexa     =                   makeGem (dark chartreuse)   [(0, 0.5), (0.26, 0.95), (0.74, 0.95), (1, 0.5), (0.74, 0.05), (0.26, 0.05)]

myYellow :: Color
myYellow = makeColor 0.88 0.75 0.15 1

outlineColor :: Color
outlineColor = makeColor 0 0 0 0.35


makeGem :: Color -> [(Float, Float)] -> Picture
makeGem col pts = let 
  mainP = Polygon pts
  ptsClosed = pts ++ [head pts]
  innerPoints = map (\ (x, y) -> ((x + 0.5) / 2, (y + 0.5) / 2)) ptsClosed
  upwardsLinesC = zipWith (\x y -> [x, y]) ptsClosed innerPoints
  upwardsLines = map (color outlineColor . line) upwardsLinesC in
  Pictures $ [
  color col mainP,
  color (dim col) $ Scale 0.5 0.5 $ Translate 0.5 0.5 mainP,
  color outlineColor $ line ptsClosed,
  color outlineColor $ line innerPoints] ++ upwardsLines

combineBlocks :: [Picture] -> Picture
combineBlocks pics = Pictures $ map (\(i, p) -> Translate i 0 p) $ zip [0..] pics


combineRows :: [Picture] -> Picture
combineRows pics = Pictures $ zipWith (Translate 0) [0..] pics 

unselectAll :: Field -> Field
unselectAll = map (map (\b -> b { selected = False }))

unselectAllWorld :: World -> World
unselectAllWorld w = w { field = unselectAll $ field w }

maybeGet :: Int -> [a] -> Maybe a
maybeGet i l | length l <= i = Nothing
             | otherwise        = Just $ l !! i

getBlockAt :: Int -> Int -> World -> Maybe Block
getBlockAt x y w = let fld = field w in
  maybeGet y fld >>= maybeGet x

getSelectedBlock :: World -> Maybe (Block, Int, Int)
getSelectedBlock w = let fld = field w in
  msum $ concatMap (\(row, yi) -> map (\(b, xi) -> if selected b then Just (b, xi, yi) else Nothing) $ zip row [0..])  $ zip fld [0..]

selectBlockWorld :: Int -> Int -> World -> World
selectBlockWorld x y = modifyFieldEntryWorld (\b -> b {selected = True}) x y . unselectAllWorld

modifyFieldEntryWorld :: (Block -> Block) -> Int -> Int -> World -> World
modifyFieldEntryWorld mf x y w = w { field = modifyFieldEntry mf x y $ field w }

modifyFieldEntry :: (Block -> Block) -> Int -> Int -> Field -> Field
modifyFieldEntry mf x y fld = 
  map (\(row, yi) -> if yi == y then  
    map (\(b, xi) -> if xi == x then  
      mf b else b) $ zip row [0..] 
    else row) $ zip fld [0..]

displayMode :: Display
--displayMode = FullScreen (screenWidth, screenHeight)
displayMode = InWindow "BeHaskelled" (screenWidth, screenHeight) (0, 0)
--displayMode = FullScreen (640, 480)
--displayMode = InWindow "test" (640, 480) (0, 0)

data World = World {
    field :: Field
  , score :: Int
  , lastInputs :: [(Int, Int)]
  , randomBlocks :: [Block]
  , timeElapsed :: Float
  , changed :: Bool
  , combo :: Int
--  , animationQueue :: [Animation]
} 
instance Show World where
  show w = 
    "{\n\t  " ++ show (field w) ++ 
     "\n\t, " ++ show (score w) ++ 
     "\n\t, " ++ show (take 5 $ randomBlocks w) ++ 
     "\n\t, " ++ show (timeElapsed w) ++
     "\n\t, " ++ show (changed w) ++ "}"

defWorld :: Field -> [Block] -> World
defWorld f rbs = World {
    field = f
  , score = 0
  , lastInputs = [(0, 0), (0, 0), (0, 0), (0, 0), (0, 0)]
  , randomBlocks = rbs
  , timeElapsed = 0
  , changed = True
  , combo = 0
} 
--data Animation = ...

updateLastInputs :: (Int, Int) -> World -> World
updateLastInputs p w = w {
  lastInputs = tail $ lastInputs w ++ [p]
}

swap :: (Int, Int) -> (Int, Int) -> World -> World
swap (x1, y1) (x2, y2) w 
  = let 
    fld = field w
    b1 = fld !! y1 !! x1 
    b2 = fld !! y2 !! x2 in
    modifyFieldEntryWorld (const b2) x1 y1 $ modifyFieldEntryWorld (const b1) x2 y2 w

minDelSize :: Int
minDelSize = 4

handleSelect :: Int -> Int -> World -> World
handleSelect x y w | changed w = w
                   | otherwise
  = case getBlockAt x y w of
    Nothing             -> w
    Just b | selected b -> unselectAllWorld w 
           | otherwise  -> case getSelectedBlock w of 
              Just (_, x1, y1) | neighbors (x, y) (x1, y1) -> let
                wSwapped = swap (x, y) (x1, y1) w { timeElapsed = 0, changed = True }
                fld = field wSwapped
                del1 = connectedHelper fld (x, y)
                del2 = connectedHelper fld (x1, y1)
                delete = DS.size del1 >= minDelSize || DS.size del2 >= minDelSize in
                if delete then
                  unselectAllWorld wSwapped 
                else selectBlockWorld x y w
                --del1' = if DS.size del1 >= minDelSize then del1 else DS.empty
                --del2' = if DS.size del2 >= minDelSize then del2 else DS.empty 
                --sdel1 = DS.size del1'
                --sdel2 = DS.size del2' in
                --if sdel1 == 0 && sdel2 == 0 then selectBlockWorld x y w else let
                --deletions = DS.toList $ del1' `DS.union` del2' in
                --unselectAllWorld $ 
                --processDeletions deletions wSwapped   
              _          -> selectBlockWorld x y w

processDeletions :: [(Int, Int)] -> World -> World
processDeletions coords w = let 
  scorePlus = 5 * combo w * length coords * (floor . logBase 2.0 . fromIntegral) (length coords) in 
  foldl delCoord (w { score = scorePlus + score w }) coords

neighbors :: (Int, Int) -> (Int, Int) -> Bool
neighbors (x1, y1) (x2, y2) | abs (x1 - x2) == 1 && y1 == y2 = True
                            | abs (y1 - y2) == 1 && x1 == x2 = True
                            | otherwise                      = False 

delCoord :: World -> (Int, Int) -> World
delCoord w (x, y) = modifyFieldEntryWorld (const $ defBlock Blank) x y w
--delCoord :: World -> (Int, Int) -> World
--delCoord w coord = w { field = newField, randomBlocks = tail $ randomBlocks w } where
--  newBlock = head $ randomBlocks w
--  newField = deletionHelper newBlock coord $ field w

mapField :: (Block -> a) -> Field -> [[a]]
mapField f = map (map f)

foldlField :: (a -> b -> a) -> a -> [[b]] -> a
foldlField f = foldl (foldl f)

getAllDeletions :: Field -> DS.Set (Int, Int)
getAllDeletions fld = foldl (\ set (row, y) -> DS.union set $ getRowDeletions fld row y) DS.empty $ zip fld [0..]

getRowDeletions :: Field -> [Block] -> Int -> DS.Set (Int, Int)
getRowDeletions fld row y = foldl (\set (_, x) -> let set2 = connectedHelper fld (x, y) in DS.union set $ if DS.size set2 < minDelSize then DS.empty else set2) DS.empty $ zip row [0..]

containsBlank :: Field -> Bool
containsBlank = foldlField (\ yn b -> yn || shape b == Blank) False

deletionTrickle :: Block -> (Int, Int) -> Field -> Field
deletionTrickle b (x, y) fld | y == 0   = modifyFieldEntry (const b) x y fld
                            | otherwise = deletionTrickle b (x, y - 1) $ modifyFieldEntry (const bAbove) x y fld where
  bAbove = fld !! (y - 1)!! x

eventHandler :: Event -> World -> IO World
eventHandler (EventKey (MouseButton LeftButton) Down _ (posx, posy)) w 
  = let
    clickedCoords@(ccx, ccy) = pointToCoords (posx, posy) in
    putStrLn (show posx ++ ", " ++ show posy) >> handleSelect ccx ccy . updateLastInputs clickedCoords <$> return w
eventHandler (EventKey (SpecialKey KeyEsc) Down _ _) _ = exitSuccess
eventHandler _ w = return w

initialRandomBlocks :: StdGen -> [Block]
                                                            -- minus one since blanks shouldn't be autogenerated
initialRandomBlocks r = map (defBlock . toEnum) (randomRs (0, maxShapeIndex - 1) r)

takeNTimes :: Int -> [a] -> ([a] -> ([a], [a])) -> ([[a]], [a])
takeNTimes 0 r _  = ([], r)
takeNTimes n r rf = let 
  (r1, b) = rf r 
  (r2, rest) = takeNTimes (n-1) b rf in (r1 : r2, rest)  

connectedHelper :: (Eq a) => [[a]] -> (Int, Int) -> DS.Set (Int, Int)
connectedHelper fld (cx, cy) = connected (fld !! cy !! cx) DS.empty fld (cx, cy)

connected :: (Eq a) => a -> DS.Set (Int, Int) -> [[a]] -> (Int, Int) -> DS.Set (Int, Int)
connected toTest seen fld coord@(cx, cy) 
  | cx < 0 || cy < 0 || DS.member coord seen || cy >= length fld || cx >= length (fld !! cy) = seen
  | otherwise = let 
    b0 = (fld !! cy) !! cx in
    if b0 /= toTest then seen else let
    nSeen = DS.insert (cx, cy) seen in 
    foldl (\ sn coord1 -> connected toTest sn fld coord1) nSeen [(cx-1, cy), (cx+1, cy), (cx, cy-1), (cx, cy+1)]

xsize :: Int
ysize :: Int
xsize = 12
ysize = 12

initialWorld :: StdGen -> World
initialWorld r = let 
  rbs = initialRandomBlocks r
  (fld, rest) = takeNTimes ysize rbs (take xsize &&& drop xsize)
  firstWorld = defWorld fld rest in
  settleWorld firstWorld

settleWorld :: World -> World
settleWorld w = (settleWorldHelper True w) { score = 0, timeElapsed = 0, combo = 0 }

settleWorldHelper :: Bool -> World -> World
settleWorldHelper False w = w
settleWorldHelper True w = let 
  nextWorld = onTickHelper 0.5 w in
  settleWorldHelper (changed nextWorld) nextWorld

drawWorld :: World -> IO Picture
drawWorld w = return $ Pictures [upscale $ drawField $ field w, drawScore w, drawCombo w, drawInstructions] --drawDebug w)

drawScore :: World -> Picture
drawScore w = Translate 130 250 $ Scale 0.5 0.5 $ Text $ "Score: " ++ show (score w)

drawCombo :: World -> Picture
drawCombo w = Translate 130 180 $ Scale 0.5 0.5 $ Text $ "Combo: " ++ show (combo w)

drawInstructions :: Picture
drawInstructions = Pictures [
  Translate 130 120 $ Scale 0.25 0.25 $ Text "Instructions:",  
  Translate 130  90 $ Scale 0.20 0.20 $ Text "Match at least 4 blocks in any",
  Translate 130  60 $ Scale 0.20 0.20 $ Text "connected pattern to score." ]

drawDebug :: World -> [Picture]
drawDebug w = map (\((x, y), i) -> Translate 100 ((120*i) - 200) $ Text $ show x ++ ", " ++ show y) $ zip (lastInputs w) [0..]

onTickHelper :: Float -> World -> World
onTickHelper time w = let time' = time + timeElapsed w in
  if time' < 0.5 then
    w { timeElapsed = time' } 
  else if containsBlank $ field w then
    foldl updateOne w { timeElapsed = 0 } [0..(xsize - 1)]
  else if changed w then let 
    deletions = getAllDeletions $ field w in
    if DS.size deletions == 0 then w { timeElapsed = 0, changed = False, combo = 0 } else
    processDeletions (DS.toList deletions) w { timeElapsed = 0, combo = 1 + combo w }
  else
    w { timeElapsed = time' }

onTick :: Float -> World -> IO World
onTick f w = return $ onTickHelper f w
  
-- finds some empty block in fields column x starting in row y and returns its y-coord
findEmpty :: Int -> Int -> Field -> Maybe Int
findEmpty x y fld | y == ysize = Nothing
                  | shape (fld !! y !! x) == Blank = Just y
                  | otherwise  = findEmpty x (y + 1) fld

updateOne :: World -> Int -> World
updateOne w x = case findEmpty x 0 $ field w of
  Nothing -> w
  Just y  -> w { field = deletionTrickle (head $ randomBlocks w) (x, y) (field w), randomBlocks = tail $ randomBlocks w } 

main :: IO ()
main = do
  r <- getStdGen
  playIO displayMode bgColor fps (initialWorld r) drawWorld eventHandler onTick


