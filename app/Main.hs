-- catmap by catroidvania
-- code sucks i know :// but i will learn to haskell someday!

module Main (main) where


import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact
import System.IO
import System.Environment
import System.Directory
import Data.Char

-- | for tracking what we are doing to tiles
data Mode = None | Showing | Hiding | Freelook | Tinting Color deriving Eq

-- | record containing tile data
data Tile = Tile 
    {
    xLoc, yLoc  :: Float,       -- ^ x and y position on screen in pixels
    tint        :: Color,       -- ^ holds colored tints or opacity
    icons       :: [Picture],   -- ^ icons to be drawn on the Tile
    texture     :: Picture      -- ^ picture representing the texture to be drawn when visible
    }

-- | a list of Tiles and more used as the world for gloss stuff
data State = State 
    {
    tiles               :: [Tile],  -- ^ list of tiles used to be displayed
    iconList            :: [Icons], -- ^ list of icons and thier letter to be drawn by the user
    showFog             :: Bool,    -- ^ whether to show fog over tiles or not
    showGrid            :: Bool,    -- ^ whether to display outlines around Tiles
    setting             :: Mode,    -- ^ action we are preforming
    size                :: Float,   -- ^ scale of the map
    sizeVel             :: Float,   -- ^ speed to scale the map
    xVel, yVel          :: Float,   -- ^ values the map gets moved by every frame
    xPos, yPos          :: Float,   -- ^ offset of the map
    xAnchor, yAnchor    :: Float    -- ^ saveing position in freelook
    }

-- | alias for pictures mapped to thier letter
type Icons = (Char, Picture)

-- | alias for a string holding dungeon info
--   characters corrospond to a .bmp file with the same name as the character for tiles
--   spaces are the background colored tiles
type MapString = String


-- | width and hieght of textures, hardcoded
textureSizeInt :: Int
textureSizeInt = 64

-- | width and hieght of textures as a float
textureSizeFloat :: Float
textureSizeFloat = fromIntegral textureSizeInt

-- | width and hieght of the window, hardcoded
windowWidth, windowHeight :: Int
windowWidth  = 1024
windowHeight = 768


-- | background color, hardcoded
background :: Color
background = light $ light black

-- | Picture for fog drawn when a Tile is not visible
fog :: Picture
fog = Color (light black) $ rectangleSolid textureSizeFloat textureSizeFloat

{- -- unused
-- | map Icons
crossIcon :: Picture
crossIcon = Pictures [Color black crossPic, Color white $ Scale 0.7 0.7 crossPic]
    where crossPic = Pictures [Pictures [Polygon [((-26),(-18)),((-18),(-26)),(26,18),(18,26)], Polygon [(26,(-18)),(18,(-26)),((-26),18),((-18),26)]]]

circleIcon :: Picture
circleIcon = Pictures [Color black circlePic, Color white $ Scale 0.7 0.7 circlePic]
    where circlePic = circleSolid 26

diamondIcon :: Picture
diamondIcon = Pictures [Color black $ diamondPic, Color white $ Scale 0.7 0.7 diamondPic]
    where diamondPic = Polygon [(0,-26),(-26,0),(0,26),(26,0)]

boxIcon :: Picture
boxIcon = Pictures [Color black boxPic, Color white $ Scale 0.7 0.7 boxPic]
    where boxPic = rectangleSolid 52 52

heartIcon :: Picture
heartIcon = Pictures [Color black $ heartPic, Color white $ Scale 0.7 0.7 heartPic]
    where heartPic = Pictures [Translate (-13) 13 $ circleSolid 13, Translate 13 13 $ circleSolid 13, Polygon [(0,(-26)),((-24.5),7),(24.5,7)], Translate 0 10 $ rectangleSolid 13 13]

triangleIcon :: Picture
triangleIcon = Pictures [Color black $ trianglePic, Color white $ Scale 0.7 0.7 trianglePic]
    where trianglePic = Polygon [(0,26),(26,(-13)),((-26),(-13))]
-}

-- | transparent color
transparency :: Color
transparency = makeColor 0 0 0 0

-- | opacity color that gets replaced by fog image
opaque :: Color
opaque = makeColor 0 0 0 1

-- | tint colors
redTint :: Color
redTint = makeColor 1 0 0 0.3           -- ^ red

orangeTint :: Color
orangeTint = makeColor 1 0.4 0.1 0.3    -- ^ orange

yellowTint :: Color
yellowTint = makeColor 1 1 0 0.3        -- ^ yellow

greenTint :: Color
greenTint = makeColor 0 1 0 0.3         -- ^ green

blueTint :: Color
blueTint = makeColor 0 0 1 0.3          -- ^ blue

purpleTint :: Color
purpleTint = makeColor 1 0 1 0.3        -- ^ purple

whiteTint :: Color
whiteTint = makeColor 1 1 1 0.3         -- ^ white

greyTint :: Color
greyTint = makeColor 0.5 0.5 0.5 0.3    -- ^ grey

blackTint :: Color
blackTint = makeColor 0 0 0 0.3         -- ^ black

-- | Tile to represent spaces in a MapString, just shows the background as it is transparent
transparent :: Picture
transparent = Color transparency $ rectangleSolid textureSizeFloat textureSizeFloat


-- | cursor image displayed on the centre so we know which tile we are looking at
cursor :: State -> Picture  -- more shameless copy paste
cursor s@(State {showFog = False})   = Pictures [Translate 10 (-10) $ Scale 0.08 0.08 $ Color (light blue) $ info, Color (light blue) $ Circle 2]
    where
        info = Text $ (show $ size s) ++ " " ++ (show $ xPos s) ++ " " ++ (show $ yPos s) ++ " Fog:" ++ (show $ showFog s) ++ " Grid:" ++ (show $ showGrid s)
cursor (State {setting = Showing})      = Color white $ Circle 5
cursor (State {setting = Hiding})       = Color black $ Circle 5
cursor s@(State {setting = Freelook})   = Pictures [Translate 10 (-10) $ Scale 0.08 0.08 $ Color (light red) $ info, Color (light red) $ Circle 2]
    where
        info = Text $ (show $ size s) ++ " " ++ (show $ xPos s) ++ " " ++ (show $ yPos s) ++ " Fog:" ++ (show $ showFog s) ++ " Grid:" ++ (show $ showGrid s) ++ (show $ iconList s)
cursor (State {setting = Tinting t})    = Color (withAlpha 1 t) $ Circle 5
cursor _ = Color (makeColor 0.5 0.5 0.5 1) $ Pictures [rectangleWire 9.0 0.1, rectangleWire 0.1 9.0]

-- | scales a texture to 256 by 256
normaliseTexture :: Picture -> (Int, Int) -> Picture
normaliseTexture p (w, h) = Scale (textureSizeFloat / (fromIntegral w)) (textureSizeFloat / (fromIntegral h)) p

-- | loads an image based on a given filepath as a 256 by 256 picture
loadImage :: FilePath -> IO Picture
loadImage filename = do
    pic@(Bitmap bmp) <- loadBMP filename
    return $ normaliseTexture pic (bitmapSize bmp)

-- | takes the element out of a list of io elements - this sucks man
unIOify :: [a] -> [IO a] -> IO [a]
unIOify l [] = return l
unIOify l (x:xs) = do
    x' <- x
    unIOify (l ++ [x']) xs

-- | returns a key value thing of icon pictures
initIcons :: State -> IO State
initIcons state = do
    files <- listDirectory "icons"
    let toLoad = map (\s -> head s) $ filter (\s -> head s /= '.') $ files 
    loadedIcons <- unIOify [] $ map (\p -> do loadImage $ "icons/" ++ [p] ++ ".bmp") toLoad
    return $ state {iconList = zipWith (\f p -> (f, p)) toLoad loadedIcons}

-- | creates a State based on a MapString
initTiles :: MapString -> IO State -> Float -> Float -> IO State
initTiles [] state _ _ = state
initTiles ('\n':cs) state x y = initTiles cs state 0.0 (y - textureSizeFloat)
initTiles (' ':cs) state x y = initTiles cs state (x - textureSizeFloat) y
initTiles (c:cs) state x y = do
    img <- loadImage $ "textures/" ++ (c:".bmp")
    unpackedState <- state
    let tile = Tile {
        xLoc    = x,
        yLoc    = y,
        tint    = opaque,
        icons   = [],
        texture = img
        }
        state' = unpackedState {tiles = tile:(tiles unpackedState)}
    initTiles cs (return state') (x + textureSizeFloat) y

-- | swaps the visibility of the tile at a given coordinate
updateMap :: (Float, Float, (Tile -> Color)) -> Tile -> Tile
updateMap (x, y, b) t
    | round (x / textureSizeFloat) == round (xLoc t / textureSizeFloat) &&
      round (y / textureSizeFloat) == round (yLoc t / textureSizeFloat) = t {tint = b t}
    | otherwise = t

-- | switches a tint to a tile
tintMap :: (Float, Float, Color) -> Tile -> Tile
tintMap (x, y, c) t
    | round (x / textureSizeFloat) == round (xLoc t / textureSizeFloat) &&
      round (y / textureSizeFloat) == round (yLoc t / textureSizeFloat) = t {tint = if tint t /= opaque then c else opaque}
    | otherwise = t

-- | adds an icon to a Tile
addIcon :: (Float, Float, Picture) -> Tile -> Tile
addIcon (x, y, i) t
    | round (x / textureSizeFloat) == round (xLoc t / textureSizeFloat) &&
      round (y / textureSizeFloat) == round (yLoc t / textureSizeFloat) = t {icons = (icons t) ++ [i]}
    | otherwise = t

-- | removes the latest icon from a Tile
popIcon :: (Float, Float) -> Tile -> Tile
popIcon (x, y) t
    | round (x / textureSizeFloat) == round (xLoc t / textureSizeFloat) &&
      round (y / textureSizeFloat) == round (yLoc t / textureSizeFloat) = t {icons = if (length $ icons t) > 0 then reverse $ tail $ reverse $ icons t else []}
    | otherwise = t

-- | Gloss display settings
window :: Display
window = InWindow "catmap!" (windowWidth, windowHeight) (20, 20)

-- | creates a State based on the contents of a file
initState :: FilePath -> IO State
initState path = do
    withFile path ReadMode (\h -> do
        room <- hGetContents h
        state' <- initTiles room (return State {
            tiles       = [],
            iconList    = [],
            showFog     = True,
            showGrid    = False,
            setting     = None,
            size        = 1.0,
            sizeVel     = 0.0,
            xVel        = 0.0,
            yVel        = 0.0,
            xPos        = 0.0,
            yPos        = 0.0,
            xAnchor     = 0.0,
            yAnchor     = 0.0
            }) 0 0
        initIcons state')

-- | turns a State into a single Picture
renderState :: State -> Picture
renderState state = Scale ms ms $ Pictures $ [Translate xp yp $ Pictures $ map renderTile $ tiles state, cursor state]
    where
        grid = if showGrid state then Color (white) $ rectangleWire textureSizeFloat textureSizeFloat else transparent
        getTexture t = Translate (xLoc t) (yLoc t) $ Pictures $ [texture t, Pictures $ icons t, Color (tint t) $ rectangleSolid textureSizeFloat textureSizeFloat, grid]
        renderTile t = if showFog state
        then
            if tint t /= opaque
            then getTexture t
            else Translate (xLoc t) (yLoc t) $ Pictures [fog, grid]
        else getTexture (t {tint = transparency})
        ms = size state
        xp = xPos state
        yp = yPos state

-- | step function, does movement and some show/hide tile stuff
step :: Float -> State -> State
step _ s = s {
    xPos = xPos s + xVel s,
    yPos = yPos s + yVel s,
    size = if newSize > 0.01 && setting s == Freelook then newSize else size s,
    tiles = case setting s of
            Showing     -> map (updateMap (0 - xPos s, 0 - yPos s, (\_ -> transparency))) $ tiles s
            Hiding      -> map (updateMap (0 - xPos s, 0 - yPos s, (\_ -> opaque))) $ tiles s
            (Tinting c) -> map (tintMap (0 - xPos s, 0 - yPos s, c)) $ tiles s
            _           -> tiles s
    }
    where newSize = size s + sizeVel s

-- | input handler
inputHandler :: Event -> State -> State
inputHandler (EventKey (MouseButton LeftButton) Down _ (x, y)) s = s {xPos = xPos s - x, yPos = yPos s - y}
-- inputHandler (EventKey (MouseButton RightButton) Down mod (x, y)) s  -- these are replaces by number keys
--    | shift mod == Up     = s {tiles = map (tintMap (x - xPos s, y - yPos s, yellowTint)) $ tiles s}
--    | shift mod == Down   = s {tiles = map (tintMap (x - xPos s, y - yPos s, redTint)) $ tiles s}
inputHandler (EventKey (SpecialKey KeySpace) Down _ (x, y)) s 
    | setting s /= Freelook = s {setting = if length currentTile > 0 && (tint (head currentTile) /= opaque) then Hiding else Showing}
    where
        currentTile = filter (getTile (0 - xPos s, 0 - yPos s)) $ tiles s 
        getTile (x, y) t = (round (x / textureSizeFloat) == round (xLoc t / textureSizeFloat) && round (y / textureSizeFloat) == round (yLoc t / textureSizeFloat))
inputHandler (EventKey (Char c) Down (Modifiers {alt = Down}) _) s
    | key == charKey = s {tiles = map (addIcon (0 - xPos s, 0 - yPos s, iconImg)) $ tiles s}
    where
        key = toLower c
        match = filter (\(f,p) -> c == f) $ iconList s
        (charKey, iconImg) = if length match > 0 then head match else (' ', transparent)
inputHandler (EventKey (Char key) Down mod (x, y)) s 
    | key `elem` ['1', '!'], fr = s {setting = Tinting (cM redTint)}      -- look at this clunky bozo looking code block
    | key `elem` ['2', '@'], fr = s {setting = Tinting (cM orangeTint)}
    | key `elem` ['3', '#'], fr = s {setting = Tinting (cM yellowTint)}
    | key `elem` ['4', '$'], fr = s {setting = Tinting (cM greenTint)}
    | key `elem` ['5', '%'], fr = s {setting = Tinting (cM blueTint)}
    | key `elem` ['6', '^'], fr = s {setting = Tinting (cM purpleTint)}
    | key `elem` ['7', '&'], fr = s {setting = Tinting (cM whiteTint)}
    | key `elem` ['8', '*'], fr = s {setting = Tinting (cM greyTint)}
    | key `elem` ['9', '('], fr = s {setting = Tinting (cM blackTint)}
    | key `elem` ['0', ')'], fr = s {setting = Tinting transparency}
    {-
    | key == 'x', fr            = s {tiles = map (addIcon (0 - xPos s, 0 - yPos s, crossIcon)) $ tiles s}
    | key == 'c', fr            = s {tiles = map (addIcon (0 - xPos s, 0 - yPos s, circleIcon)) $ tiles s}
    | key == 'v', fr            = s {tiles = map (addIcon (0 - xPos s, 0 - yPos s, diamondIcon)) $ tiles s}
    | key == 'b', fr            = s {tiles = map (addIcon (0 - xPos s, 0 - yPos s, boxIcon)) $ tiles s}
    | key == 'm', fr            = s {tiles = map (addIcon (0 - xPos s, 0 - yPos s, heartIcon)) $ tiles s}
    | key == 'n', fr            = s {tiles = map (addIcon (0 - xPos s, 0 - yPos s, triangleIcon)) $ tiles s}
    -}
    where
        fr = (setting s /= Freelook)     
        colorMod (Modifiers {shift = Down}) t = light $ light t
        colorMod (Modifiers {ctrl = Down}) t = dark $ dark t
        colorMod _ t = t
        cM = colorMod mod
inputHandler (EventKey (Char c) Down mod (x, y)) s
    | key == 'w'            = s {yVel = (-spd)}
    | key == 's'            = s {yVel = spd}
    | key == 'a'            = s {xVel = spd}
    | key == 'd'            = s {xVel = (-spd)}
    | key == 'e'            = s {sizeVel = 0.01}
    | key == 'q'            = s {sizeVel = (-0.01)}
    | key == 'f'            = if setting s == Freelook 
                              then s {setting = None, xPos = xAnchor s, yPos = yAnchor s, size = 1.0}
                              else s {setting = Freelook, xAnchor = xPos s, yAnchor = yPos s}
    | key == 't'            = s {showFog = not $ showFog s}
    | key == 'g'            = s {showGrid = not $ showGrid s}
    | key == 'r'            = s {xPos = 0.0, yPos = 0.0}
    where   -- i really out to move some of these out of htis where block
        key = toLower c
        spdMod (Modifiers {shift = Down}) = 12.0
        spdMod _ = 6.0
        spd = spdMod mod
inputHandler (EventKey (SpecialKey key) Down mod _) s
    | key == KeyUp          = s {yVel = (-spd)}
    | key == KeyDown        = s {yVel = spd}
    | key == KeyLeft        = s {xVel = spd}
    | key == KeyRight       = s {xVel = (-spd)}
    | key `elem` [KeyBackspace, KeyDelete]  = s {tiles = map (popIcon (0 - xPos s, 0 - yPos s)) $ tiles s}
    where   -- shameless copy paste
        spdMod (Modifiers {shift = Down}) = 12.0
        spdMod _ = 6.0
        spd = spdMod mod
inputHandler (EventKey (Char c) Up _ _) s 
    | cl == 'a' || cl == 'd'    = s {xVel = 0.0}
    | cl == 'w' || cl == 's'    = s {yVel = 0.0}
    | cl == 'q' || cl == 'e'    = s {sizeVel = 0.0}
    | cl `elem` ['0'..'9'] || cl `elem` ['!', '@', '#', '$', '%', '^', '&', '*', '(', ')']  = s {setting = None}
    where cl = toLower c
inputHandler (EventKey (SpecialKey key) Up _ _) s 
    | key == KeySpace, setting s /= Freelook    = s {setting = None}
    | key `elem` [KeyUp, KeyDown]               = s {yVel = 0.0}
    | key `elem` [KeyLeft, KeyRight]            = s {xVel = 0.0}
inputHandler _ s = s 
run :: FilePath -> IO ()
run path = do
    state <- initState path
    play window background 100 state renderState inputHandler step

-- main
main :: IO ()
main = do
    args <- getArgs
    case args of
        []  -> print "Please provide a file!"
        _   -> run $ head args
