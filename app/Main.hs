-- catmap by catroidvania
-- code sucks i know :// but i will learn to haskell someday!

module Main (main) where


import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact
import System.IO
import System.Environment


-- | record containing tile data
data Tile = Tile 
    {
    xLoc    :: Float,       -- ^ x position on screen in pixels
    yLoc    :: Float,       -- ^ y position on screen in pixels
    visible :: Bool,        -- ^ draws texture if True, fog if False
    texture :: Picture      -- ^ picture representing the texture to be drawn when visible
    }

-- | a list of Tiles and more used as the world for gloss stuff
data State = State 
    {
    tiles   :: [Tile],  -- ^ list of tiles used to be displayed
    showFog :: Bool,    -- ^ whether to show fog over tiles or not
    showing :: Bool,    -- ^ whether or not we are revealing tiles, may change to something like seeing instead
    size    :: Float,   -- ^ scale of the map - unused as it messes with the coord system too much
                        --   and i cannot be botherthed to refactor everything to make it work
    sizeVel :: Float,   -- ^ speed to scale the map - also unused for now
    xVel    :: Float,   -- ^ value the map gets shifted horizontally by every frame
    yVel    :: Float,   -- ^ value the map gets shifted vertically by every frame
    xPos    :: Float,   -- ^ horizontal offset of the map
    yPos    :: Float    -- ^ vertical offset of the map
    }

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
background = light black

-- | Picture for fog drawn when a Tile is not visible
fog :: Picture
fog = Color (light $ light black) $ rectangleSolid textureSizeFloat textureSizeFloat

-- | Tile to represent spaces in a MapString, not transparency, just the background color
transparent :: Picture
transparent = Color background $ rectangleSolid textureSizeFloat textureSizeFloat

-- | cursor image displayed on the centre so we know which tile we are looking at
cursor :: Picture
cursor = Color red $ circle 2


-- | scales a texture to 256 by 256
normaliseTexture :: Picture -> (Int, Int) -> Picture
normaliseTexture p (w, h) = Scale (textureSizeFloat / (fromIntegral w)) (textureSizeFloat / (fromIntegral h)) p

-- | loads an image based on a given filepath as a 256 by 256 picture
loadImage :: FilePath -> IO Picture
loadImage filename = do
    pic@(Bitmap bmp) <- loadBMP filename
    return $ normaliseTexture pic (bitmapSize bmp)

-- | creates a State based on a MapString
initTiles :: MapString -> IO State -> Float -> Float -> IO State
initTiles [] state _ _ = state
initTiles ('\n':cs) state x y = initTiles cs state 0.0 (y - textureSizeFloat)
initTiles (' ':cs) state x y = initTiles cs state (x - textureSizeFloat) y
initTiles (c:cs) state x y = do
    img <- loadImage $ c:".bmp"
    unpackedState <- state
    let tile = Tile {
        xLoc    = x,
        yLoc    = y,
        visible = False,
        texture = img
        }
        state' = unpackedState {tiles = tile:(tiles unpackedState)}
    initTiles cs (return state') (x + textureSizeFloat) y

-- | swaps the visibility of the tile at a given coordinate
updateMap :: (Float, Float) -> Tile -> Tile
updateMap (x, y) t
    | round (x / textureSizeFloat) == round (xLoc t / textureSizeFloat) &&
      round (y / textureSizeFloat) == round (yLoc t / textureSizeFloat) = t {visible = not $ visible t}
    | otherwise = t

-- | Gloss display settings
window :: Display
window = InWindow "catmap!" (windowWidth, windowHeight) (0, 0)

-- | creates a State based on the contents of a file
initState :: FilePath -> IO State
initState path = do
    withFile path ReadMode (\h -> do
        room <- hGetContents h
        initTiles room (return State {
            tiles   = [],
            showFog = True,
            showing = False,
            size    = 1.0,
            sizeVel = 0.0,
            xVel    = 0.0,
            yVel    = 0.0,
            xPos    = 0.0,
            yPos    = 0.0
            }) 0 0)

-- | turns a State into a single Picture
renderState :: State -> Picture
renderState state = Scale ms ms $ Pictures $ [Translate xp yp $ Pictures $ map renderTile $ tiles state] ++ [cursor]
    where
        getTexture t = Translate (xLoc t) (yLoc t) $ texture t
        renderTile t = if showFog state
        then
            if visible t
            then getTexture t
            else Translate (xLoc t) (yLoc t) $ fog
        else getTexture t
        ms = size state
        xp = xPos state
        yp = yPos state

-- | step function, does nothing as of now
step :: Float -> State -> State
step _ s = s {
    tiles = if showing s then map (updateMap (0 - (cs * xPos s), 0 - (cs * yPos s))) $ tiles s else tiles s,
    showing = False,
    xPos = xPos s + xVel s,
    yPos = yPos s + yVel s
    -- size = if (sizeVel s + cs) > 0.05 then cs + sizeVel s else cs
    }
    where cs = size s

-- | input handler
inputHandler :: Event -> State -> State
inputHandler (EventKey key Down mod _) s 
    | key == SpecialKey KeySpace = s {showing = True}
    | key == Char 'w'   = s {yVel = (-spd)}
    | key == Char 's'   = s {yVel = spd}
    | key == Char 'a'   = s {xVel = spd}
    | key == Char 'd'   = s {xVel = (-spd)}
    | key == Char 'r'   = s {xPos = 0.0, yPos = 0.0}
    {- | key == Char 'q'   = s {sizeVel = 0.01}
     | key == Char 'e'   = s {sizeVel = (-0.01)} -}
    | key == Char 'f'   = s {showFog = not $ showFog s}
    where spd = 6.0 -- if shift mod == Down then 20.0 else 5.0 -- not working for some reason
inputHandler (EventKey (Char c) Up _ _) s 
    | c == 'a' || c == 'd'  = s {xVel = 0.0}
    | c == 'w' || c == 's'  = s {yVel = 0.0}
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
