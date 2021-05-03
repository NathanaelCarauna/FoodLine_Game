module Main where
import Lib
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game
    ( KeyState(Down, Up),
      SpecialKey(KeySpace, KeyLeft, KeyRight),
      Key(SpecialKey),
      Event(EventKey) )
import System.Random

type Position = (Float, Float)
type Velocity = Float
type Object  = (Position, Velocity, Picture)

-- Defining game elements
data SpaceSurvivalGame = Game
  { player :: Object
  , bullets :: [Object]
  , asteroids :: [Object]
  , paused :: Bool
  } deriving Show

-- Initial game state
initialState :: SpaceSurvivalGame
initialState = Game
  { player = ((0, -250), 0, ship)
  , bullets = [((0,0),0, bullet)]
  , asteroids = [((0,250), -2, asteroid 5 5)]
  , paused = False  
  }


generateRandomValue :: (Random a, RandomGen b) => a -> a -> b -> a
generateRandomValue l h g = do fst $ randomR (l, h) g                        


-- Asteroid Picture
asteroid :: Float -> Float -> Picture
asteroid w h = scale w h $ color white $ lineLoop
          [(1,5),(1,6),(2,4),(3,3),(4,3),(4,2),(3,2),(4,0),(3,-1),(2,-3),(0,-3),(-3,1),(-4,2),(-4,3),(-2,3),(1,5)]

-- Bullet Picture
bullet :: Picture
bullet = color white $ rectangleSolid 3 5

-- Player ship Picture
ship :: Picture
ship = color white $ lineLoop [(10,0), (0,25 ), (-10, 0), (9,0)]                

-- Draw an object on the screen
drawObject :: Object -> Picture
drawObject ((x,y), v, p) =
  translate x y p

-- Draw an Object list on the screen
drawObjectList :: [Object] -> Picture
drawObjectList objects = 
  pictures $ map drawObject objects        

-- Call all the drawers
render :: SpaceSurvivalGame -> Picture
render game = 
    pictures [ drawObject (player game)            
             , drawObjectList (bullets game)
             , drawObjectList (asteroids game)]


-- Keys configurations
handleKeys :: Event -> SpaceSurvivalGame -> SpaceSurvivalGame
handleKeys (EventKey (SpecialKey KeyLeft ) Down _ _) game = game { player = updateVelocity (player game) (-playerSpeed)}
handleKeys (EventKey (SpecialKey KeyLeft ) Up _ _) game = game {player = updateVelocity (player game) 0}
handleKeys (EventKey (SpecialKey KeyRight ) Down _ _) game = game {player = updateVelocity (player game) playerSpeed}
handleKeys (EventKey (SpecialKey KeyRight ) Up _ _) game = game {player = updateVelocity (player game) 0}
handleKeys (EventKey (SpecialKey KeySpace ) Down _ _) game = bulletsGenerator game
-- handleKeys (EventKey (SpecialKey KeyUp ) Up _ _) game = game {}
-- handleKeys (EventKey (SpecialKey KeyDown ) Down _ _) game = game {}
-- handleKeys (EventKey (SpecialKey KeyDown ) Up _ _) game = game {}
handleKeys _ game = game

-- asteroidsGenerator :: SpaceSurvivalGame -> SpaceSurvivalGame
-- asteroidsGenerator game  
--   | asteroids game == [] = game{asteroids = ((x, y), 0, asteroid) : asteroids game}



verifyPosition :: (Ord a1, Num a1) => a1 -> ((a2, a1), b, c) -> Bool
verifyPosition lim ((x, y), v, p) 
                                  | lim < 0 && y <= lim = False
                                  | lim > 0 && y >= lim = False
                                  | otherwise = True 
                                

verifyBulletPositionToDestroy :: SpaceSurvivalGame -> SpaceSurvivalGame
verifyBulletPositionToDestroy game = game {bullets = filter (verifyPosition 250) (bullets game)  }


-- Generate bullets at player position
bulletsGenerator :: SpaceSurvivalGame -> SpaceSurvivalGame
bulletsGenerator game = game {bullets = (retrieveObjectPosition (player game), 2, bullet) : bullets game}

--Retrieve the position from a given object
retrieveObjectPosition :: Object -> Position
retrieveObjectPosition ((x, y), v, p) = (x, y)

-- Retrieve the velocity of a given object
retrieveVelocity :: Object -> Float
retrieveVelocity ((x, y), v, p) = v

updateVelocity :: Object -> Float -> Object
updateVelocity  ((x, y), v, p ) velocity =  ((x, y), velocity, p )

updatePositionX :: Object -> Float
updatePositionX ((x, y), v, p)  = x + v

updatePositionY :: Object -> Float
updatePositionY ((x, y), v, p)  = y + v

updateObject :: Object -> Object
updateObject ((x, y), v, p) = ((x, y+v), v, p)

updateObjectPosition :: [Object] -> [Object]
updateObjectPosition objs = map updateObject objs


updatePlayerPosition :: SpaceSurvivalGame -> SpaceSurvivalGame
updatePlayerPosition game = game {player = ((limitMovement x' width 20,  y ), v, ship)}
                        where
                          y = -250
                          x' = updatePositionX (player game)
                          v = retrieveVelocity( player game)

updateBulletPosition :: SpaceSurvivalGame -> SpaceSurvivalGame
updateBulletPosition game = game {bullets = updateObjectPosition (bullets game)}              

updateAsteroidPosition :: SpaceSurvivalGame -> SpaceSurvivalGame
updateAsteroidPosition game = game {asteroids = updateObjectPosition (asteroids game)}        


-- Call the all functions and update the game
update :: Float -> SpaceSurvivalGame -> SpaceSurvivalGame
update seconds game = if not (paused game) then ( updatePlayerPosition 
                                                . updateBulletPosition
                                                . updateAsteroidPosition
                                                . verifyBulletPositionToDestroy
                                                ) game else game

limitMovement :: Float -> Int -> Float -> Float
limitMovement move width playerWidth
        | move < leftLimit = leftLimit
        | move > rightLimit = rightLimit 
        | otherwise = move
        where
          leftLimit = playerWidth/2 - fwidth/2 
          rightLimit = fwidth /2 - playerWidth/2
          fwidth = fromIntegral width :: Float

playerSpeed :: Float
playerSpeed = 5

-- Window dimensions
width, height, offset :: Int
width = 400
height = 600
offset = 150

-- Window configurations
window :: Display
window = InWindow "Space Survival" (width, height) (offset, offset)

-- Background color/image
background :: Color
background = black

-- Frames per second
fps :: Int
fps = 60

-- Main
main :: IO ()
main = play window background fps initialState render handleKeys update