module Main where
import Lib
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game
    ( KeyState(Down, Up),
      SpecialKey(KeySpace, KeyLeft, KeyRight),
      Key(SpecialKey, Char),
      Event(EventKey) )
import System.Random
import RandomXpositions

type Position = (Float, Float)
type Velocity = Float
type Object  = (Position, Velocity, Picture)

-- Defining game elements
data SpaceSurvivalGame = Game
  { player :: Object
  , lifes :: Float
  , score :: Float
  , bullets :: [Object]
  , asteroids :: [Object]
  , paused :: Bool
  } deriving Show

-- Initial game state
initialState :: SpaceSurvivalGame
initialState = Game
  { player = ((0, -250), 0, ship)
  , lifes = 3
  , score = 0
  , bullets = [((0,0),0, bullet)]
  , asteroids = [((head randomXpositions, 250), -2, asteroid 5 5)]
  , paused = False  
  }
            

-- Asteroid Picture
asteroid :: Float -> Float -> Picture
asteroid w h = scale w h $ color white $ lineLoop
          [(0, 4), (2, 2), (2, 0), (0, -2), (-1, -2), (-2, -1), (-2, 0), (-3, 1), (-3, 2), (-1, 4), (0, 4)]

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
handleKeys (EventKey (Char 'p'  ) Up _ _) game = game {paused = not $ paused game}
-- handleKeys (EventKey (SpecialKey KeyDown ) Down _ _) game = game {}
-- handleKeys (EventKey (SpecialKey KeyDown ) Up _ _) game = game {}
handleKeys _ game = game

-- asteroidsGenerator :: SpaceSurvivalGame -> SpaceSurvivalGame
-- asteroidsGenerator game  
--   | asteroids game == [] = game{asteroids = ((x, y), 0, asteroid) : asteroids game}



-- Verify object position and return if position is greater than limit
verifyPositionY :: (Ord a1, Num a1) => a1 -> ((a2, a1), b, c) -> Bool
verifyPositionY lim ((x, y), v, p) 
                                  | lim < 0 && y <= lim = False
                                  | lim > 0 && y >= lim = False
                                  | otherwise = True 
                                

verifyBulletPositionToDestroy :: SpaceSurvivalGame -> SpaceSurvivalGame
verifyBulletPositionToDestroy game = game {bullets = filter (verifyPositionY 250) (bullets game)  }

verifyAsteroidPositionToDestroy :: SpaceSurvivalGame -> SpaceSurvivalGame
verifyAsteroidPositionToDestroy game = game {asteroids = filter (verifyPositionY (-250)) (asteroids game)  }


-- Generate bullets at player position
bulletsGenerator :: SpaceSurvivalGame -> SpaceSurvivalGame
bulletsGenerator game = game {bullets = (sumInY (retrieveObjectPosition (player game)) (0,30), 2, bullet) : bullets game}                        

sumInY :: Position-> Position -> Position
sumInY (x,y) (x1,y1) = (x+x1,y+y1) 

--Retrieve the position from a given object
retrieveObjectPosition :: Object -> Position
retrieveObjectPosition ((x, y), v, p) = (x, y)


-- Update player lifes
updatePlayerLifes :: Float -> Float -> Float
updatePlayerLifes currentLife value = currentLife + value

-- Update Score
updateScore :: Float -> Float -> Float
updateScore currentScore points = currentScore + points

-- Verify if a collision happened
collision :: Object -> Object -> Bool
collision ((x1, y1), v1, p1) ((x2, y2), v2, p2) 
                                             | x1 >= x2 -15 && x1 <= x2 + 15 && y1 >= y2 + 15 && y1 <= y2 - 15 = False
                                             | otherwise = True

-- verifyBulletVsAsteroidCollision :: SpaceSurvivalGame -> SpaceSurvivalGame
-- verifyBulletVsAsteroidCollision game = game {bullets = filter collision ()}

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
                                                . verifyAsteroidPositionToDestroy
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
main = do
  play window background fps initialState render handleKeys update