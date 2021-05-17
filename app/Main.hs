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
import Random

type Position = (Float, Float)
type Velocity = Float
type Scale = Float
type IsAlive = Bool
type Object  = (Position, Velocity, Picture, Scale, IsAlive)

-- Defining game elements
data SpaceSurvivalGame = Game
  { player :: Object
  , lifes :: Float
  , score :: Float
  , bullets :: [Object]
  , indexRandomPosition :: Int
  , indexRandomScale :: Int
  , asteroids :: [Object]
  , time :: Float
  , paused :: Bool
  } deriving Show

-- Initial game state
initialState :: SpaceSurvivalGame
initialState = Game
  { player = ((0, -250), 0, ship, 0, True)
  , lifes = 3
  , score = 0
  , bullets = []
  , indexRandomPosition = 0
  , indexRandomScale = 0
  , asteroids = []
  , time = 0
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
drawObject ((x,y), v, p, s, a) =
  translate x y p

-- Draw an Object list on the screen
drawObjectList :: [Object] -> Picture
drawObjectList objects =
  pictures $ map drawObject objects

-- Call all the drawers
render :: SpaceSurvivalGame -> Picture
render game | not (paused game) = pictures [ drawObject (player game)
                                                 , drawObjectList (bullets game)
                                                 , drawObjectList (asteroids game)
                                                 , drawObject ((-175,250),0, scale 0.1 0.1 $ color white $ Text $show (score game), 0, True )
                                            ]
            |paused game = drawObject ((-175,0),0, scale 0.4 0.4 $ color white $ Text "Jogo Pausado", 0, True)
            |otherwise = drawObject ((-175,0),0, scale 0.4 0.4 $ color white $ Text "Game Over",0, True)



-- Keys configurations
handleKeys :: Event -> SpaceSurvivalGame -> SpaceSurvivalGame
handleKeys (EventKey (SpecialKey KeyLeft ) Down _ _) game = game { player = updateVelocity (player game) (-playerSpeed)}
handleKeys (EventKey (SpecialKey KeyLeft ) Up _ _) game = game {player = updateVelocity (player game) 0}
handleKeys (EventKey (SpecialKey KeyRight ) Down _ _) game = game {player = updateVelocity (player game) playerSpeed}
handleKeys (EventKey (SpecialKey KeyRight ) Up _ _) game = game {player = updateVelocity (player game) 0}
handleKeys (EventKey (SpecialKey KeySpace ) Down _ _) game = bulletsGenerator game
handleKeys (EventKey (Char 'p'  ) Up _ _) game = game {paused = not $ paused game}
handleKeys (EventKey (Char 'a' ) Down _ _) game = asteroidsGenerator game
-- handleKeys (EventKey (SpecialKey KeyDown ) Up _ _) game = game {}
handleKeys _ game = game


asteroidsGenerator :: SpaceSurvivalGame -> SpaceSurvivalGame
asteroidsGenerator game
                        | indexRandomPosition game >= 600 = game {indexRandomPosition = 0}
                        | indexRandomScale game >= 45 = game {indexRandomScale = 0}
                        | time game >= 100 = game
                              { asteroids =  ((xPosition, 250), -2, asteroid scale scale, scale, True) : asteroids game
                              , indexRandomPosition = indexRandomPosition game + 1
                              , indexRandomScale = indexRandomScale game + 1
                              , time = 0
                              }
                        | otherwise = game

              where
                xPosition =retrieveValueFromList (indexRandomPosition game) randomXpositions
                scale = retrieveValueFromList (indexRandomScale game) randomScale

clock :: SpaceSurvivalGame -> SpaceSurvivalGame
clock game = game{time = time game + 1}

retrieveValueFromList :: Int -> [Float] -> Float
retrieveValueFromList index list = list !! index

retrieveScale :: Object -> Float
retrieveScale ((x, y), v, p, s, a) = s

-- Verify object position and return if position is greater than limit
verifyPositionY :: (Ord a1, Num a1) => a1 -> ((a2, a1), b, c, s, a) -> Bool
verifyPositionY lim ((x, y), v, p, s, a)
                                  | lim < 0 && y <= lim = False
                                  | lim > 0 && y >= lim = False
                                  | otherwise = True


verifyBulletPositionToDestroy :: SpaceSurvivalGame -> SpaceSurvivalGame
verifyBulletPositionToDestroy game = game {bullets = filter (verifyPositionY 250) (bullets game)  }

verifyAsteroidPositionToDestroy :: SpaceSurvivalGame -> SpaceSurvivalGame
verifyAsteroidPositionToDestroy game = game {asteroids = filter (verifyPositionY (-250)) (asteroids game)  }


-- Generate bullets at player position
bulletsGenerator :: SpaceSurvivalGame -> SpaceSurvivalGame
bulletsGenerator game = game {bullets = (sumInY (retrieveObjectPosition (player game)) (0,30), 2, bullet, 2, True) : bullets game}

sumInY :: Position-> Position -> Position
sumInY (x,y) (x1,y1) = (x+x1,y+y1)

--Retrieve the position from a given object
retrieveObjectPosition :: Object -> Position
retrieveObjectPosition ((x, y), v, p, s, a) = (x, y)


-- Update player lifes
updatePlayerLifes :: Float -> Float -> Float
updatePlayerLifes currentLife value = currentLife + value

-- Update Score
updateScore :: Float -> Float -> Float
updateScore currentScore points = currentScore + points


-- Verify if a collisionB happened
collisionB :: Object -> Object -> Object
collisionB ((x1, y1), v1, p1, s1, a1) ((x2, y2), v2, p2, s2, a2)
                                             | x1 >= x2 - s2 * raio && x1 <= x2 + s2 * raio && y1 >= y2 - s2 * raio = ((x1, y1), v1, p1, s1, False)
                                             | otherwise = ((x1, y1), v1, p1, s1, True)
                                          where 
                                            raio = 3

verifyCollisionB :: [Object] ->Object -> Object
verifyCollisionB meteoros bala@((x1, y1), v1, p1, s1, a1) 
                      | null meteoros = bala
                      | a1 == False = ((x1, y1), v1, p1, s1, False)
                      | a1 == True = foldl (collisionB) bala meteoros

collisionA :: Object -> Object -> Object
collisionA ((x1, y1), v1, p1, s1, a1) ((x2, y2), v2, p2, s2, a2)
                                             | x1 - s1 * raio <= x2  && x1 + s1 * raio >= x2 && y1 - s1 * raio <= y2 = ((x1, y1), v1, p1, s1, False)
                                             | otherwise = ((x1, y1), v1, p1, s1, True)
                                      where
                                        raio = 3

verifyCollisionA :: [Object] ->Object -> Object
verifyCollisionA balas meteoro@((x1, y1), v1, p1, s1, a1) 
                      | null balas = meteoro
                      | a1 == False = ((x1, y1), v1, p1, s1, False)
                      | a1 == True = foldl (collisionA) meteoro balas




verifyBulletVsAsteroidCollision :: SpaceSurvivalGame -> SpaceSurvivalGame
verifyBulletVsAsteroidCollision game = game { 
                                              bullets = filter retrieveIsAlive $ map (verifyCollisionB meteoros) balas
                                            , asteroids = filter retrieveIsAlive $ map(verifyCollisionA balas) meteoros                                            
                                            }
                                      where                        
                                          balas = bullets game
                                          meteoros = asteroids game


retrieveIsAlive :: Object -> Bool
retrieveIsAlive ((x1, y1), v1, p1, s1, a1) = a1

-- Retrieve the velocity of a given object
retrieveVelocity :: Object -> Float
retrieveVelocity ((x, y), v, p, s, a) = v

updateVelocity :: Object -> Float -> Object
updateVelocity  ((x, y), v, p, s, a) velocity =  ((x, y), velocity, p,s,a )

updatePositionX :: Object -> Float
updatePositionX ((x, y), v, p, s, a)  = x + v

updatePositionY :: Object -> Float
updatePositionY ((x, y), v, p, s, a)  = y + v

updateObject :: Object -> Object
updateObject ((x, y), v, p, s, a) = ((x, y+v), v, p,s, a)

updateObjectPosition :: [Object] -> [Object]
updateObjectPosition objs = map updateObject objs


updatePlayerPosition :: SpaceSurvivalGame -> SpaceSurvivalGame
updatePlayerPosition game = game {player = ((limitMovement x' width 20,  y ), v, ship, 0, True)}
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
update seconds game | not (paused game) = ( updatePlayerPosition
                                          . updateBulletPosition
                                          . updateAsteroidPosition
                                          . verifyBulletPositionToDestroy
                                          . verifyAsteroidPositionToDestroy
                                          . clock
                                          . asteroidsGenerator
                                          . verifyBulletVsAsteroidCollision
                                          ) game
                    | otherwise = game

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