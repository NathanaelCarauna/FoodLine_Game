module Main where
import Lib
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game

type Position = (Float, Float)

-- Defining game elements
data SpaceSurvivalGame = Game
  { shipLocation :: (Float, Float)        
  , shipHVelocity :: Float
  , asteroidPosition :: (Float, Float)
  , paused :: Bool
  } deriving Show

-- Initial game state
initialState :: SpaceSurvivalGame
initialState = Game
  { shipLocation = (0,-250)
  , shipHVelocity = 0
  , asteroidPosition = (0, 250)
  , paused = False  
  }

asteroidShape :: Float -> Float -> Float -> [Point]
asteroidShape x y s = [(x,y+s),(x+s,y), (x,y+0.5),(x+0.4*s,y-0.4*s),(x-0.4*s,y+0.5*s),(x-0.2*s,y-0.2*s),(x+0.6*s,y+0.3*s)]

-- Draw all pictures at screen
render :: SpaceSurvivalGame -> Picture
render game = 
    pictures [ mkShip $ shipLocation game
             , asteroid 5 8]

    where
        mkShip :: Position -> Picture
        mkShip (x,y) = translate x y $ color white $ lineLoop [(10,0), (0,25 ), (-10, 0), (9,0)]

        
        asteroid x y= scale x y $color white $ lineLoop[(1,5),(1,6),(2,4),(3,3),(4,3),(4,2),(3,2),(4,0),(3,-1),(2,-3),(0,-3),(-3,1)
          ,(-4,2),(-4,3),(-2,3),(1,5)]


-- Keys configurations
handleKeys :: Event -> SpaceSurvivalGame -> SpaceSurvivalGame
handleKeys (EventKey (SpecialKey KeyLeft ) Down _ _) game = game { shipHVelocity = -playerSpeed }
handleKeys (EventKey (SpecialKey KeyLeft ) Up _ _) game = game {shipHVelocity = 0}
handleKeys (EventKey (SpecialKey KeyRight ) Down _ _) game = game {shipHVelocity = playerSpeed}
handleKeys (EventKey (SpecialKey KeyRight ) Up _ _) game = game {shipHVelocity = 0}
-- handleKeys (EventKey (SpecialKey KeyUp ) Down _ _) game = game {}
-- handleKeys (EventKey (SpecialKey KeyUp ) Up _ _) game = game {}
-- handleKeys (EventKey (SpecialKey KeyDown ) Down _ _) game = game {}
-- handleKeys (EventKey (SpecialKey KeyDown ) Up _ _) game = game {}
handleKeys _ game = game

updatePlayerPosition :: SpaceSurvivalGame -> SpaceSurvivalGame
updatePlayerPosition game = game {shipLocation = (limitMovement x' width 20,  y )}
                        where
                          y = snd $ shipLocation game
                          x' = shipHVelocity game + fst (shipLocation game)

-- Call the all functions and update the game
update :: Float -> SpaceSurvivalGame -> SpaceSurvivalGame
update seconds game = if not (paused game) then updatePlayerPosition game else game

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