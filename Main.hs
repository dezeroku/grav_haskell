module Main where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Data.Fixed (mod')

data Grav = Grav
    { moonLoc :: (Float, Float) 
    , moonDeg :: Float
    , moonRad :: Float
    , moonMass :: Float
    , moonColor :: Color

    , planetLoc :: (Float, Float)
    , planetVel :: (Float, Float)
    , planetRad :: Float
    , planetMass :: Float
    , planetColor :: Color

    , shipLoc :: (Float, Float)
    , shipVel :: (Float, Float)
    , shipRad :: Float
    , shipMass :: Float
    , shipColor :: Color
} deriving (Show)

startState :: Grav
startState = Grav 
    { moonLoc = (-10, 10)
    , moonDeg = 0
    , moonRad = 7
    , moonMass = 1000
    , moonColor = light $ light blue

    , planetLoc = (10, 10)
    , planetVel = (0,0)
    , planetRad = 22
    , planetMass = 10000000000
    , planetColor = dark green
    
    , shipLoc = (100, 60)
    , shipVel = (0,0.2)
    , shipRad = 3
    , shipMass = 10000000
    , shipColor = red
    }

render :: Grav -> Picture
render game = 
    pictures [planet, ship, moon]
    where
    planet = uncurry translate (planetLoc game) $ color (planetColor game) $ circleSolid (planetRad game)
    ship = uncurry translate (shipLoc game) $ color (shipColor game) $ circleSolid (shipRad game)
    moon = uncurry translate (moonLoc game) $ color (moonColor game) $ circleSolid (moonRad game)

move :: (Float, Float) -> (Float, Float) -> (Float, Float)
move (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

gravityForce :: Float -> Float -> Float -> Float
gravityForce massOne massTwo radius = (gCons * massOne * massTwo) / (radius ^ 2)
  where gCons = 6.674 ** (-11)

calculateSpeed :: (Float, Float) -> (Float, Float) -> Float -> (Float, Float)
calculateSpeed (xSpeed, ySpeed) (xDiff, yDiff) speed = (xSpeed + xDiffC, ySpeed + yDiffC)
                        where 
                        c = sqrt (xDiff ^ 2 + yDiff ^ 2)
                        xDiffC = speed * (xDiff / c)
                        yDiffC = speed * (yDiff / c)

step :: ViewPort -> Float -> Grav -> Grav
step _ _ game = game 
    { moonDeg = newMoonDeg
    , moonLoc = newMoonLoc
    
    , planetLoc = newPlanetLoc
    , planetVel = newPlanetVel

    , shipLoc = newShipLoc
    , shipVel = newShipVel
    }
    where
        newMoonDeg = mod' ((moonDeg game) + 0.01) 360
        (pX, pY) = planetLoc game
        (sX, sY) = shipLoc game

        -- Moon simulation.
        planetMoonDistance = 40
        realMoonDistance = planetMoonDistance + (moonRad game) / 2 + (planetRad game) / 2
        newMoonLoc = ((sin newMoonDeg * realMoonDistance) + pX, (cos newMoonDeg * realMoonDistance) + pY)
 
        -- Move stuff
        newPlanetLoc = move (planetLoc game) (planetVel game)
        newShipLoc = move (shipLoc game) (shipVel game)

        -- Calculate forces. TODO: it's really cheap here
        planetShipDistance = sqrt ((pX - sX) ^ 2 + (pY - sY) ^ 2)
        force = gravityForce (planetMass game) (shipMass game) planetShipDistance
        speedPlanet = force / planetMass game
        speedShip = force / shipMass game

        shipDiff = (pX - sX, pY - sY)
        planetDiff = (sX - pX, sY - pY)
        newPlanetVel = calculateSpeed (planetVel game) planetDiff speedPlanet
        newShipVel = calculateSpeed (shipVel game) shipDiff speedShip

window :: Display
window = InWindow "Gravity..." (200, 200) (10, 10)

background :: Color
background = black

main :: IO ()
main = simulate window background 120 startState render step 
