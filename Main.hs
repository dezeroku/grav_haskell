module Main where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Data.Fixed

data Grav = Grav
    { moonLoc :: (Float, Float) 
    , moonDeg :: Float
    , moonRad :: Float
    , moonMass :: Float

    , planetLoc :: (Float, Float)
    , planetVel :: (Float, Float)
    , planetRad :: Float
    , planetMass :: Float

    , shipLoc :: (Float, Float)
    , shipVel :: (Float, Float)
    , shipRad :: Float
    , shipMass :: Float
} deriving (Show)

startState :: Grav
startState = Grav 
    { moonLoc = (-10, 10)
    , moonDeg = 0
    , moonRad = 15
    , moonMass = 1000

    , planetLoc = (10, 10)
    , planetVel = (0,0)
    , planetRad = 30
    , planetMass = 10000000000
    
    , shipLoc = (100, 60)
    , shipVel = (0,0.2)
    , shipRad = 5
    , shipMass = 10000000
    }

render :: Grav -> Picture
render game = 
    pictures [planet, ship, moon]
    where
    planet = uncurry translate (planetLoc game) $ color planetColor $ circleSolid (planetRad game)
    ship = uncurry translate (shipLoc game) $ color shipColor $ circleSolid (shipRad game)
    moon = uncurry translate (moonLoc game) $ color moonColor $ circleSolid (moonRad game)
    shipColor = red
    planetColor = dark green
    moonColor = light $ light blue

window :: Display
window = InWindow "Nice Window" (200, 200) (10, 10)

background :: Color
background = black

increase :: (Float, Float) -> (Float, Float) -> (Float, Float)
increase a b = (fst a + fst b, snd a + snd b)

gravity :: Float -> Float -> Float -> Float
gravity massOne massTwo radius = (6.674 ** (-11)) * (massOne * massTwo) / (radius ^ 2)

speed :: (Float, Float) -> (Float, Float) -> Float -> (Float, Float)
speed first diffs spe = (firstRes, secondRes)
                        where 
                        przeciw = sqrt (fst diffs ^ 2 + snd diffs ^ 2)
                        xDiff = spe * (fst diffs / przeciw)
                        yDiff = spe * (snd diffs / przeciw)
                        firstRes = fst first + xDiff
                        secondRes = snd first + yDiff
                          

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
        newPlanetLoc = increase (planetLoc game) (planetVel game)
        newShipLoc = increase (shipLoc game) (shipVel game)

        -- Calculate forces. TODO: it's really cheap here
        planetShipDistance = sqrt ((pX - sX) ^ 2 + (pY - sY) ^ 2)
        force = gravity (planetMass game) (shipMass game) planetShipDistance
        speedPlanet = force / (planetMass game)
        speedShip = force / (shipMass game)

        shipDiff = (pX - sX, pY - sY)
        planetDiff = (sX - pX, sY - pY)
        newPlanetVel = speed (planetVel game) planetDiff speedPlanet
        newShipVel = speed (shipVel game) shipDiff speedShip
       

main :: IO ()
--main = putStrLn "LOL"
main = simulate window background 120 startState render step 
