module Lib
    where

import Graphics.Gloss (Color)

data GravState = GravState
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
