module Main where

import Network
import World
import Graphics.Gloss
import System.Random

main :: IO ()
main = do
   std <- getStdGen
   simulate (InWindow "ANN Simulation" (500, 500) (10, 10))
            white
            5
            (initWorld std)
            renderWorld
            updateWorld

