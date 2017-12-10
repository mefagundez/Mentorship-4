module Main where

import Lib

main :: IO ()

main = do
    arbolTest <- arbol1
    putStrLn $ show $ walkWithFunction (+2) arbolTest
    putStrLn $ show $ walkToList arbolTest
