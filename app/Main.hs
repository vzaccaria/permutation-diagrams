{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Prelude
import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine
import Data.Colour.SRGB.Linear

l = [(0,0), (0,1), (0,2)]

r = [(1,0), (1,1), (1,2)]

left  v = showOrigin $ (square 0.1 ||| (fromVertices [ p2 (0,0), p2 (0.1,0)])) # named ("L-" ++ show v)
right v = showOrigin $ ((fromVertices [ p2 (-0.1,0), p2 (0,0)]) ||| square 0.1 ) # named ("R-" ++ show v) 


pointsl = vsep 0.05 $ map left [1 .. 5]
pointsr = vsep 0.05 $ map right [1 .. 5]
points = hsep 1 [ pointsl, pointsr ]

hmov p x = p .+^ ((x ^& 0))

attach n1 n2 =
  withName n1 $ \b1 ->
     withName n2 $ \b2 ->
                     let bb1 = hmov (location b1) 0.15
                         bb2 = hmov (location b2) (-0.1)
                         in atop ((bb1 ~~ bb2) # lc red)

dia =  points # (attach "L-1" "R-5")

main = mainWith (dia :: Diagram B)




