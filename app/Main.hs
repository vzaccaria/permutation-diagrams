{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Prelude
import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine

-- parameters

-- Sizes

ll = 0.3
ss = 0.1
vs = 0.05

---


vl      = [ p2 (0,0), p2 (ll, 0)  ]
vr      = [ p2 (-1 * ll,0), p2 (0,0) ]
left  v =  (square ss ||| (fromVertices vl)) # named ("L-" ++ show v)
right v =  ((arrowBetween (vr !! 0) (vr !! 1)) ||| square ss ) # named ("R-" ++ show v) 


stride i s n = mod ((i - 1) *s) n + quot ((i - 1)*s) n + 1

hmov p x = p .+^ ((x ^& 0))

attach n1' n2' =
  let n1 = "L-" ++ (show n1')
      n2 = "R-" ++ (show n2')
  in withName n1 $ \b1 ->
        withName n2 $ \b2 ->
                        let bb1 = hmov (location b1) $ (ss/2 + ll)
                            bb2 = hmov (location b2) $ ll * (-1)
                            in atop (bb1 ~~ bb2)

draw s mx =
  let 
      pointsl  = vsep ss $ map left  [1 .. mx]
      pointsr  = vsep ss $ map right [1 .. mx]
      points   = hsep 1 [ pointsl, pointsr ]
      pairs    = [(i, stride i s mx) | i <- [ 1 ..  mx] ]
  in
    foldl (#) points (map (uncurry attach) pairs)

main = mainWith ((draw 8 16):: Diagram B)




