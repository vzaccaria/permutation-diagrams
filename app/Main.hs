{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
module Main where

import Prelude
import Data.Typeable.Internal
import Diagrams.Prelude
import Diagrams.Backend.CmdLine
import Diagrams.Backend.Cairo.CmdLine
import Debug.Trace

-- parameters

-- Sizes

ll :: Fractional a => a
ll = 0.3

ss :: Fractional a => a
ss = 0.1

vs :: Fractional a => a
vs = 0.05

---


vl :: Fractional n => [P2 n]
vl      = [ p2 (0,0), p2 (ll, 0)  ]

vr :: Fractional n => [P2 n]
vr      = [ p2 (-1 * ll,0), p2 (0,0) ]

left
  :: (Floating n, Ord n, Show a, Monoid m, Semigroup m,
      TrailLike (QDiagram b V2 n m)) =>
     a -> QDiagram b V2 n m
left  v =  (square ss ||| fromVertices vl) # named ("L-" ++ show v)

right
  :: (RealFloat n, Show a, Data.Typeable.Internal.Typeable n,
      Renderable (Path V2 n) b) =>
     a -> QDiagram b V2 n Any
right v =  (arrowBetween (head vr) (vr !! 1) ||| square ss ) # named ("R-" ++ show v) 


stride :: Integral a => a -> a -> a -> a
stride i s n = mod ((i - 1) *s) n + quot ((i - 1)*s) n + 1

pri i o = "\n" ++ (show i) ++ " -> " ++ (show o) ++ "\n"

bstride :: Int-> Int-> Int-> Int-> Int
bstride i' s n b = let i = i' - 1
                       q = quot i b
                       r = mod i b
                       p = div n b
                       g = stride (q + 1) s p
                       f = (g - 1) * b + r
                  -- in  Debug.Trace.trace (pri (i,s,n,b) (q,r,p,f)) (f + 1)
                  in  (f + 1)

hmov
  :: (Num a, Num (FinalCoord (Diff p a)), Affine p,
      Coordinates (Diff p a)) =>
     p a -> PrevDim (Diff p a) -> p a
hmov p x = p .+^ (x ^& 0)

attach
  :: (Floating n, Fractional (PrevDim (v n)), Num (FinalCoord (v n)),
      Ord n, Show a, Show a1, Semigroup m, Metric v,
      TrailLike (QDiagram b v n m), Coordinates (v n)) =>
     a1 -> a -> QDiagram b v n m -> QDiagram b v n m
attach n1' n2' =
  let n1 = "L-" ++ show n1'
      n2 = "R-" ++ show n2'
  in withName n1 $ \b1 ->
        withName n2 $ \b2 ->
                        let bb1 = hmov (location b1) (ss/2 + ll)
                            bb2 = hmov (location b2) $ ll * (-1)
                            in atop (bb1 ~~ bb2)

draw :: Int -> Int -> Diagram B
draw s mx = drawBlock s mx 1

pairs s mx b = [(i, bstride i s mx b) | i <- [ 1 ..  mx ] ]
  
drawBlock :: Int -> Int -> Int -> Diagram B
drawBlock s mx b = 
  let 
      pointsl  = vsep ss $ map left  [1 .. mx]
      pointsr  = vsep ss $ map right [1 .. mx]
      points   = hsep 1 [ pointsl, pointsr ]
      prs      = pairs s mx b
   in
    foldl (#) points (map (uncurry attach) prs)


main :: IO ()
main = mainWith drawBlock




