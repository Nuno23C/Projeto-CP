module LTree3 where

import Cp
import Data.Monoid
import Control.Applicative
import List

-- (1) Datatype definition -----------------------------------------------------

data LTree3 = Tri a | Nodo (LTree3 a) (LTree3 a) (LTree3 a) deriving (Eq,Show)

type Tri = (Point,Side) where type Point = (Int,Int)
							  type Side = Int

sierpinski :: (Tri,Int) -> [Int]
sierpinski = folhasSierp . geraSierp

geraSierp :: (Tri,Int) -> LTree3 Tri
geraSierp = anaLTree3 g2

folhasSierp :: LTree3 Tri -> [Tri]
folhasSierp = cataLTree3 g1

inLTree :: Either Tri a (LTree3 a, LTree3 a, LTree3 a) -> LTree3 a
inLTree = either Tri Nodo

outLTree :: LTree3 a -> Either Tri a (LTree3 a, LTree3 a, LTree3 a)
outLTree Tri a = i1 a
outLTree (Nodo (t1, t2, t3)) = i2 (t1, t2, t3)

baseLTree3 g f = g -|- ((f >< f) >< f)

-- (2) Ana + cata + hylo -------------------------------------------------------

recLTree3 f = baseLTree3 id f

cataLTree3 g = g . (recLTree3 (cataLTree3 g)) . outLTree3

anaLTree3 f = inLTree3 . (recLTree3 (anaLTree3 f) ) . f

hyloLTree3 f g = cataLTree3 f . anaLTree3 g