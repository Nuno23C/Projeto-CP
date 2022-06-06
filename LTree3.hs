module LTree3 where

import Cp
import Data.Monoid
import Control.Applicative
import List

-- (1) Datatype definition -----------------------------------------------------

data LTree3 = Tri | Nodo (LTree3 a) (LTree3 a) (LTree3 a) deriving (Eq,Show)

type Tri = (Point,Side) where type Point = (Int,Int)
							  type Side = Int

sierpinski :: (Tri,Int) -> [Int]
sierpinski = folhasSierp . geraSierp

geraSierp :: (Tri,Int) -> LTree3 Tri
geraSierp = anaLTree3 g2

folhasSierp :: LTree3 Tri -> [Tri]
folhasSierp = cataLTree3 g1

baseLTree3 g f = 

-- (2) Ana + cata + hylo -------------------------------------------------------

recLTree3 f = baseLTree3 id f

cataLTree3 g = g - 