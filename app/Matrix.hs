module Matrix where

import Data.Matrix

(#-):: Num a => Matrix a -> Matrix a -> Matrix a
(#-) = elementwise (-)

(#+):: Num a => Matrix a -> Matrix a -> Matrix a
(#+) = elementwise (+)

(#*):: Num a => Matrix a -> Matrix a -> Matrix a
(#*) = elementwise (*)
