module Matrix where

import Data.Matrix

--Matrix operations elementwise
(#+):: Num a => Matrix a -> Matrix a -> Matrix a
(#+) = elementwise (+)

(#-):: Num a => Matrix a -> Matrix a -> Matrix a
(#-) = elementwise (-)

(#*):: Num a => Matrix a -> Matrix a -> Matrix a
(#*) = elementwise (*)
