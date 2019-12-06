module Network where

import Data.Matrix

type Patterns = Matrix Int
type Labels = Matrix Int

type Weights = Matrix Double
type Thresholds = Matrix Double

type LayerResult = Matrix Double
type Error = Matrix Double

data Layer = Layer Weights Thresholds

forward:: Layer -> LayerResult -> LayerResult
forward (Layer w th) layRes = fmap sigmoid (elementwise (-) (multStd w layRes) th)

sigmoid:: Double -> Double
sigmoid x = 1 / (1 + exp (-x))

sigmoidprim:: Double -> Double
sigmoidprim x = sigmoid x * (1 - sigmoid x)

{-
outputError:: Labels -> LayerResult -> Error
outputError lab layRes = elementwise (*) ()
  where gradient = elementwise (*) layRes ()
-}
