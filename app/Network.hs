module Network where

import Data.Matrix
import Test.QuickCheck
import Matrix
--import Data.Random.Normal

-------------------------------------
-- | Types
type Patterns = Matrix Int
type Labels = Matrix Int

type Weights = Matrix Double
type Thresholds = Matrix Double

type LayerResult = Matrix Double
type Error = Matrix Double

data Layer = Layer Weights Thresholds

----------------------------------------------
learnRate:: Double
learnRate = 0.1

-- | Forward propagation
forward:: Layer -> LayerResult -> LayerResult
forward (Layer w th) layRes = fmap sigmoid (multStd w layRes #- th)

---------------------------------------------
-- | Sigmoid functions
sigmoid:: Double -> Double
sigmoid x = 1 / (1 + exp (-x))

prop_sigmoid:: Double -> Bool
prop_sigmoid x = sigmoid x <= 1.0 && sigmoid x >= 0.0

sigmoidprim:: Double -> Double
sigmoidprim x = sigmoid x * (1 - sigmoid x)

gradient:: Num a => Matrix a -> Matrix a
gradient = fmap (\x -> x*(1-x))

-------------------------------------------------
-- | The output error
outputError:: Labels -> LayerResult -> Error
outputError lab layRes = gradient layRes #* (fmap fromIntegral lab #- layRes)

-- | err:    1*nOut
-- | w:      nOut*nIn
-- | layRes: 1*nIn
-- | Backpropagation
backward:: Error -> Weights -> LayerResult -> Error
backward err w layRes = multStd err w #* gradient layRes

updateWeights:: Weights -> Error -> LayerResult -> Weights
updateWeights w err layRes =
  w #+ scaleMatrix learnRate (multStd (transpose err) layRes)

updateThresholds:: Thresholds -> Error -> Thresholds
updateThresholds th err = th #- scaleMatrix learnRate err

update:: Layer -> Error -> LayerResult -> Layer
update (Layer w th) err layRes =
  Layer (updateWeights w err layRes) (updateThresholds th err)

--------------------------------------------------
-- | Network type
type Network = [Layer]

-- | Takes a list of number of neurons for each layer and initializes a network
network:: Int -> [Int] -> Network
network seed (x:y:xs) = []
  where initWeights:: Int -> Int -> Weights
        initWeights n1 n2 = fromList n2 n1 [0.0,0.0..]
        initThresholds:: Int -> Thresholds
        initThresholds n2 = fromList 1 n2 [0.0,0.0..]
network _ _ = error "Needs at least 2 layers: input and output"
