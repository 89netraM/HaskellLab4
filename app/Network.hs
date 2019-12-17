module Network where

import Data.Matrix hiding (trace)
import Test.QuickCheck
import Matrix
import Data.Random.Normal
import Debug.Trace

-------------------------------------
-- | Types
type Pattern = Matrix Int
type Label = Matrix Int

type Weights = Matrix Double
type Thresholds = Matrix Double

type LayerResult = Matrix Double
type Error = Matrix Double

data Layer = Layer Weights Thresholds
  deriving Show

----------------------------------------------
learnRate:: Double
learnRate = 0.1

-- | w:      nOut*nIn
-- | th:     nOut*1
-- | layRes: nIn*1
-- | returns: nOut*1
-- | Forward propagation
forward:: Layer -> LayerResult -> LayerResult
forward (Layer w th) layRes = fmap sigmoid ((multStd w layRes) #- th)

prop_forward w th layRes = (nrows th, ncols th) == (nrows forward_value, ncols forward_value)
  where forward_value = forward (Layer w th) layRes
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
-- | lab:     nOut*1
-- | layRes:  nOut*1
-- | returns: nOut*1
-- | The output error
outputError:: Label -> LayerResult -> Error
outputError lab layRes = (gradient layRes) #* (fmap fromIntegral lab #- layRes)

-- | err:     nOut*1
-- | w:       nOut*nIn
-- | layRes:  nIn*1
-- | returns: nIn*1
-- | Backpropagation
backward:: Error -> Weights -> LayerResult -> Error
backward err w layRes = (transpose $ multStd (transpose err) w) #* (gradient layRes)

-- | w:       nOut*nIn
-- | err:     nOut*1
-- | layRes:  nIn*1
-- | returns: nOut*nIn
updateWeights:: Weights -> Error -> LayerResult -> Weights
updateWeights w err prevlayRes = w #+ (scaleMatrix learnRate prod)
   where prod = multStd err (transpose prevlayRes)

traceDim:: Num a => Matrix a -> Matrix a
traceDim m = trace (show (nrows m, ncols m)) m

-- | th:      nOut*1
-- | err:     nOut*1
-- | returns: nOut*1
updateThresholds:: Thresholds -> Error -> Thresholds
updateThresholds th err = th #- scaleMatrix learnRate err

update:: Layer -> Error -> LayerResult -> Layer
update (Layer w th) err prevlayRes =
  Layer (updateWeights w err prevlayRes) (updateThresholds th err)

--------------------------------------------------
-- | Network type
type Network = [Layer]

-- | Takes a list of number of neurons for each layer and initializes a network
network:: Int -> [Int] -> Network
network seed (x:y:xs) = [Layer (initWeights n1 n2 r) (initThresholds n2) | (n1, n2, r) <- zip3 (x:y:xs) (y:xs) [0,1..]]
  where initWeights:: Int -> Int -> Int -> Weights
        initWeights n1 n2 r = fromList n2 n1 (mkNormals' (0.0, 1.0 / fromIntegral n1) (seed + r))
        initThresholds:: Int -> Thresholds
        initThresholds n2 = fromList n2 1 [0.0,0.0..]
network _ _ = error "Needs at least 2 layers: input and output"

------------------------------------------------
-- | Takes the network and input value (in double) and gives a list of layer results
forwardLoop:: Network -> LayerResult -> [LayerResult]
forwardLoop []     _     = []
forwardLoop (l:ls) input = layRes : forwardLoop ls layRes
  where layRes = forward l input

-- | Takes the network and reversed layer result list and the output error and gives "reversed" error list
backwardLoop:: Network -> [LayerResult] -> Error -> [Error]
backwardLoop (Layer w th:ls) (lr:lrs) err = errOut : backwardLoop ls lrs errOut
  where errOut = backward err w lr
backwardLoop  _               _       _   = []

-- | Takes a network, a list of errors, and a list of results and updates the network.
updateLoop:: Network -> [Error] -> [LayerResult] -> Network
updateLoop (l:ls) (e:es) (lr:lrs) = update l e lr : updateLoop ls es lrs
updateLoop _      _      _        = []

mainLoop:: Network -> [(Pattern, Label)] -> Network
mainLoop net ((p, l) : pls) = mainLoop net2 pls
  where inputLayer :: LayerResult
        inputLayer = (fmap fromIntegral p)
        layers = forwardLoop net inputLayer
        revLay = reverse layers
        outErr = outputError l (head revLay)
        errs   = reverse $ outErr : backwardLoop net (tail revLay) outErr
        net2   = updateLoop net errs (inputLayer:layers)
mainLoop net []             = net

-- | Takes a (trained) network and a pattern, and returns it's guess of the answer.
getResult:: Network -> Pattern -> LayerResult
getResult net p = last $ forwardLoop net (fmap fromIntegral p)
