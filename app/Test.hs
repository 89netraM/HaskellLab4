module Test where

import Data.Matrix
import Network

w = fromList 2 3 [-2..]
th = fromList 2 1 [0,0]

p:: Pattern
p = fromList 3 1 [-1..]
l::Label
l = fromList 2 1 [1, 0]

net = network 4567 [3, 2]
