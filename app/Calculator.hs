module Calculator where

-- This module is a starting point for implementing the Graph Drawing
-- Calculator as described in Part II of the Standard Lab. You can use this
-- directly, or just study it as an example of how to use threepenny-gui.

import ThreepennyPages
import Graphics.UI.Threepenny.Core as UI
import qualified Graphics.UI.Threepenny as UI
import Expr
import Data.Maybe

canWidth,canHeight :: Num a => a
canWidth  = 300
canHeight = 300

points :: Expr -> Double -> (Int, Int) -> [Point]
points exp scale (w,h) = zip (map (\x -> halfW + (x / scale)) vals) es
   where es = map (\x -> halfH - (eval exp x / scale)) vals
         vals = [start,start+scale..end]
         halfW = fromIntegral w / 2
         halfH = fromIntegral h / 2
         end = halfW * scale
         start = -end

main :: IO ()
main = startGUI defaultConfig setup

setup :: Window -> UI ()
setup window =
  do -- Create them user interface elements
     canvas  <- mkCanvas canWidth canHeight   -- The drawing area
     fx      <- mkHTML "<i>f</i>(<i>x</i>)="  -- The text "f(x)="
     input   <- mkInput 20 "x"                -- The formula input
     draw    <- mkButton "Draw graph"         -- The draw button
     zoom    <- mkInput 20 "0.004"            -- The zoom box
       -- The markup "<i>...</i>" means that the text inside should be rendered
       -- in italics.

     -- Add the user interface elements to the page, creating a specific layout
     formula <- row [pure fx,pure input]
     getBody window #+ [column [pure canvas,pure formula,pure zoom,pure draw]]

     -- Styling
     getBody window # set style [("backgroundColor","lightblue"),
                                 ("textAlign","center")]
     pure input # set style [("fontSize","14pt")]

     -- Interaction (install event handlers)
     on UI.click     draw  $ \ _ -> readAndDraw input zoom canvas
     on valueChange' input $ \ _ -> readAndDraw input zoom canvas


readAndDraw :: Element -> Element -> Canvas -> UI ()
readAndDraw input zoom canvas =
  do -- Get the current formula (a String) from the input element
     formula <- get value input
     zoomStr <- get value zoom
     let exp = fromJust (readExpr formula)
     let ps = points exp (read zoomStr) (canWidth, canHeight)
     -- Clear the canvas
     clearCanvas canvas
     -- The following code draws the formula text in the canvas and a blue line.
     -- It should be replaced with code that draws the graph of the function.
     set UI.fillStyle (UI.solidColor (UI.RGB 0 0 0)) (pure canvas)
     UI.fillText formula (10,canHeight/2) canvas
     path "blue" [(canWidth/2,0),(canWidth/2,canHeight)] canvas
     path "blue" [(0,canHeight/2),(canWidth,canHeight/2)] canvas
     path "black" ps canvas
