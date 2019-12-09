module Image where

import qualified Graphics.Image as I
import qualified Data.Vector.Unboxed as V

parseImage :: FilePath -> IO (V.Vector Int)
parseImage path = do
    img <- I.readImageExact' I.PNG path :: IO (I.Image I.VS I.RGB I.Word8)
    let grayScaleList = map pixelToGrayScale $ concat $ I.toLists img
    return $ V.fromList grayScaleList

pixelToGrayScale :: I.Pixel I.RGB I.Word8 -> Int
pixelToGrayScale p = 254 - (word8ToInt $ foldl1 (\a b -> (a `div` 2) + (b `div` 2)) p)

word8ToInt :: I.Word8 -> Int
word8ToInt = fromIntegral . toInteger