module LoadMNISTdata where

import Data.IDX
import Data.Matrix
import qualified Data.Vector.Unboxed as V

-- | The MNIST training and testing data
train_images = "train-images-idx3-ubyte"
train_labels = "train-labels-idx1-ubyte"
test_images = "t10k-images-idx3-ubyte"
test_labels = "t10k-labels-idx1-ubyte"

-- | Load the data from idx files to vectors.
loadData :: FilePath -> FilePath -> IO (Maybe [(Int, V.Vector Int)])
loadData file_lab file_dat = do
    m_idx_lab <- decodeIDXLabelsFile file_lab
    m_idx_dat <- decodeIDXFile file_dat
    return $ case m_idx_lab of
        Just idx_lab -> case m_idx_dat of
                            Just idx_dat -> labeledIntData idx_lab idx_dat
                            Nothing      -> Nothing
        Nothing      -> Nothing

-- | Converts a label (int) to a vector label.
makeLabelVector :: Int -> Matrix Int
makeLabelVector n = fromList 1 10 $ replicate n 0 ++ (1:replicate (9-n) 0)

-- | Returns a nicer representation of the data.
-- | Pair of (Data as a matrix, Label as a matrix).
makeNiceData :: [(Int, V.Vector Int)] -> [(Matrix Int, Matrix Int)]
makeNiceData = map (\(l, v) -> (rowVector $ V.convert v, makeLabelVector l))
