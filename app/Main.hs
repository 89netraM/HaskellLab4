import Data.IDX
import Data.Matrix
import qualified Data.Vector.Unboxed as V

train_images = "train-images-idx3-ubyte"
train_labels = "train-labels-idx1-ubyte"
test_images = "t10k-images-idx3-ubyte"
test_labels = "t10k-labels-idx1-ubyte"

loadData :: FilePath -> FilePath -> IO (Maybe [(Int, V.Vector Int)])
loadData file_lab file_dat = do
    m_idx_lab <- decodeIDXLabelsFile file_lab
    m_idx_dat <- decodeIDXFile file_dat
    return $ case m_idx_lab of
        Just idx_lab -> case m_idx_dat of
                            Just idx_dat -> labeledIntData idx_lab idx_dat
                            Nothing      -> Nothing
        Nothing      -> Nothing

-- | Returns a list of all the data as vectors.
getDatas :: [(Int, V.Vector Int)] -> [Matrix Int]
getDatas = map (rowVector . V.convert . snd)

-- | Converts a label (int) to a vector label.
makeLabelVector :: Int -> Matrix Int
makeLabelVector n = fromList 1 10 $ replicate n 0 ++ (1:replicate (9-n) 0)

-- | Returns a list of all the labels as vectors.
getLabels :: [(Int, V.Vector Int)] -> [Matrix Int]
getLabels = map (makeLabelVector . fst)

main = undefined
