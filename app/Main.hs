import Data.IDX
import Data.Matrix
import Data.Vector
import Data.Vector.Split
import Data.Maybe

imSize = 784

--loadData:: FilePath -> FilePath -> IO (Maybe [(Int, Vector Int)])
loadData file_lab file_dat = do
    m_idx_lab <- decodeIDXLabelsFile file_lab
    m_idx_dat <- decodeIDXFile file_dat
    return (maybe Nothing (\idx_lab -> maybe Nothing (labeledIntData idx_lab) m_idx_dat) m_idx_lab)

main = do
    idxdat <- decodeIDXFile "train-images-idx3-ubyte"
    let train_vector = chunksOf 784 (idxIntContent (fromJust idxdat))
    return (Prelude.head train_vector)
