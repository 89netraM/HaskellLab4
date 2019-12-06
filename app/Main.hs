import Data.IDX
import Data.Matrix
import Data.Vector
import Data.Vector.Split
import Data.Maybe

loadData file_lab file_dat = do
    m_idx_lab <- decodeIDXLabelsFile file_lab
    m_idx_dat <- decodeIDXFile file_dat
    return $ case m_idx_lab of
        Just idx_lab -> case m_idx_dat of
                            Just idx_dat -> labeledIntData idx_lab idx_dat
                            Nothing      -> Nothing
        Nothing      -> Nothing

main = do
    idxdat <- decodeIDXFile "train-images-idx3-ubyte"
    let train_vector = chunksOf 784 (idxIntContent (fromJust idxdat))
    return (Prelude.head train_vector)
