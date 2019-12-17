import Data.IDX
import Data.Matrix
import qualified Data.Vector.Unboxed as V
import Network
import Matrix
import LoadMNISTdata
import Test
import Test.QuickCheck

-- | Main function running the training, and testing
main = undefined

labDat = do
    (Just raw) <- loadData train_labels train_images
    return $ makeNiceData raw

netw = network 45678 [784, 10]

smallData = do h <- labDat
               return $ take 10 h

getFirstPatternResult net = do small <- smallData
                               return (getResult net (fst (head small)))
