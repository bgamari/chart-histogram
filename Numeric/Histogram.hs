module Numeric.Histogram ( Range
                         , binBounds
                         , histValues
                         , histWithBins
                         ) where

import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import Control.Monad.ST
import Control.Monad (foldM)

type Range a = (a,a)

-- | 'binBounds a b n' generates bounds for 'n' bins spaced linearly between
-- 'a' and 'b'
binBounds :: RealFrac a => a -> a -> Int -> [Range a]
binBounds a b n = map (\i->(lbound i, lbound (i+1))) [0..n]
        where lbound i = a + (b-a) * realToFrac i / realToFrac n

-- | 'histValues a b n vs' returns the histogram values for the histogram of
-- 'vs' on the range from 'a' to 'b' with 'n' bins
histValues :: RealFrac a => a -> a -> Int -> [a] -> [(Range a, Int)]
histValues a b n = histWithBins (binBounds a b n)

histWithBins :: RealFrac a => [Range a] -> [a] -> [(Range a, Int)]
histWithBins bins xs =
        let testBin :: RealFrac a => a -> Range a -> Bool
            testBin x (a,b) = x >= a && x < b

            --f :: RealFrac a => MV.STVector () Int -> a -> ST () (MV.STVector () Int)
            f bs x = do case dropWhile (not . testBin x . snd) $ zip [0..] bins of
                             [] -> return bs
                             (idx,bounds):_  -> do n <- MV.read bs idx
                                                   MV.write bs idx (n+1)
                                                   return bs

            counts = runST $ do b <- MV.replicate (length bins) 0
                                b' <- foldM f b xs
                                (return . V.toList) =<< V.freeze b'
        in zip bins counts
