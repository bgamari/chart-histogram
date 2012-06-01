{-# LANGUAGE TemplateHaskell #-}

module Graphics.Rendering.Chart.Plot.Histogram ( -- * Histograms
                                                 PlotHist (..)
                                               , defaultPlotHist
                                                 -- * Bar plots
                                               , histToBarsPlot
                                               , histToFloatBarsPlot
                                               , histToNormedBarsPlot
                                                 -- * Accessors
                                               , plot_hist_item_styles
                                               , plot_hist_bins
                                               , plot_hist_values
                                               , plot_hist_range
                                               ) where

import Data.Accessor.Template
import Numeric.Histogram
import Data.List (transpose)
import Graphics.Rendering.Chart.Types
import Graphics.Rendering.Chart.Axis.Types
import Graphics.Rendering.Chart.Plot.Types
import Graphics.Rendering.Chart.Plot.Bars

data PlotHist x = PlotHist { plot_hist_item_styles_          :: [ (CairoFillStyle, Maybe CairoLineStyle) ]
                           , plot_hist_bins_                 :: Int
                           , plot_hist_values_               :: [[x]]
                           , plot_hist_range_                :: Maybe (x,x)
                           }

defaultPlotHist :: PlotHist x
defaultPlotHist = PlotHist { plot_hist_item_styles_ = plot_bars_item_styles_ (defaultPlotBars :: PlotBars x Int)
                           , plot_hist_range_ = Nothing
                           , plot_hist_bins_ = 20
                           , plot_hist_values_ = []
                           }
        
histToBins :: (RealFrac x, PlotValue a) => (Double -> Int -> a) -> PlotHist x -> [((x,x), [a])]
histToBins normalizeFunc hist = zip bounds (transpose counts)
    where n = plot_hist_bins_ hist
          (a,b) = realHistRange hist
          dx = realToFrac (b-a) / realToFrac n
          bounds = binBounds a b n
          counts = map (\xs->map (normalizeFunc (dx*realToFrac (length xs)) . snd)
                             $ histWithBins bounds xs
                       ) $ plot_hist_values_ hist

realHistRange :: (RealFrac x) => PlotHist x -> (x,x)
realHistRange hist = maybe (dmin,dmax) id $ plot_hist_range_ hist
    where values = plot_hist_values_ hist
          dmin = minimum $ map minimum values
          dmax = maximum $ map maximum values

histToBars :: (RealFrac x, BarsPlotValue a) => (Double -> Int -> a) -> PlotHist x -> PlotBars x a
histToBars normalizeFunc hist =
    defaultPlotBars { plot_bars_item_styles_ = plot_hist_item_styles_ hist
                    , plot_bars_values_ = map (\((a,b),c)->(a,c))
                                          $ histToBins normalizeFunc hist
                    , plot_bars_spacing_ = BarsFixGap 0 0
                    , plot_bars_alignment_ = BarsLeft
                    , plot_bars_style_ = BarsStacked
                    }

-- | Produce a bar plot from a histogram with counts along the Y axis
histToBarsPlot :: RealFrac x => PlotHist x -> Plot x Int
histToBarsPlot = plotBars . histToBars (const id)

-- | Produce a bar plot from a histogram with counts along the Y axis
histToFloatBarsPlot :: RealFrac x => PlotHist x -> Plot x Double
histToFloatBarsPlot = plotBars . histToBars (const realToFrac)

-- | Produce a bar plot from a histogram with normalized probability
-- density along the Y axis
histToNormedBarsPlot :: RealFrac x => PlotHist x -> Plot x Double
histToNormedBarsPlot = plotBars . histToBars (\norm n->realToFrac n / norm)

$( deriveAccessors ''PlotHist )
  
