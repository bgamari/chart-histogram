{-# LANGUAGE TemplateHaskell #-}

module Graphics.Rendering.Chart.Plot.Histogram ( PlotHist (..)
                                               , defaultPlotHist
                                               , plotHist
                                               ) where

import Data.Accessor.Template
import Numeric.Histogram
import Data.List (transpose)
import Graphics.Rendering.Chart.Types
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
        
histToBars :: RealFrac x => PlotHist x -> PlotBars x Int
histToBars hist =
        defaultPlotBars { plot_bars_item_styles_ = plot_hist_item_styles_ hist
                        , plot_bars_values_ = values'
                        , plot_bars_spacing_ = BarsFixGap 0 0
                        , plot_bars_alignment_ = BarsLeft
                        , plot_bars_style_ = BarsStacked
                        }
        where values = plot_hist_values_ hist
              dmin = minimum $ map minimum values
              dmax = maximum $ map maximum values
              (a,b) = maybe (dmin,dmax) id $ plot_hist_range_ hist
              n = plot_hist_bins_ hist
              bounds = binBounds a b n
              counts = map (map snd . histValues a b n) values
              values' = zip (map fst bounds) (transpose counts)

plotHist :: RealFrac x => PlotHist x -> Plot x Int
plotHist = plotBars . histToBars

$( deriveAccessors ''PlotHist )
  
