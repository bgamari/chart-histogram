module Main where

import Data.Accessor
import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Gtk
import Graphics.Rendering.Chart.Plot.Histogram

values = [[1,1,2,3, 8,8,8,8]] :: [[Double]]

chart = layout
        where hist = defaultPlotHist { plot_hist_values_ = values
                                     , plot_hist_range_ = Just (0, 10)
                                     }
              layout = layout1_title ^= "Hello World"
                     $ layout1_plots ^= [Left (plotHist hist)]
                     $ defaultLayout1

main = do renderableToWindow (toRenderable chart) 640 480

