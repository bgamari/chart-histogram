module Main where

import Data.Accessor
import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Gtk
import Graphics.Rendering.Chart.Plot.Histogram

values = [[1,1,2,3, 8,8,8,8]] :: [[Double]]

chart = layout
        where hist = plot_hist_values  ^= values
                     $ plot_hist_range ^= Just (0, 10)
                     $ defaultPlotHist
              layout = layout1_title ^= "Hello World"
                     $ layout1_plots ^= [Left (histToBarsPlot hist)]
                     $ defaultLayout1

main = do renderableToWindow (toRenderable chart) 640 480

