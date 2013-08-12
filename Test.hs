module Main where

import Control.Lens
import Data.Default
import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Gtk
import Graphics.Rendering.Chart.Plot.Histogram
import qualified Data.Vector as V

values = V.fromList [1,1,2,3,8,8,8,8,10] :: V.Vector Double

chart = layout
        where hist = plot_hist_values  .~ values
                     $ plot_hist_range .~ Just (0, 10)
                     $ plot_hist_bins  .~ 10
                     $ plot_hist_drop_lines .~ True
                     $ defaultPlotHist
              layout :: Layout1 Double Int
              layout = layout1_title .~ "Hello World"
                     $ layout1_plots .~ [ Left (histToPlot hist) ]
                     $ def

main = renderableToWindow (toRenderable chart) 640 480
