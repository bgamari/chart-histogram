import Data.Accessor                
import Graphics.Rendering.Chart.Plot.HeatMap
import Graphics.Rendering.Chart.Plot.Histogram
import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Gtk
       
chart = layout
  where layout = layout1_title ^= "Hello World"
               $ layout1_plots ^= [Left (toPlot hi)]
               $ defaultLayout1
        hi = heat_map_values ^= [(x,y,(x+y)/8::Double) | x <- [1..4], y <- [1..4]]
           $ defaultHeatMap
main = renderableToWindow (toRenderable chart) 640 480
