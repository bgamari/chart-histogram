{-# LANGUAGE TemplateHaskell #-}

module Graphics.Rendering.Chart.Plot.HeatMap ( HeatMap
                                             , defaultHeatMap
                                             , heat_map_color_map
                                             , heat_map_cell_size
                                             , heat_map_values
                                             ) where

import qualified Graphics.Rendering.Cairo as C

import Graphics.Rendering.Chart.Types
import Graphics.Rendering.Chart.Plot.Types
import Graphics.Rendering.Chart.Axis
import Graphics.Rendering.Chart.ColorMap
import Data.Accessor.Template
import Data.Colour

import Control.Monad 
       
fst3 (a,_,_) = a
snd3 (_,a,_) = a
thd3 (_,_,a) = a 

data HeatMap z x y = HeatMap
  { heat_map_color_map_          :: z -> AlphaColour Double
  , heat_map_cell_size_          :: (x,y)
  , heat_map_values_             :: [(x,y,z)]
  }
  
defaultHeatMap :: (PlotValue x, PlotValue y, PlotValue z) => HeatMap z x y
defaultHeatMap = HeatMap
  { heat_map_color_map_ = grey
  , heat_map_cell_size_ = (fromValue 1, fromValue 1)
  , heat_map_values_    = []
  }
  
instance (PlotValue z) => ToPlot (HeatMap z) where
  toPlot p = Plot { plot_render_     = renderHeatMap p
                  , plot_legend_     = []
                  , plot_all_points_ = ( map fst3 $ heat_map_values_ p
                                       , map snd3 $ heat_map_values_ p )
                  }
                  
renderHeatMap  :: (PlotValue z) =>
                  HeatMap z x y -> PointMapFn x y -> CRender ()
renderHeatMap p pmap = preserveCState $
    forM_ (heat_map_values_ p) $ \(x,y,z) -> c $ do
      setSourceColor $ heat_map_color_map_ p z
      let Point x' y' = pmap (LValue x, LValue y)
      C.rectangle x' y' 20 20
      C.fill
  where (dx, dy) = heat_map_cell_size_ p
  
$( deriveAccessors ''HeatMap )

