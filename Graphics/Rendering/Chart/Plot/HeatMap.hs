{-# LANGUAGE TemplateHaskell #-}

module Graphics.Rendering.Chart.Plot.HeatMap ( HeatMap
                                             , defaultHeatMap
                                             , heat_map_color_map
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
  , heat_map_values_             :: [((x,y), (x,y), z)]
  }
  
defaultHeatMap :: (PlotValue x, PlotValue y, PlotValue z) => HeatMap z x y
defaultHeatMap = HeatMap
  { heat_map_color_map_ = grey
  , heat_map_values_    = []
  }
  
instance (PlotValue z) => ToPlot (HeatMap z) where
  toPlot p = Plot { plot_render_     = renderHeatMap p
                  , plot_legend_     = []
                  , plot_all_points_ = let p0s = map fst3 $ heat_map_values_ p
                                           p1s = map snd3 $ heat_map_values_ p
                                       in ( map fst p0s ++ map fst p1s
                                          , map snd p0s ++ map snd p1s
                                          )
                  }
                  
renderHeatMap  :: (PlotValue z) =>
                  HeatMap z x y -> PointMapFn x y -> CRender ()
renderHeatMap p pmap = preserveCState $
    forM_ (heat_map_values_ p) $ \((x0,y0), (x1,y1), z) -> c $ do
      setSourceColor $ heat_map_color_map_ p z
      let Point x0' y0' = pmap (LValue x0, LValue y0)
          Point x1' y1' = pmap (LValue x1, LValue y1)
      C.rectangle x0' y0' (x1'-x0') (y1'-y0')
      C.fill
  
$( deriveAccessors ''HeatMap )

