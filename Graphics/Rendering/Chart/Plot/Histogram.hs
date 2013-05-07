{-# LANGUAGE TemplateHaskell #-}

module Graphics.Rendering.Chart.Plot.Histogram ( -- * Histograms
                                                 PlotHist
                                               , histToPlot
                                               , defaultPlotHist
                                               , defaultFloatPlotHist
                                               , defaultNormedPlotHist
                                                 -- * Accessors
                                               , plot_hist_title
                                               , plot_hist_bins
                                               , plot_hist_values
                                               , plot_hist_no_zeros
                                               , plot_hist_range
                                               , plot_hist_drop_lines
                                               , plot_hist_line_style
                                               , plot_hist_fill_style
                                               , plot_hist_norm_func
                                               ) where

import Control.Monad (when)       
import Data.List (transpose)
import qualified Data.Vector as V

import Data.Accessor.Template
import Graphics.Rendering.Chart.Types
import Graphics.Rendering.Chart.Axis.Types
import Graphics.Rendering.Chart.Plot.Types
import qualified Graphics.Rendering.Cairo as C

import Data.Colour (opaque)
import Data.Colour.Names (black, blue)
import Data.Colour.SRGB (sRGB)

import Numeric.Histogram

data PlotHist x y = PlotHist { plot_hist_title_                :: String
                             , plot_hist_bins_                 :: Int
                             , plot_hist_values_               :: [x]
                             , plot_hist_no_zeros_             :: Bool
                             , plot_hist_range_                :: Maybe (x,x)
                             , plot_hist_drop_lines_           :: Bool
                             , plot_hist_fill_style_           :: CairoFillStyle
                             , plot_hist_line_style_           :: CairoLineStyle
                             , plot_hist_norm_func_            :: Double -> Int -> y
                             }

defaultPlotHist :: PlotHist x Int
defaultPlotHist = PlotHist { plot_hist_bins_        = 20
                           , plot_hist_title_       = ""
                           , plot_hist_values_      = []
                           , plot_hist_no_zeros_    = False
                           , plot_hist_range_       = Nothing
                           , plot_hist_drop_lines_  = False
                           , plot_hist_line_style_  = defaultLineStyle
                           , plot_hist_fill_style_  = defaultFillStyle
                           , plot_hist_norm_func_   = const id
                           }

defaultFloatPlotHist :: PlotHist x Double
defaultFloatPlotHist = defaultPlotHist { plot_hist_norm_func_ = const realToFrac }

defaultNormedPlotHist :: PlotHist x Double
defaultNormedPlotHist = defaultPlotHist { plot_hist_norm_func_ = \n y->realToFrac y / n }
        
defaultFillStyle :: CairoFillStyle
defaultFillStyle = solidFillStyle (opaque $ sRGB 0.5 0.5 1.0)

defaultLineStyle :: CairoLineStyle
defaultLineStyle = (solidLine 1 $ opaque blue) {
     line_cap_  = C.LineCapButt,
     line_join_ = C.LineJoinMiter
 }

histToPlot :: (RealFrac x, PlotValue y) => PlotHist x y -> Plot x y
histToPlot p = Plot {
        plot_render_      = renderPlotHist p,
        plot_legend_      = [(plot_hist_title_ p, renderPlotLegendHist p)],
        plot_all_points_  = unzip
                            $ concatMap (\((x1,x2), y)->[(x1,y), (x2,y)])
                            $ histToBins p
    }

buildHistPath :: (RealFrac x, PlotValue y) => [((x,x), y)] -> [(x,y)]
buildHistPath [] = []
buildHistPath bins = (x1,fromValue 0):f bins
    where ((x1,_),_) = head bins
          f (((x1,x2),y):[])    = [(x1,y), (x2,y), (x2,fromValue 0)]
          f (((x1,x2),y):bins)  = (x1,y):(x2,y):f bins
    
renderPlotHist :: (RealFrac x, PlotValue y) => PlotHist x y -> PointMapFn x y -> CRender ()
renderPlotHist p pmap 
    | null bins = return ()
    | otherwise = preserveCState $ do
        setFillStyle (plot_hist_fill_style_ p)
        fillPath $ map (mapXY pmap) $ buildHistPath bins
        setLineStyle (plot_hist_line_style_ p)
        when (plot_hist_drop_lines_ p) $
            mapM_ (\((x1,x2), y)->drawLines (mapXY pmap) [(x1,fromValue 0), (x1,y)])
            $ tail bins
        drawLines (mapXY pmap) $ buildHistPath bins
    where drawLines mapfn pts = strokePath (map mapfn pts)
          bins = histToBins p

renderPlotLegendHist :: PlotHist x y -> Rect -> CRender ()
renderPlotLegendHist p r@(Rect p1 p2) = preserveCState $ do
    setLineStyle (plot_hist_line_style_ p)
    let y = (p_y p1 + p_y p2) / 2
    strokePath [Point (p_x p1) y, Point (p_x p2) y]

histToBins :: (RealFrac x, PlotValue y) => PlotHist x y -> [((x,x), y)]
histToBins hist =
    filter_zeros $ zip bounds $ counts
    where n = plot_hist_bins_ hist
          (a,b) = realHistRange hist
          dx = realToFrac (b-a) / realToFrac n
          bounds = binBounds a b n
          values = plot_hist_values_ hist
          filter_zeros | plot_hist_no_zeros_ hist  = filter (\(b,c)->c>fromValue 0)
                       | otherwise                 = id
          norm = dx * realToFrac (length values)
          normalize = plot_hist_norm_func_ hist $ norm
          counts = map (normalize . snd)
                   $ histWithBins (V.fromList bounds) (zip (repeat 1) values)

-- TODO: Determine more aesthetically pleasing range
realHistRange :: (RealFrac x) => PlotHist x y -> (x,x)
realHistRange hist = maybe (dmin,dmax) id $ plot_hist_range_ hist
    where values = plot_hist_values_ hist
          dmin = minimum values
          dmax = maximum values

$( deriveAccessors ''PlotHist )
  
