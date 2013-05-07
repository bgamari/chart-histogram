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

import Control.Lens
import Graphics.Rendering.Chart.Types
import Graphics.Rendering.Chart.Axis.Types
import Graphics.Rendering.Chart.Plot.Types
import qualified Graphics.Rendering.Cairo as C

import Data.Colour (opaque)
import Data.Colour.Names (black, blue)
import Data.Colour.SRGB (sRGB)

import Numeric.Histogram

data PlotHist x y = PlotHist { _plot_hist_title                :: String
                             , _plot_hist_bins                 :: Int
                             , _plot_hist_values               :: V.Vector x
                             , _plot_hist_no_zeros             :: Bool
                             , _plot_hist_range                :: Maybe (x,x)
                             , _plot_hist_drop_lines           :: Bool
                             , _plot_hist_fill_style           :: CairoFillStyle
                             , _plot_hist_line_style           :: CairoLineStyle
                             , _plot_hist_norm_func            :: Double -> Int -> y
                             }

defaultPlotHist :: PlotHist x Int
defaultPlotHist = PlotHist { _plot_hist_bins        = 20
                           , _plot_hist_title       = ""
                           , _plot_hist_values      = V.empty
                           , _plot_hist_no_zeros    = False
                           , _plot_hist_range       = Nothing
                           , _plot_hist_drop_lines  = False
                           , _plot_hist_line_style  = defaultLineStyle
                           , _plot_hist_fill_style  = defaultFillStyle
                           , _plot_hist_norm_func   = const id
                           }

defaultFloatPlotHist :: PlotHist x Double
defaultFloatPlotHist = defaultPlotHist { _plot_hist_norm_func = const realToFrac }

defaultNormedPlotHist :: PlotHist x Double
defaultNormedPlotHist = defaultPlotHist { _plot_hist_norm_func = \n y->realToFrac y / n }

defaultFillStyle :: CairoFillStyle
defaultFillStyle = solidFillStyle (opaque $ sRGB 0.5 0.5 1.0)

defaultLineStyle :: CairoLineStyle
defaultLineStyle = (solidLine 1 $ opaque blue) {
     _line_cap  = C.LineCapButt,
     _line_join = C.LineJoinMiter
 }

histToPlot :: (RealFrac x, PlotValue y) => PlotHist x y -> Plot x y
histToPlot p = Plot {
        _plot_render      = renderPlotHist p,
        _plot_legend      = [(_plot_hist_title p, renderPlotLegendHist p)],
        _plot_all_points  = unzip
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
        setFillStyle (_plot_hist_fill_style p)
        fillPath $ map (mapXY pmap) $ buildHistPath bins
        setLineStyle (_plot_hist_line_style p)
        when (_plot_hist_drop_lines p) $
            mapM_ (\((x1,x2), y)->drawLines (mapXY pmap) [(x1,fromValue 0), (x1,y)])
            $ tail bins
        drawLines (mapXY pmap) $ buildHistPath bins
    where drawLines mapfn pts = strokePath (map mapfn pts)
          bins = histToBins p

renderPlotLegendHist :: PlotHist x y -> Rect -> CRender ()
renderPlotLegendHist p r@(Rect p1 p2) = preserveCState $ do
    setLineStyle (_plot_hist_line_style p)
    let y = (p_y p1 + p_y p2) / 2
    strokePath [Point (p_x p1) y, Point (p_x p2) y]

histToBins :: (RealFrac x, PlotValue y) => PlotHist x y -> [((x,x), y)]
histToBins hist =
    filter_zeros $ zip bounds $ counts
    where n = _plot_hist_bins hist
          (a,b) = realHistRange hist
          dx = realToFrac (b-a) / realToFrac n
          bounds = binBounds a b n
          values = _plot_hist_values hist
          filter_zeros | _plot_hist_no_zeros hist  = filter (\(b,c)->c>fromValue 0)
                       | otherwise                 = id
          norm = dx * realToFrac (V.length values)
          normalize = _plot_hist_norm_func hist $ norm
          counts = V.toList $ V.map (normalize . snd)
                   $ histWithBins (V.fromList bounds) (zip (repeat 1) $ V.toList values)

-- TODO: Determine more aesthetically pleasing range
realHistRange :: (RealFrac x) => PlotHist x y -> (x,x)
realHistRange hist = maybe (dmin,dmax) id $ _plot_hist_range hist
    where values = _plot_hist_values hist
          dmin = V.minimum values
          dmax = V.maximum values

$( makeLenses ''PlotHist )
