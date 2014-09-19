{-# LANGUAGE TemplateHaskell, FlexibleInstances #-}

module Graphics.Rendering.Chart.Plot.Histogram
  ( -- * Histograms
    PlotHist (..)
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
import Data.Monoid
import Data.List (transpose)
import Data.Maybe (fromMaybe)
import qualified Data.Foldable as F
import qualified Data.Vector as V

import Control.Lens hiding (moveTo)
import Graphics.Rendering.Chart
import Data.Default

import Data.Colour (opaque)
import Data.Colour.Names (black, blue)
import Data.Colour.SRGB (sRGB)

import Numeric.Histogram

data PlotHist x y = PlotHist
    { -- | Plot title
      _plot_hist_title                :: String

      -- | Number of bins
    , _plot_hist_bins                 :: Int

      -- | Values to histogram
    , _plot_hist_values               :: V.Vector x

      -- | Don't attempt to plot bins with zero counts. Useful when
      -- the y-axis is logarithmically scaled.
    , _plot_hist_no_zeros             :: Bool

      -- | Override the range of the histogram. If @Nothing@ the
      -- range of @_plot_hist_values@ is used
    , _plot_hist_range                :: Maybe (x,x)

      -- | Plot vertical lines between bins
    , _plot_hist_drop_lines           :: Bool

      -- | Fill style of the bins
    , _plot_hist_fill_style           :: FillStyle

      -- | Line style of the bin outlines
    , _plot_hist_line_style           :: LineStyle

      -- | Normalization function
    , _plot_hist_norm_func            :: Double -> Int -> y
    }

instance Default (PlotHist x Int) where
    def = defaultPlotHist

-- | The default style is an unnormalized histogram of 20 bins.
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

-- | @defaultPlotHist@ but with real counts
defaultFloatPlotHist :: PlotHist x Double
defaultFloatPlotHist = defaultPlotHist { _plot_hist_norm_func = const realToFrac }

-- | @defaultPlotHist@ but normalized such that the integral of the
-- histogram is one.
defaultNormedPlotHist :: PlotHist x Double
defaultNormedPlotHist = defaultPlotHist { _plot_hist_norm_func = \n y->realToFrac y / n }

defaultFillStyle :: FillStyle
defaultFillStyle = solidFillStyle (opaque $ sRGB 0.5 0.5 1.0)

defaultLineStyle :: LineStyle
defaultLineStyle = (solidLine 1 $ opaque blue)
     { _line_cap  = LineCapButt
     , _line_join = LineJoinMiter
     }

-- | Convert a @PlotHist@ to a @Plot@
histToPlot :: (RealFrac x, Num y, Ord y) => PlotHist x y -> Plot x y
histToPlot p = Plot {
        _plot_render      = renderPlotHist p,
        _plot_legend      = [(_plot_hist_title p, renderPlotLegendHist p)],
        _plot_all_points  = unzip
                            $ concatMap (\((x1,x2), y)->[(x1,y), (x2,y)])
                            $ histToBins p
    }

buildHistPath :: (RealFrac x, Num y)
              => PointMapFn x y -> [((x,x), y)] -> Path
buildHistPath _ [] = End
buildHistPath pmap bins = MoveTo (pt x0 0) (go bins)
    where go [((x1,x2),y)]      = LineTo (pt x1 y)
                                $ LineTo (pt x2 y)
                                $ LineTo (pt x2 0)
                                $ End
          go (((x1,x2),y):rest) = LineTo (pt x1 y)
                                $ LineTo (pt x2 y)
                                $ go rest
          ((x0,_),_) = head bins
          pt x y = pmap (LValue x, LValue y)

renderPlotHist :: (RealFrac x, Num y, Ord y)
               => PlotHist x y -> PointMapFn x y -> ChartBackend ()
renderPlotHist p pmap
    | null bins = return ()
    | otherwise = do
        withFillStyle (_plot_hist_fill_style p) $
            alignFillPath (buildHistPath pmap bins) >>= fillPath
        withLineStyle (_plot_hist_line_style p) $ do
            when (_plot_hist_drop_lines p) $
                alignStrokePath dropLinesPath >>= strokePath
            alignStrokePath (buildHistPath pmap bins) >>= strokePath
    where bins = histToBins p
          pt x y = pmap (LValue x, LValue y)
          dropLinesPath = F.foldMap (\((x1,_), y)->moveTo (pt x1 0)
                                                <> lineTo (pt x1 y)
                                    ) $ tail bins

renderPlotLegendHist :: PlotHist x y -> Rect -> ChartBackend ()
renderPlotLegendHist p r@(Rect p1 p2) =
    withLineStyle (_plot_hist_line_style p) $
        let y = (p_y p1 + p_y p2) / 2
        in strokePath $ moveTo' (p_x p1) y <> lineTo' (p_x p2) y

histToBins :: (RealFrac x, Num y, Ord y) => PlotHist x y -> [((x,x), y)]
histToBins hist =
    filter_zeros $ zip bounds $ counts
    where n = _plot_hist_bins hist
          (a,b) = realHistRange hist
          dx = realToFrac (b-a) / realToFrac n
          bounds = binBounds a b n
          values = _plot_hist_values hist
          filter_zeros | _plot_hist_no_zeros hist  = filter (\(b,c)->c > 0)
                       | otherwise                 = id
          norm = dx * realToFrac (V.length values)
          normalize = _plot_hist_norm_func hist norm
          counts = V.toList $ V.map (normalize . snd)
                   $ histWithBins (V.fromList bounds) (zip (repeat 1) $ V.toList values)

-- TODO: Determine more aesthetically pleasing range
realHistRange :: (RealFrac x) => PlotHist x y -> (x,x)
realHistRange hist = fromMaybe range $ _plot_hist_range hist
    where values = _plot_hist_values hist
          range = if V.null values
                    then (0,0)
                    else (V.minimum values, V.maximum values)

$( makeLenses ''PlotHist )
