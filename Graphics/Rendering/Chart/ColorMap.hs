module Graphics.Rendering.Chart.ColorMap ( ColorMap
                                         , grey
                                         ) where

import Graphics.Rendering.Chart.Axis.Types       
import Data.Colour
import Data.Colour.RGBSpace
import Data.Colour.RGBSpace.HSL
import Data.Colour.SRGB
       
type ColorMap z = z -> AlphaColour Double

grey :: PlotValue z => ColorMap z
grey = opaque . uncurryRGB (rgbUsingSpace sRGBSpace) . hsl 0 0 . toValue


