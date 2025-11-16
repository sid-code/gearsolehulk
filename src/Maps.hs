module Maps where

import Data.Array
import Types

mapWithWalls :: Int -> Int -> Map Tile
mapWithWalls w h =
    MkMap
        w
        h
        ( listArray
            (0, w * h - 1)
            ( replicate w Wall
                ++ concat (replicate (h - 2) ([Wall] ++ replicate (w - 2) Grass ++ [Wall]))
                ++ replicate w Wall
            )
        )
