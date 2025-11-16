module FieldOfView where

import Data.Ecstasy
import Data.Map qualified as M
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Set qualified as S
import Optics
import Types

emptyFOV :: ViewField =
    MkViewField{position = MkPosition (0, 0), visible = M.empty, timeStationary = 0}
updateFOV :: Map Tile -> M.Map Position (S.Set Ent) -> Position -> ViewField -> ViewField
updateFOV mp entpos p (MkViewField{position, visible, timeStationary}) =
    MkViewField
        { position = p
        , visible = M.map (\x -> x{isMemory = True}) visible <> scanCircular p timeStationary
        , timeStationary = if position == p then timeStationary + 1 else 0
        }
  where
    scanCircular (MkPosition (x, y)) maxview = M.fromList . mapMaybe mkViewTile $ [(x + dx, y + dy) | dx <- [-maxview .. maxview], dy <- [-maxview .. maxview]]
    mkViewTile (x, y) =
        if x < 0 || x >= width mp || y < 0 || y >= height mp
            then Nothing
            else
                Just
                    ( MkPosition (x, y)
                    , MkViewTile
                        { entities = S.toList (M.findWithDefault S.empty p entpos)
                        , tile = mp `mapAccess` (x, y)
                        , isMemory = False
                        , lightLevel = 1
                        }
                    )

    probe mp entpos p =
        MkViewTile
            { entities = maybe [] S.toList (M.lookup p entpos)
            , tile = mp ^. posLens p
            , isMemory = False
            , lightLevel = 1
            }
