{-# LANGUAGE OverloadedLabels #-}

module Storage where

import Data.Ecstasy (
    Ent,
    StorageType (WorldOf),
    Update (Keep, Set, Unset),
    VTable (..),
    defStorage,
 )
import Data.IntMap qualified as IM
import Data.Map qualified as M
import Data.Set (delete, insert, member, singleton)
import Optics (over)
import Types
import Util

vgetPos :: Ent -> Underlying (Maybe Position)
vgetPos ent = gets (findKeyWith (const $ member ent) . lsEntityPositions)

vsetPos :: Ent -> Update Position -> Underlying ()
vsetPos ent upd = do
    curPos <- vgetPos ent
    modify $ over #lsEntityPositions (doUpdate curPos upd)
  where
    doUpdate curPos (Set newPos) = M.alter (Just . maybe (singleton ent) (insert ent)) newPos . doUpdate curPos Unset
    doUpdate curPos Unset = maybe id (M.update (Just . delete ent)) curPos
    doUpdate _ Keep = id

vgetZlevel :: Ent -> Underlying (Maybe Int)
vgetZlevel ent = do
    zLevelEnts <- gets lsZLevelEntities
    pure $ findBy (const $ member ent) zLevelEnts

vsetZlevel :: Ent -> Update Int -> Underlying ()
vsetZlevel ent upd = do
    currentz <- vgetZlevel ent
    modify $ over #lsZLevelEntities (doUpdate currentz upd)
  where
    doUpdate curz (Set newz) = IM.alter (Just . maybe (singleton ent) (insert ent)) newz . doUpdate curz Unset
    doUpdate curz Unset = maybe id (IM.update (Just . delete ent)) curz
    doUpdate _ Keep = id

storage :: EntWorld (WorldOf Underlying)
storage =
    (defStorage :: EntWorld (WorldOf Underlying))
        { pos = VTable vgetPos vsetPos
        , zLevel = VTable vgetZlevel vsetZlevel
        }
