module Util where

import Control.Monad (MonadPlus (mzero))
import Control.Monad.Reader (ReaderT (ReaderT))
import Data.Ecstasy.Types
import Data.IntMap (IntMap, keys, (!))
import Data.Map qualified as M
import Types

findBy :: (Int -> t -> Bool) -> IntMap t -> Maybe Int
findBy cond intmap = go (keys intmap)
  where
    go [] = Nothing
    go (k : ks) =
        if cond k (intmap ! k)
            then Just k
            else go ks

findKeyWith :: (k -> v -> Bool) -> M.Map k v -> Maybe k
findKeyWith cond =
    M.foldrWithKey
        ( \k v ->
            \case
                Just x -> Just x
                Nothing -> if cond k v then Just k else Nothing
        )
        Nothing

queryIf :: Bool -> Query ()
queryIf x = if x then QueryT $ ReaderT $ const $ pure () else mzero
