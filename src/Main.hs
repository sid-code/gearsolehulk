{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Action (maybePerformActions)
import Control.Monad.Loops (whileM_)
import Control.Monad.Reader (MonadIO (liftIO), ReaderT (runReaderT))
import Data.Array (array)
import Data.Colour.SRGB (sRGB24)
import Data.Ecstasy (
    createEntity,
    newEntity,
    runSystemT,
 )
import Data.IORef (newIORef)
import Data.IntMap qualified as IM
import Data.Map qualified as M
import Debug (debugInit)
import Items (Trash (MkTrash))
import Maps (mapWithWalls)
import Optics (over, set, view)
import Renderer.Terminal (
    setupTerminal,
    teardownTerminal,
 )
import Storage (storage)
import System.Random (mkStdGen)
import Types

step :: Game ()
step = do
    modify (over #time (+ 1))
    maybePerformActions

main :: IO ()
main = do
    debugInit
    let
        systemRun = runSystemT storage $ do
            pid <-
                createEntity
                    newEntity
                        { pos = Just (MkPosition (1, 1))
                        , zLevel = Just 0
                        , isPlayer = Just ()
                        , speed = Just 100
                        , messages = Just []
                        , display = Just MkEntityDisplay{symbol = '@', color = sRGB24 120 0 120}
                        , actionPolicy = Just AskIO
                        }
            _thingy <-
                createEntity
                    newEntity
                        { pos = Just (MkPosition (5, 5))
                        , zLevel = Just 0
                        , speed = Just 100
                        , actionPolicy = Just AlwaysWait
                        , display = Just MkEntityDisplay{symbol = '?', color = sRGB24 0 240 120}
                        }

            modify (set #player $ Just pid)

            liftIO setupTerminal
            whileM_ (not . view #quit <$> get) $ do
                step
            liftIO teardownTerminal

        initialState =
            MkLocalState
                { quit = False
                , maps = array (0, 0) [(0, mapWithWalls 20 10)]
                , lsEntityPositions = M.empty
                , lsZLevelEntities = IM.empty
                , time = 0
                , player = Nothing
                , rng = mkStdGen 123
                , viewfield = Nothing
                }
    stateReference <- newIORef initialState
    runReaderT systemRun stateReference
