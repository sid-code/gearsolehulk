{-# LANGUAGE OverloadedLabels #-}

module Action where

import Control.Monad
import Control.Monad.Coroutine (Coroutine (resume))
import Control.Monad.Coroutine.SuspensionFunctors (Yield (Yield))
import Control.Monad.IO.Class
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Data.Array
import Data.Ecstasy
import Data.Functor
import Data.Text qualified as T
import Message
import Optics
import Renderer.Terminal (inputCoroutine, renderTerminal)
import System.Random
import Types
import Util

allActionable :: Int -> Query (Ent, ActionPolicy)
allActionable curTime = do
    spd <- query speed
    queryIf $ curTime `mod` spd == 0
    policy <- query actionPolicy
    (,policy) <$> queryEnt

getAction :: Ent -> ActionPolicy -> Game Action
getAction _ AlwaysWait = pure Wait
getAction _ RandomWalk = do
    r <- gets (view #rng)
    let (d :: Direction, r') = random r
    modify (set #rng r')
    pure $ Move d
getAction _ AskIO = do
    -- TODO: this should use whatever interface is configured
    renderTerminal
    nextInput <- liftIO $ resume inputCoroutine
    case nextInput of
        Right () -> pure Wait
        Left (Yield ch _) ->
            case ch of
                'q' -> modify (set #quit True) $> Wait
                'h' -> pure $ Move W
                'j' -> pure $ Move S
                'k' -> pure $ Move N
                'l' -> pure $ Move E
                _ -> pure Wait

performAction :: Ent -> Action -> Game ()
performAction ent Wait = liftIO $ putStrLn $ show ent ++ " waits."
performAction ent (Move dir) = do
    void $ tryMove ent dir
performAction ent (Use oent) = undefined

maybePerformActions :: Game ()
maybePerformActions = do
    curTime <- gets (view #time)
    entsAndActions <- efor allEnts (allActionable curTime)
    forM_ entsAndActions $ \(ent, policy) -> do
        act <- getAction ent policy
        performAction ent act

tryMove :: Ent -> Direction -> Game ()
tryMove ent d = void . runMaybeT $ do
    let (dx, dy) = toDelta d
    ent' <- lift $ getEntity ent
    MkPosition (x, y) <- hoistMaybe (pos ent')
    let newp = MkPosition (x + dx, y + dy)
    zl <- hoistMaybe (zLevel ent')
    mps :: Array Int (Map Tile) <- gets (view #maps)
    let mp = mps ! zl
        existing = view (posLens newp) mp
    if existing == Wall
        then do lift $ sendMessage ent "You run into a wall."
        else lift $ setEntity ent unchanged{pos = Set newp}

move :: Ent -> Position -> Game ()
move ent p = do
    localState <- get
    setEntity ent unchanged{pos = Set p}
