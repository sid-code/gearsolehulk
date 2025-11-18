{-# LANGUAGE OverloadedLabels #-}

module Action where

import Control.Exception (throw)
import Control.Monad
import Control.Monad.Coroutine (Coroutine (resume))
import Control.Monad.Coroutine.SuspensionFunctors (Yield (Yield))
import Control.Monad.IO.Class
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Data.Array
import Data.Ecstasy
import Data.Functor
import Data.Maybe (fromMaybe)
import Data.Text (pack, singleton)
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

getAction :: Ent -> ActionPolicy -> Game ValidAction
getAction _ AlwaysWait = pure validWait
getAction ent RandomWalk = do
    r <- gets (view #rng)
    let (d :: Direction, r') = random r
    modify (set #rng r')
    orWait ent (Move d)
getAction ent AskIO = do
    -- TODO: this should use whatever interface is configured
    renderTerminal
    let getNextAction = do
            nextInput <- liftIO $ resume inputCoroutine
            case nextInput of
                Right () -> pure Wait
                Left (Yield ch _) -> do
                    case ch of
                        'q' -> modify (set #quit True) $> Wait
                        'h' -> pure $ Move W
                        'j' -> pure $ Move S
                        'k' -> pure $ Move N
                        'l' -> pure $ Move E
                        _ -> pure Invalid
    let go = getNextAction >>= checkAction ent >>= maybe go pure
    go

newtype ValidAction = MkValidAction Action

-- wait is always valid
validWait :: ValidAction
validWait = MkValidAction Wait

orWait :: Ent -> Action -> Game ValidAction
orWait ent act = checkAction ent act <&> fromMaybe validWait

checkAction :: Ent -> Action -> Game (Maybe ValidAction)
checkAction _ Invalid = pure Nothing
checkAction ent (Move d) = runMaybeT $ do
    let (dx, dy) = toDelta d
    ent' <- lift $ getEntity ent
    MkPosition (x, y) <- hoistMaybe (pos ent')
    let newp = MkPosition (x + dx, y + dy)
    zl <- hoistMaybe (zLevel ent')
    mps :: Array Int (Map Tile) <- gets (view #maps)
    let mp = mps ! zl
        existing = view (posLens newp) mp
    hoistMaybe $ if existing == Wall then Nothing else Just $ MkValidAction $ MoveTo newp
checkAction _ a@(MoveTo _) = pure $ Just $ MkValidAction a -- counterintuitely, these are always valid
checkAction _ a = pure $ Just $ MkValidAction a

performAction :: Ent -> Action -> Game ()
performAction _ Invalid = error "Invalid action"
performAction ent Wait = liftIO $ putStrLn $ show ent ++ " waits."
performAction ent (Move dir) = undefined
performAction ent (MoveTo newp) = move ent newp
performAction ent (Use oent) = undefined

maybePerformActions :: Game ()
maybePerformActions = do
    curTime <- gets (view #time)
    entsAndActions <- efor allEnts (allActionable curTime)
    forM_ entsAndActions $ \(ent, policy) -> do
        MkValidAction act <- getAction ent policy
        performAction ent act

move :: Ent -> Position -> Game ()
move ent p = setEntity ent unchanged{pos = Set p}
