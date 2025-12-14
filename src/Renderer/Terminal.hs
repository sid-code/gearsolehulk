{-# LANGUAGE OverloadedLabels #-}

module Renderer.Terminal where

import Control.Monad (forM_, forever)
import Control.Monad.Coroutine (Coroutine)
import Control.Monad.Coroutine.SuspensionFunctors (Yield, yield)
import Control.Monad.Extra (mapMaybeM)
import Control.Monad.Reader (MonadIO (liftIO))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT (runMaybeT), hoistMaybe)
import Data.Array ((!))
import Data.ByteString.Builder (Builder, charUtf8, hPutBuilder, stringUtf8)
import Data.Colour (ColourOps (darken))
import Data.Ecstasy
import Data.Foldable (for_)
import Data.Functor ((<&>))
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.Set qualified as S
import Data.Text (pack)
import Data.Text.IO qualified as T
import Data.Time (diffUTCTime, getCurrentTime)
import Debug (debugLn)
import Optics (view, (^.))
import System.Console.ANSI (ConsoleLayer (Foreground), SGR (Reset, SetRGBColor), clearScreen, hideCursor, setCursorPosition, setSGR, setSGRCode, showCursor)
import System.IO (BufferMode (NoBuffering), hFlush, hReady, hSetBuffering, hSetEcho, stdin, stdout)
import Types

getKey :: IO [Char]
getKey = reverse <$> getKey' ""
  where
    getKey' chars = do
        char <- getChar
        more <- hReady stdin
        (if more then getKey' else return) (char : chars)

inputCoroutine :: Coroutine (Yield Char) IO ()
inputCoroutine =
    forever $ do
        x <- liftIO getChar
        yield x

drawTile :: Tile -> LightLevel -> Builder
drawTile t light =
    stringUtf8
        ( setSGRCode
            [ SetRGBColor Foreground (darken light (tileColor t))
            ]
        )
        <> charUtf8 (tileSymbol t)

drawMap :: Map Tile -> Builder
drawMap mp = mconcat $ map buildRow [0 .. height mp - 1]
  where
    buildRow y =
        mconcat [drawTile (view (posLens $ MkPosition (x, y)) mp) 1 | x <- [0 .. width mp - 1]]
            <> stringUtf8 "\n"

setupTerminal :: IO ()
setupTerminal = do
    hideCursor
    hSetBuffering stdin NoBuffering
    hSetEcho stdin False

teardownTerminal :: IO ()
teardownTerminal = showCursor

selectEntityToDraw :: S.Set Ent -> Ent -> Game (Maybe Ent)
selectEntityToDraw entitiesOnTile player
    | S.member player entitiesOnTile = pure $ Just player
    | otherwise = do
        let entitiesList = S.toList entitiesOnTile
        mobs <- efor (someEnts entitiesList) $ do
            _ <- query isMobile
            queryEnt
        pure $ case (mobs, entitiesList) of
            (m : _, _) -> Just m
            (_, e : _) -> Just e
            ([], []) -> Nothing

renderTerminal :: Game ()
renderTerminal = do
    now <- liftIO getCurrentTime
    result <- runMaybeT $ do
        ls <- get
        pid <- hoistMaybe $ ls ^. #player
        pent <- lift $ getEntity pid

        let zlev = fromMaybe 0 (zLevel pent)
        let mps = view #maps ls
        let mp = mps ! zlev
        let ents = view #lsEntityPositions ls
        let msgs = fromMaybe [] $ messages pent

        liftIO $ do
            clearScreen
            setCursorPosition 0 0
            hPutBuilder stdout $ drawMap mp
            setSGR [Reset]

        symsToDraw <- lift $
            flip mapMaybeM (M.toList ents) $
                \(MkPosition (x, y), es) -> do
                    let mkTileDesc MkEntityDisplay{symbol, color} = (x, y, symbol, color)
                        showEnt e = getEntity e <&> fmap mkTileDesc . display
                    entToDraw <- selectEntityToDraw es pid
                    maybe (pure Nothing) showEnt entToDraw

        liftIO $ forM_ symsToDraw $ \(x, y, symbol, color) -> do
            setCursorPosition y x
            setSGR [SetRGBColor Foreground color]
            putChar symbol

        liftIO $ do
            setCursorPosition (height mp) 0
            setSGR [Reset]
            for_ msgs T.putStrLn
            hFlush stdout
    nower <- liftIO getCurrentTime
    liftIO $ debugLn $ "Render took " <> pack (show (diffUTCTime nower now))
    pure $ fromMaybe () result
