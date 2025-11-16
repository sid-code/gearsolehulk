{-# LANGUAGE OverloadedLabels #-}

module Renderer.Terminal where

import Control.Monad (forM, forM_, forever)
import Control.Monad.Coroutine (Coroutine)
import Control.Monad.Coroutine.SuspensionFunctors (Yield, yield)
import Control.Monad.Reader (MonadIO (liftIO))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT (runMaybeT), hoistMaybe)
import Data.Array ((!))
import Data.ByteString.Builder (Builder, charUtf8, hPutBuilder, stringUtf8)
import Data.Colour (ColourOps (darken))
import Data.Ecstasy (
    getEntity,
 )
import Data.Foldable (for_)
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.Set qualified as S
import Data.Text (pack)
import Data.Text.IO qualified as T
import Data.Time (diffUTCTime, getCurrentTime)
import Debug (debugLn)
import Optics (set, view, (^.))
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
drawTile t light = stringUtf8 (setSGRCode [SetRGBColor Foreground (darken light (tileColor t))]) <> charUtf8 (tileSymbol t)

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

        symsToDraw <- forM (concatMap (\(k, v) -> map (k,) (S.toList v)) (M.toList ents)) $
            \(MkPosition (x, y), e) -> do
                entityData <- lift $ getEntity e
                MkEntityDisplay{symbol, color} <- hoistMaybe $ display entityData
                pure (x, y, symbol, color)

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
