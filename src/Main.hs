{-# LANGUAGE OverloadedRecordDot #-}

module Main (main) where

import Control.Monad.Reader (MonadIO (liftIO), MonadReader (ask), ReaderT (runReaderT))
import Data.Array
import Data.ByteString (ByteString)
import Data.ByteString.Builder (Builder, charUtf8, hPutBuilder, stringUtf8)
import Data.Colour (Colour)
import Data.Colour.RGBSpace (RGB (RGB))
import Data.Colour.SRGB (sRGB24)
import Data.Ecstasy (
    Component,
    ComponentType (Field, Virtual),
    Ent,
    Generic,
    QueryT,
    StorageType (FieldOf, WorldOf),
    SystemT,
    Update (Keep, Set, Unset),
    VTable (..),
    allEnts,
    createEntity,
    defStorage,
    emap,
    getEntity,
    newEntity,
    query,
    runSystemT,
    setEntity,
    unchanged,
 )
import Data.Foldable (for_)
import Data.IORef (IORef, modifyIORef, newIORef, readIORef)
import Data.Map.Strict qualified as SM
import Data.Set (Set, delete, insert, member)
import Data.Text (Text)
import System.Console.ANSI (Color (Green, Magenta, White), ColorIntensity (Vivid), ConsoleIntensity (NormalIntensity), ConsoleLayer (Foreground), SGR (SetColor, SetRGBColor), setSGRCode)
import System.IO (stdout)

data Position = MkPosition
    { x :: Int
    , y :: Int
    }
    deriving stock (Eq, Show)

type Flag f = Component f 'Field ()
type Field f a = Component f 'Field a

data EntWorld f = Entity
    { pos :: Component f 'Virtual Position
    , zLevel :: Component f 'Virtual Int
    , description :: Field f Text
    , isPlayer :: Flag f
    }
    deriving stock (Generic)

data Tile = Dirt | Stone | Grass | Wall
    deriving stock (Show)

data Map d = MkMap
    { width :: Int
    , height :: Int
    , tiles :: Array Int d
    }
    deriving stock (Show)

emptyMap :: Int -> Int -> Map Tile
emptyMap w h = MkMap w h (listArray (0, w * h - 1) (repeat Wall))

instance Functor Map where
    fmap :: forall a b. (a -> b) -> Map a -> Map b
    fmap f m = m{tiles = fmap f t}
      where
        t = tiles m

class Get c m where
    type GetVal m
    access :: m -> c -> GetVal m

instance Get (Int, Int) (Map d) where
    type GetVal (Map d) = d
    access :: Map d -> (Int, Int) -> d
    access MkMap{width, tiles} (x, y) =
        tiles ! (y * width + x)

class Topology c m where
    neighbors :: m -> c -> [c]

instance Topology (Int, Int) (Map d) where
    neighbors :: Map d -> (Int, Int) -> [(Int, Int)]
    neighbors mp (x, y) =
        let xp = x > 0
            yp = y > 0
            xl = x < width mp - 1
            yl = y < height mp - 1
         in [(-1, 0) | xp]
                ++ [(-1, -1) | xp && yp]
                ++ [(0, -1) | yp]
                ++ [(1, 0) | xl]
                ++ [(1, 1) | xl && yl]
                ++ [(0, 1) | yl]
                ++ [(-1, 1) | xp && yl]
                ++ [(1, -1) | yp && xl]

type Underlying = Control.Monad.Reader.ReaderT (IORef LocalState) IO

data LocalState = MkLocalState
    { lsMaps :: !(Array Int (Map Tile))
    , lsEntityPositions :: SM.Map Ent Position
    , lsZLevelEntities :: SM.Map Int (Set Ent)
    , lsCamera :: Position
    , lsTime :: Int
    }
    deriving stock (Show)

get ::
    (Control.Monad.Reader.MonadIO m, Control.Monad.Reader.MonadReader (IORef LocalState) m) =>
    m LocalState
get = gets id

gets ::
    (Control.Monad.Reader.MonadIO m, Control.Monad.Reader.MonadReader (IORef LocalState) m) =>
    (LocalState -> a) ->
    m a
gets f = do
    ref <- Control.Monad.Reader.ask
    fmap f . Control.Monad.Reader.liftIO $ readIORef ref

modify ::
    (Control.Monad.Reader.MonadIO m, Control.Monad.Reader.MonadReader (IORef LocalState) m) =>
    (LocalState -> LocalState) ->
    m ()
modify f = do
    ref <- Control.Monad.Reader.ask
    Control.Monad.Reader.liftIO $ modifyIORef ref f

type Game = SystemT EntWorld Underlying

type Entity = EntWorld 'FieldOf

type Query = QueryT EntWorld Underlying

data RawMoveResult = RawMoveSuccess | RawMoveInvalid
move :: Ent -> Position -> Game RawMoveResult
move ent p@(MkPosition{}) = do
    localState <- get
    setEntity ent unchanged{pos = Set p}
    pure RawMoveSuccess

tick :: Game ()
tick = emap allEnts $ do
    position <- query pos
    Control.Monad.Reader.liftIO $ print position
    pure unchanged

vgetPos :: Ent -> Underlying (Maybe Position)
vgetPos ent = do
    poses <- gets lsEntityPositions
    pure $ SM.lookup ent poses

vsetPos :: Ent -> Update Position -> Underlying ()
vsetPos ent upd =
    modify $ \ls@MkLocalState{lsEntityPositions} -> do
        ls
            { lsEntityPositions = doUpdate upd lsEntityPositions
            }
  where
    doUpdate (Set newpos) = SM.insert ent newpos . SM.delete ent
    doUpdate Unset = SM.delete ent
    doUpdate Keep = id

findKeyWith :: (k -> v -> Bool) -> SM.Map k v -> Maybe k
findKeyWith cond =
    SM.foldrWithKey
        ( \k v ->
            \case
                Just x -> Just x
                Nothing -> if cond k v then Just k else Nothing
        )
        Nothing

vgetZlevel :: Ent -> Underlying (Maybe Int)
vgetZlevel ent = do
    zLevelEnts <- gets lsZLevelEntities
    pure $ findKeyWith (const $ member ent) zLevelEnts

vsetZlevel :: Ent -> Update Int -> Underlying ()
vsetZlevel ent upd = do
    currentz <- vgetZlevel ent
    modify $ \ls@MkLocalState{lsZLevelEntities} -> do
        ls{lsZLevelEntities = doUpdate currentz upd lsZLevelEntities}
  where
    doUpdate curz (Set newz) = SM.update (Just . insert ent) newz . doUpdate curz Unset
    doUpdate curz Unset = maybe id (SM.update (Just . delete ent)) curz
    doUpdate _ Keep = id

tileColor :: Tile -> Colour Float
tileColor Grass = sRGB24 0 0 0
tileColor Stone = sRGB24 0 0 0
tileColor Dirt = sRGB24 0 0 0
tileColor Wall = sRGB24 0 0 0

tileSymbol :: Tile -> Char
tileSymbol Grass = ','
tileSymbol Stone = '.'
tileSymbol Dirt = ','
tileSymbol Wall = '#'

drawTile :: Tile -> Builder
drawTile t = stringUtf8 (setSGRCode [SetRGBColor Foreground (tileColor t)]) <> charUtf8 (tileSymbol t)

drawMap :: Map Tile -> Builder
drawMap mp = mconcat $ map buildRow [0 .. height mp - 1]
  where
    buildRow y =
        mconcat [drawTile (access mp (x, y)) | x <- [0 .. width mp - 1]]
            <> stringUtf8 "\n"

renderTerminal :: Game ()
renderTerminal = do
    MkLocalState{lsMaps} <- get

    liftIO $ hPutBuilder stdout $ drawMap (lsMaps ! 0)

main :: IO ()
main = do
    let posVtable = VTable vgetPos vsetPos
        zLevelVtable = VTable vgetZlevel vsetZlevel
        storage =
            (defStorage :: EntWorld (WorldOf Underlying))
                { pos = posVtable
                , zLevel = zLevelVtable
                }
        systemRun = runSystemT storage $ do
            y <- createEntity newEntity{pos = Just (MkPosition 0 0)}
            z <- getEntity y
            liftIO $ print (pos z)
        initialState =
            MkLocalState
                { lsMaps = array (0, 0) [(0, emptyMap 10 10)]
                , lsEntityPositions = SM.empty
                , lsZLevelEntities = SM.empty
                , lsCamera = MkPosition 0 0
                , lsTime = 0
                }
    stateReference <- newIORef initialState
    runReaderT systemRun stateReference
