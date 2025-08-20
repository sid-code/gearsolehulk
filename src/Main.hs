{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Control.Monad.Loops (whileM_)
import Control.Monad.Reader (MonadIO (liftIO), MonadReader (ask), ReaderT (runReaderT))
import Data.Array (Array, array, listArray, (!), (//))
import Data.ByteString.Builder (Builder, charUtf8, hPutBuilder, stringUtf8)
import Data.Colour (Colour, ColourOps (darken))
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
import Data.IORef (IORef, modifyIORef, newIORef, readIORef)
import Data.IntMap qualified as IM
import Data.Map qualified as M
import Data.Set (Set, delete, insert, member)
import Data.Text (Text)
import Optics (Lens, lens, set, view)
import System.Console.ANSI (ConsoleLayer (Foreground), SGR (SetRGBColor), setSGRCode)
import System.IO (BufferMode (NoBuffering), hReady, hSetBuffering, hSetEcho, stdin, stdout)

getKey :: IO [Char]
getKey = reverse <$> getKey' ""
  where
    getKey' chars = do
        char <- getChar
        more <- hReady stdin
        (if more then getKey' else return) (char : chars)

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
    , isActor :: Flag f
    , nextAction :: Field f Action
    , isContainer :: Flag f
    , isItem :: Flag f
    , itemData :: Field f Item
    , heldBy :: Field f Ent
    }
    deriving stock (Generic)

data Action = Wait | Move Direction | Use Ent
    deriving stock (Show, Eq)

data Direction = N | NE | E | SE | S | SW | W | NW
    deriving stock (Show, Eq, Ord, Bounded, Enum)

class IsItem i where
    useItem :: Ent -> i -> Game ()

data Item = forall i. (IsItem i) => MkItem i

data Weapon = MkWeapon {}
    deriving stock (Show, Eq)

instance IsItem Weapon where
    useItem :: Ent -> Weapon -> Game ()
    useItem ent i = do
        pure ()

data Tile = Dirt | Stone | Grass | Wall
    deriving stock (Show)

-- | The light level. 0 = no light, 1 = maximal light.
type LightLevel = Float

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

posLens :: (Int, Int) -> Lens (Map d) (Map d) d d
posLens p@(x, y) = lens (`mapAccess` p) $ \m s -> mapUpdate m p s

mapAccess :: Map d -> (Int, Int) -> d
mapAccess MkMap{width, tiles} (x, y) =
    tiles ! (y * width + x)

mapUpdate :: Map d -> (Int, Int) -> d -> Map d
mapUpdate m@MkMap{width, tiles} (x, y) new =
    m{tiles = tiles // [(y * width + x, new)]}

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

type Underlying = ReaderT (IORef LocalState) IO

data LocalState = MkLocalState
    { quit :: Bool
    , maps :: !(Array Int (Map Tile))
    , lsEntityPositions :: M.Map Ent Position
    , lsZLevelEntities :: IM.IntMap (Set Ent)
    , lsCamera :: !Position
    , lsTime :: !Int
    }
    deriving stock (Show, Generic)

get ::
    (MonadIO m, MonadReader (IORef LocalState) m) =>
    m LocalState
get = gets id

gets ::
    (MonadIO m, MonadReader (IORef LocalState) m) =>
    (LocalState -> a) ->
    m a
gets f = do
    ref <- ask
    fmap f . liftIO $ readIORef ref

modify ::
    (MonadIO m, MonadReader (IORef LocalState) m) =>
    (LocalState -> LocalState) ->
    m ()
modify f = do
    ref <- ask
    liftIO $ modifyIORef ref f

type Game = SystemT EntWorld Underlying

type Entity = EntWorld 'FieldOf

type Query = QueryT EntWorld Underlying

move :: Ent -> Position -> Game ()
move ent p@(MkPosition{}) = do
    localState <- get
    setEntity ent unchanged{pos = Set p}

tick :: Game ()
tick = emap allEnts $ do
    position <- query pos
    liftIO $ print position
    pure unchanged

vgetPos :: Ent -> Underlying (Maybe Position)
vgetPos ent = do
    poses <- gets lsEntityPositions
    pure $ M.lookup ent poses

vsetPos :: Ent -> Update Position -> Underlying ()
vsetPos ent upd =
    modify $ \ls@MkLocalState{lsEntityPositions} -> do
        ls
            { lsEntityPositions = doUpdate upd lsEntityPositions
            }
  where
    doUpdate (Set newpos) = M.insert ent newpos . M.delete ent
    doUpdate Unset = M.delete ent
    doUpdate Keep = id

findKeyWith :: (k -> v -> Bool) -> M.Map k v -> Maybe k
findKeyWith cond =
    M.foldrWithKey
        ( \k v ->
            \case
                Just x -> Just x
                Nothing -> if cond k v then Just k else Nothing
        )
        Nothing

findBy :: (Int -> t -> Bool) -> IM.IntMap t -> Maybe Int
findBy cond intmap = go (IM.keys intmap)
  where
    go [] = Nothing
    go (k : ks) =
        if cond k (intmap IM.! k)
            then Just k
            else go ks
vgetZlevel :: Ent -> Underlying (Maybe Int)
vgetZlevel ent = do
    zLevelEnts <- gets lsZLevelEntities
    pure $ findBy (const $ member ent) zLevelEnts

vsetZlevel :: Ent -> Update Int -> Underlying ()
vsetZlevel ent upd = do
    currentz <- vgetZlevel ent
    modify $ \ls@MkLocalState{lsZLevelEntities} -> do
        ls{lsZLevelEntities = doUpdate currentz upd lsZLevelEntities}
  where
    doUpdate curz (Set newz) = IM.update (Just . insert ent) newz . doUpdate curz Unset
    doUpdate curz Unset = maybe id (IM.update (Just . delete ent)) curz
    doUpdate _ Keep = id

tileColor :: Tile -> Colour Float
tileColor Grass = sRGB24 40 160 20
tileColor Stone = sRGB24 10 10 10
tileColor Dirt = sRGB24 80 40 40
tileColor Wall = sRGB24 255 255 255

tileSymbol :: Tile -> Char
tileSymbol Grass = ','
tileSymbol Stone = '.'
tileSymbol Dirt = ','
tileSymbol Wall = '#'

drawTile :: Tile -> LightLevel -> Builder
drawTile t light = stringUtf8 (setSGRCode [SetRGBColor Foreground (darken (1 - light) (tileColor t))]) <> charUtf8 (tileSymbol t)

drawMap :: Map Tile -> Builder
drawMap mp = mconcat $ map buildRow [0 .. height mp - 1]
  where
    buildRow y =
        mconcat [drawTile (view (posLens (x, y)) mp) 1 | x <- [0 .. width mp - 1]]
            <> stringUtf8 "\n"

setupTerminal :: IO ()
setupTerminal = do
    hSetBuffering stdin NoBuffering
    hSetEcho stdin False

teardownTerminal :: IO ()
teardownTerminal = pure () -- TODO

renderTerminal :: Game ()
renderTerminal = do
    ls <- get

    liftIO $ hPutBuilder stdout $ drawMap (view #maps ls ! 0)

newEntOnLevel :: Int -> Game Ent
newEntOnLevel lev = do
    ent <- createEntity newEntity
    setEntity ent unchanged{zLevel = Set lev}
    pure ent

step :: Game ()
step = pure ()

handleInput :: [Char] -> Game ()
handleInput key =
    case key of
        "q" -> modify $ set #quit True
        _ -> pure ()

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

            liftIO setupTerminal
            whileM_ (not . view #quit <$> get) $ do
                renderTerminal
                key <- liftIO getKey
                handleInput key
            liftIO teardownTerminal

        initialState =
            MkLocalState
                { quit = False
                , maps = array (0, 0) [(0, emptyMap 10 10)]
                , lsEntityPositions = M.empty
                , lsZLevelEntities = IM.empty
                , lsCamera = MkPosition 0 0
                , lsTime = 0
                }
    stateReference <- newIORef initialState
    runReaderT systemRun stateReference
