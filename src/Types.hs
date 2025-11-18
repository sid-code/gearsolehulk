module Types where

import Control.Monad.Reader (MonadIO (liftIO), MonadReader (ask), ReaderT)
import Data.Array (Array, (!), (//))
import Data.Colour (Colour)
import Data.Colour.SRGB (sRGB24)
import Data.Ecstasy (
    Component,
    ComponentType (Field, Virtual),
    Ent,
    Generic,
    QueryT,
    StorageType (FieldOf),
    SystemT,
 )
import Data.IORef (IORef, modifyIORef, readIORef)
import Data.IntMap qualified as IM
import Data.Map qualified as M
import Data.Set (Set)
import Data.Text (Text)
import Optics (Lens, lens)
import System.Random (Random (..), StdGen)

type Game = SystemT EntWorld Underlying

type Entity = EntWorld 'FieldOf

type Query = QueryT EntWorld Underlying

type Underlying = ReaderT (IORef LocalState) IO

data LocalState = MkLocalState
    { quit :: Bool
    , maps :: !(Array Int (Map Tile))
    , lsEntityPositions :: M.Map Position (Set Ent)
    , lsZLevelEntities :: IM.IntMap (Set Ent)
    , player :: Maybe Ent
    , viewfield :: Maybe ViewField
    , time :: !Int
    , rng :: StdGen
    }
    deriving stock (Show, Generic)

newtype Position = MkPosition (Int, Int)
    deriving stock (Eq, Show)

instance Ord Position where
    compare (MkPosition (x1, y1)) (MkPosition (x2, y2)) =
        case compare x1 x2 of
            EQ -> compare y1 y2
            anythingElse -> anythingElse

newtype RelativePosition = MkRelativePosition Position
    deriving stock (Eq, Show)
    deriving newtype (Ord)

data ViewTile = MkViewTile
    { entities :: [Ent]
    , tile :: Tile
    , isMemory :: Bool
    , lightLevel :: Float
    }
    deriving stock (Show)

data ViewField = MkViewField
    { position :: Position
    , visible :: M.Map Position ViewTile
    , timeStationary :: Int
    }
    deriving stock (Show)

type Flag f = Component f 'Field ()
type Field f a = Component f 'Field a

data EntWorld f = Entity
    { pos :: Component f 'Virtual Position
    , zLevel :: Component f 'Virtual Int
    , -- Description
      name :: Field f Text
    , description :: Field f Text
    , display :: Field f EntityDisplay
    -- ^ How this entity is displayed.
    , isPlayer :: Flag f
    -- ^ Is this entity a player?
    , actionPolicy :: Field f ActionPolicy
    , speed :: Field f Int
    -- ^ The amount of time it takes for this entity to perform an action.
    , messages :: Field f [Text]
    , -- Item hierarchy
      isContainer :: Flag f
    , itemData :: Field f Item
    , heldBy :: Field f Ent
    }
    deriving stock (Generic)

data EntityDisplay = MkEntityDisplay
    { symbol :: Char
    , color :: Colour Float
    }
    deriving stock (Show, Eq)

data Action = Wait | Invalid | Move Direction | MoveTo Position | Use Ent
    deriving stock (Show, Eq)

data Direction = N | NE | E | SE | S | SW | W | NW
    deriving stock (Show, Eq, Ord, Bounded, Enum)

toDelta :: Direction -> (Int, Int)
toDelta NE = (1, -1)
toDelta N = (0, -1)
toDelta NW = (-1, -1)
toDelta W = (-1, 0)
toDelta SW = (-1, 1)
toDelta S = (0, 1)
toDelta SE = (1, 1)
toDelta E = (1, 0)

instance Random Direction where
    randomR (lo, hi) g = (toEnum i, g')
      where
        (i, g') = randomR (fromEnum lo, fromEnum hi) g
    random = randomR (minBound, maxBound)

class IsItem i where
    useItem :: Ent -> i -> Game ()

data Item = forall i. (IsItem i) => MkItem i

data Weapon = MkWeapon {}
    deriving stock (Show, Eq)

instance IsItem Weapon where
    useItem :: Ent -> Weapon -> Game ()
    useItem _ent _i = do
        pure ()

data Tile = Dirt | Stone | Grass | Wall
    deriving stock (Show, Eq)

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

-- | The light level. 0 = no light, 1 = maximal light.
type LightLevel = Float

data Map d = MkMap
    { width :: Int
    , height :: Int
    , tiles :: Array Int d
    }
    deriving stock (Show)

instance Functor Map where
    fmap :: forall a b. (a -> b) -> Map a -> Map b
    fmap f m = m{tiles = fmap f t}
      where
        t = tiles m

data ActionPolicy
    = AlwaysWait
    | RandomWalk
    | -- | Ask the player for an action.
      AskIO
    deriving stock (Show, Eq)

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

posLens :: Position -> Lens (Map d) (Map d) d d
posLens (MkPosition p) = lens (`mapAccess` p) $ \m s -> mapUpdate m p s

mapAccess :: Map d -> (Int, Int) -> d
mapAccess MkMap{width, tiles} (x, y) =
    tiles ! (y * width + x)

mapUpdate :: Map d -> (Int, Int) -> d -> Map d
mapUpdate m@MkMap{width, tiles} (x, y) new =
    m{tiles = tiles // [(y * width + x, new)]}
