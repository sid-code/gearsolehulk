{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Main where

import Control.Monad (void)
import Control.Monad.State
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import System.Random

-- | Entity ID for tracking entities in the world
type EntityId = Int

-- | Room ID for tracking locations
type RoomId = Int

-- | Basic Brain typeclass
class Brain b where
    type Thought b = r | r -> b
    type Body b = r | r -> b
    type Action b = r | r -> b

    type World b

    think :: b -> Body b -> World b -> (b, Thought b)
    produce :: b -> Thought b -> Action b

-- | Messages that entities can send to each other
data Message
    = Chemical String -- Chemical signals (pheromones, etc.)
    | Physical String -- Physical interactions
    | Sonic String -- Sound-based communication
    deriving stock (Eq, Show)

-- | Room in the world graph
data Room = Room
    { roomId :: RoomId
    , roomName :: String
    , roomDescription :: String
    , roomConnections :: Set RoomId
    , roomNutrients :: Int -- Available food/nutrients in this room
    , roomMessages :: [Message] -- Messages left in this room
    }
    deriving stock (Show)

-- | World state containing the graph and entities
data GraphWorld = MkGraphWorld
    { worldRooms :: Map RoomId Room
    , worldEntities :: Map EntityId SomeEntity
    , worldEntityLocations :: Map EntityId RoomId
    , worldNextEntityId :: EntityId
    , worldGen :: StdGen
    }
    deriving stock (Show)

-- | Existential wrapper for entities with different brain types
data SomeEntity
    = forall b.
      (Brain b, Action b ~ WorldAction, World b ~ GraphWorld) =>
    SomeEntity
    { entityId :: EntityId
    , entityBrain :: b
    , entityBody :: Body b
    , entityName :: String
    }

instance Show SomeEntity where
    show (SomeEntity eid _ _ name) = "Entity(" ++ show eid ++ ":" ++ name ++ ")"

-- | Actions that entities can take in the world
data WorldAction
    = Move RoomId -- Move to a connected room
    | Consume Int -- Consume nutrients from current room
    | SendMessage Message -- Send a message to current room
    | Rest -- Do nothing
    | Reproduce -- Attempt to reproduce
    deriving stock (Eq, Show)

-- | Rock implementation (simple, does nothing)
type RockThought = ()

type RockBrain = ()

type RockAction = WorldAction

type RockBody = ()

instance Brain RockBrain where
    type Thought RockBrain = RockThought
    type Action RockBrain = RockAction
    type Body RockBrain = RockBody
    type World RockBrain = GraphWorld

    think _ _ _ = ((), ())
    produce _ _ = Rest

-- | Worm implementation (more complex behavior)
data WormThought
    = Hungry Int -- Hungry with urgency level
    | Exploring -- Looking for new areas
    | Communicating -- Sending/receiving signals
    | Reproducing -- Ready to reproduce
    | Resting -- Conserving energy
    deriving stock (Eq, Show)

data WormBrain = WormBrain
    { wormLastThought :: WormThought
    , wormMemory :: Set RoomId -- Rooms the worm has visited
    , wormEnergy :: Int
    , wormAge :: Int
    }
    deriving stock (Show)

data WormBody = WormBody
    { wormGlucose :: Int
    , wormSize :: Int
    , wormHealth :: Int
    }
    deriving stock (Show)

instance Brain WormBrain where
    type Thought WormBrain = WormThought
    type Action WormBrain = WorldAction
    type Body WormBrain = WormBody
    type World WormBrain = GraphWorld

    think brain body world =
        let newBrain = brain{wormAge = wormAge brain + 1}
            thought = determineWormThought newBrain body world
         in (newBrain{wormLastThought = thought}, thought)

    produce = wormActionFromThought

-- | Determine what a worm should think based on its state
determineWormThought :: WormBrain -> WormBody -> GraphWorld -> WormThought
determineWormThought brain body world
    | wormGlucose body < 20 = Hungry (30 - wormGlucose body)
    | wormEnergy brain > 80 && wormAge brain > 50 = Reproducing
    | wormEnergy brain < 30 = Resting
    | Set.size (wormMemory brain) < 3 = Exploring
    | otherwise = Communicating

-- | Convert worm thoughts to actions
wormActionFromThought :: WormBrain -> WormThought -> WorldAction
wormActionFromThought brain thought = case thought of
    Hungry urgency -> if urgency > 15 then Consume 10 else Consume 5
    Exploring -> Move 0 -- Will be handled by world logic to pick valid room
    Communicating -> SendMessage (Chemical "worm-presence")
    Reproducing -> Reproduce
    Resting -> Rest

-- | Create initial world
createWorld :: StdGen -> GraphWorld
createWorld gen =
    MkGraphWorld
        { worldRooms =
            Map.fromList
                [ (1, Room 1 "Surface" "Rich topsoil with organic matter" (Set.fromList [2, 3]) 50 [])
                , (2, Room 2 "Deep Tunnel" "Dark underground tunnel" (Set.fromList [1, 4]) 20 [])
                , (3, Room 3 "Compost Area" "Nutrient-rich decomposing matter" (Set.fromList [1, 4]) 80 [])
                , (4, Room 4 "Root Zone" "Among plant roots" (Set.fromList [2, 3, 5]) 30 [])
                , (5, Room 5 "Clay Layer" "Dense clay soil" (Set.fromList [4]) 10 [])
                ]
        , worldEntities = Map.empty
        , worldEntityLocations = Map.empty
        , worldNextEntityId = 1
        , worldGen = gen
        }

-- | Add an entity to the world
addEntity :: SomeEntity -> RoomId -> State GraphWorld EntityId
addEntity entity roomId = do
    world <- get
    let eid = worldNextEntityId world
        newEntity = entity{entityId = eid}
    put $
        world
            { worldEntities = Map.insert eid newEntity (worldEntities world)
            , worldEntityLocations = Map.insert eid roomId (worldEntityLocations world)
            , worldNextEntityId = eid + 1
            }
    return eid

-- | Execute an action for an entity
executeAction :: EntityId -> WorldAction -> State GraphWorld ()
executeAction entityId action = do
        world <- get
        case Map.lookup entityId (worldEntityLocations world) of
            Nothing -> pure () -- Entity doesn't exist
            Just currentRoom -> case action of
                Move targetRoom -> moveEntity entityId currentRoom targetRoom
                Consume amount -> consumeNutrients entityId currentRoom amount
                SendMessage msg -> sendMessageToRoom currentRoom msg
                Rest -> pure () -- Nothing to do
                Reproduce -> attemptReproduction entityId currentRoom

-- | Move entity between rooms
moveEntity :: EntityId -> RoomId -> RoomId -> State GraphWorld ()
moveEntity entityId currentRoom targetRoom = do
    world <- get
    case Map.lookup currentRoom (worldRooms world) of
        Nothing -> return ()
        Just room ->
            if targetRoom `Set.member` roomConnections room || targetRoom == 0
                then do
                    -- If targetRoom is 0, pick a random connected room
                    let (actualTarget, newGen) =
                            if targetRoom == 0
                                then
                                    let connections = Set.toList (roomConnections room)
                                     in if null connections
                                            then (currentRoom, worldGen world)
                                            else
                                                let (idx, g') = randomR (0, length connections - 1) (worldGen world)
                                                 in (connections !! idx, g')
                                else (targetRoom, worldGen world)
                    put $
                        world
                            { worldEntityLocations = Map.insert entityId actualTarget (worldEntityLocations world)
                            , worldGen = newGen
                            }
                else return () -- Invalid move

-- | Consume nutrients from current room
consumeNutrients :: EntityId -> RoomId -> Int -> State GraphWorld ()
consumeNutrients entityId roomId amount = do
    world <- get
    case Map.lookup roomId (worldRooms world) of
        Nothing -> return ()
        Just room -> do
            let actualAmount = min amount (roomNutrients room)
                newRoom = room{roomNutrients = roomNutrients room - actualAmount}
            put $ world{worldRooms = Map.insert roomId newRoom (worldRooms world)}
            -- Update entity's glucose (this is simplified)
            updateEntityNutrition entityId actualAmount

-- | Update entity nutrition (simplified)
updateEntityNutrition :: EntityId -> Int -> State GraphWorld ()
updateEntityNutrition entityId nutrition = do
    world <- get
    case Map.lookup entityId (worldEntities world) of
        Nothing -> return ()
        Just (SomeEntity eid brain body name) ->
            -- This is a simplified update - in reality we'd need type-safe updates
            return ()

-- | Send message to a room
sendMessageToRoom :: RoomId -> Message -> State GraphWorld ()
sendMessageToRoom roomId message = do
    world <- get
    case Map.lookup roomId (worldRooms world) of
        Nothing -> return ()
        Just room -> do
            let newRoom = room{roomMessages = message : roomMessages room}
            put $ world{worldRooms = Map.insert roomId newRoom (worldRooms world)}

-- | Attempt reproduction
attemptReproduction :: EntityId -> RoomId -> State GraphWorld ()
attemptReproduction entityId roomId = do
    world <- get
    -- Simplified reproduction - just add a message
    sendMessageToRoom roomId (Chemical "reproduction-attempt")

-- | Process one simulation step for all entities
simulationStep :: State GraphWorld ()
simulationStep = do
    world <- get
    let entityIds = Map.keys (worldEntities world)
    mapM_ processEntity entityIds
    cleanupMessages

-- | Process a single entity's turn
processEntity :: EntityId -> State GraphWorld ()
processEntity entityId = do
    world <- get
    case Map.lookup entityId (worldEntities world) of
        Nothing -> pure ()
        Just (SomeEntity eid brain body name) -> do
            let (newBrain, thought) = think brain body world
                action = produce newBrain thought
                newEntity = SomeEntity eid newBrain body name
            put $ world{worldEntities = Map.insert entityId newEntity (worldEntities world)}
            executeAction entityId action

-- | Clean up old messages
cleanupMessages :: State GraphWorld ()
cleanupMessages = do
    world <- get
    let cleanRoom room = room{roomMessages = take 5 (roomMessages room)} -- Keep only 5 recent messages
        cleanedRooms = Map.map cleanRoom (worldRooms world)
    put $ world{worldRooms = cleanedRooms}

-- | Print world state
printWorldState :: GraphWorld -> IO ()
printWorldState world = do
    putStrLn "\n=== World State ==="
    putStrLn $ "Entities: " ++ show (Map.size (worldEntities world))
    mapM_ printEntity (Map.elems (worldEntities world))
    putStrLn "\nRooms:"
    mapM_ (printRoom world) (Map.elems (worldRooms world))

printEntity :: SomeEntity -> IO ()
printEntity (SomeEntity eid _ _ name) =
    putStrLn $ "  " ++ name ++ " (ID: " ++ show eid ++ ")"

printRoom :: GraphWorld -> Room -> IO ()
printRoom world room = do
    let entitiesInRoom = Map.keys $ Map.filter (== roomId room) (worldEntityLocations world)
        entityCount = length entitiesInRoom
    putStrLn $
        "  "
            ++ roomName room
            ++ " - Nutrients: "
            ++ show (roomNutrients room)
            ++ ", Entities: "
            ++ show entityCount
            ++ ", Messages: "
            ++ show (length (roomMessages room))

-- | Main simulation loop
runSimulation :: Int -> StateT GraphWorld IO ()
runSimulation 0 = pure ()
runSimulation n = do
    world <- get
    (_, world') <- runStateT (StateT $ pure . runState simulationStep) world
    put world'

    liftIO $ printWorldState world
    liftIO $ putStrLn $ "=== Step " ++ show (101 - n) ++ " completed ===\n"
    runSimulation (n - 1)

-- | Main entry point
main :: IO ()
main = do
    putStrLn "🪱 Starting Worm World Simulation 🌍"
    gen <- getStdGen

    let initialWorld = createWorld gen
        addEntity_ a b = void $ addEntity a b
        setupWorld = do
            -- Add some worms
            addEntity_ (SomeEntity 0 (WormBrain Resting Set.empty 50 0) (WormBody 25 5 100) "Worm-Alpha") 1
            addEntity_ (SomeEntity 0 (WormBrain Resting Set.empty 60 10) (WormBody 30 4 100) "Worm-Beta") 3
            addEntity_ (SomeEntity 0 (WormBrain Resting Set.empty 40 5) (WormBody 15 6 100) "Worm-Gamma") 2
            -- Add some rocks
            addEntity_ (SomeEntity 0 () () "Rock-1") 4
            addEntity_ (SomeEntity 0 () () "Rock-2") 5
            return ()

    let (_, worldWithEntities) = runState setupWorld initialWorld

    putStrLn "Initial world setup complete!"
    printWorldState worldWithEntities

    putStrLn "\nStarting simulation..."
    (_, finalWorld) <- runStateT (runSimulation 5) worldWithEntities

    putStrLn "\n🎉 Simulation completed!"
    putStrLn "Final world state:"
    printWorldState finalWorld
