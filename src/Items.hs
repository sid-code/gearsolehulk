module Items where

import Data.Ecstasy (getEntity)
import Data.Ecstasy.Types (Ent)
import Data.Maybe (fromMaybe)
import Message (sendMessage)
import Types (EntWorld (name), Game, IsItem (..))

data Trash = MkTrash {}
    deriving stock (Show, Eq)

instance IsItem Trash where
    useItem :: Ent -> Ent -> Trash -> Game ()
    useItem user item _trash = do
        itemEnt <- getEntity item
        let itemName = fromMaybe "(unnamed trash)" (name itemEnt)
        sendMessage user $ itemName <> " has no use."

data Weapon = MkWeapon {}
    deriving stock (Show, Eq)

instance IsItem Weapon where
    useItem :: Ent -> Ent -> Weapon -> Game ()
    useItem _ent _i _w = error "unimplemented"
