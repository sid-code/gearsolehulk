module Message where

import Data.Ecstasy
import Data.Text
import Types

sendMessage :: Ent -> Text -> Game ()
sendMessage ent msg =
    emap (anEnt ent) $ do
        msgs <- query messages
        pure unchanged{messages = Set $ msg : msgs}
