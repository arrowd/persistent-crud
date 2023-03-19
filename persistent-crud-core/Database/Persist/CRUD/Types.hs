module Database.Persist.CRUD.Types where

import Control.Monad.Reader (ReaderT)
import Database.Persist
import Database.Persist.SqlBackend (SqlBackend)
import Data.Dynamic

data Command = ListEntities
             | TimeHelp
             | Create [PersistValue]
             | Read {
                filter :: [[Dynamic]],
                readLimitTo :: Word
               }
             | Update {
                keyToUpdate :: PersistValue,
                valuesToUpdate :: [PersistValue]
               }
             | Delete {
                confirmDeleteEverything :: Bool,
                filter :: [[Dynamic]]
               }
  deriving Show

type Action m = Command -> ReaderT SqlBackend m PersistValue
