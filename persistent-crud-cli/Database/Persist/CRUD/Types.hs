module Database.Persist.CRUD.Types where

import Control.Monad.Reader (ReaderT)
import Database.Persist
import Database.Persist.SqlBackend (SqlBackend)

data Command = ListEntities
             | List
             | Create [PersistValue]

type Action m = Command -> ReaderT SqlBackend m PersistValue
