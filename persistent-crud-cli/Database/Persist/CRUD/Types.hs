module Database.Persist.CRUD.Types where

import Control.Monad.Reader (ReaderT)
import Database.Persist
import Database.Persist.SqlBackend (SqlBackend)

data Command = ListEntities
             | Create [PersistValue]
             | Read [PersistValue]
             | Update [PersistValue]
  deriving Show

type Action m = Command -> ReaderT SqlBackend m PersistValue
