module Database.Persist.CRUD.Types where

import Control.Monad.Reader (ReaderT)
import Data.Int
import Database.Persist
import Database.Persist.SqlBackend (SqlBackend)

data Command = ListEntities
             | TimeHelp
             | Create [PersistValue]
             | Read {
                readLimitTo :: Word
               }
             | Update {
                keyToUpdate :: PersistValue,
                valuesToUpdate :: [PersistValue]
               }
  deriving Show

type Action m = Command -> ReaderT SqlBackend m PersistValue
