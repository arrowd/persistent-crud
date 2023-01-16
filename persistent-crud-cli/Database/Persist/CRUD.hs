{-# LANGUAGE OverloadedStrings #-}
module Database.Persist.CRUD (
    module Database.Persist.CRUD.TH,
    module Database.Persist.CRUD.Types,
    listEntitiesCommand,
    fieldToArgument
  )
where

import qualified Data.Text as T
import Database.Persist
import Database.Persist.CRUD.TH
import Database.Persist.CRUD.Types
import Options.Applicative

-- | Lists all entities known to Persistent.
-- This command does not require TH code generation.
listEntitiesCommand :: Applicative m => [EntityDef] -> Mod CommandFields (Command, Action m)
listEntitiesCommand entityDefs =
  command "list-entities" $
    info (pure (ListEntities, const $ listEntities entityDefs)) (progDesc "List all known entities")

listEntities :: Applicative m => [EntityDef] -> m PersistValue
listEntities = pure . PersistList . map (PersistText . unEntityNameHS . getEntityHaskellName)

-- TODO: This is a suboptimal solution until it'd be possible to call 'tabulateEntityA' in the TH code
-- We also leak 'fieldToArgument' outside
fieldToArgument :: FieldDef -> Parser PersistValue
fieldToArgument field = case fieldType field of
    FTTypeCon _ "Text" -> textArgument
    FTTypeCon _ "String" -> textArgument
    FTTypeCon _ "Int" -> intArgument

textArgument = argument (PersistText . T.pack <$> str) (metavar "TEXT")
intArgument = argument (PersistInt64 <$> auto) (metavar "INT")
