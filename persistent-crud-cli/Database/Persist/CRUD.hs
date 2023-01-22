{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Database.Persist.CRUD (
    module Database.Persist.CRUD.TH,
    module Database.Persist.CRUD.Types,
    fieldToArgument
  )
where

import Database.Persist
import Database.Persist.Quasi.Internal (UnboundEntityDef, unboundEntityDef)
import Database.Persist.CRUD.TH
import Database.Persist.CRUD.Types
import Options.Applicative

-- TODO: This is a suboptimal solution until it'd be possible to call 'tabulateEntityA' in the TH code
-- We also leak 'fieldToArgument' outside
fieldToArgument :: FieldDef -> Parser PersistValue
fieldToArgument field = case fieldType field of
    FTTypeCon _ "Text" -> $textArgument
    FTTypeCon _ "String" -> $textArgument
    FTTypeCon _ "Int" -> $intArgument

