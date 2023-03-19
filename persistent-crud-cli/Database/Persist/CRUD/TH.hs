{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ExplicitForAll #-}
module Database.Persist.CRUD.TH(
    mkPersistCRUD,
    mkPersistCRUD'
  )
where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader.Class
import Data.Char (toUpper, toLower, isSpace)
import Data.Dynamic
import qualified Data.List as L
import qualified Data.List.Split as L
import Data.Maybe (isJust)
import qualified Data.Text as T
import Text.Read (readMaybe)

import Language.Haskell.TH.Syntax

import Database.Persist
import Database.Persist.Class.PersistEntity
import Database.Persist.Quasi.Internal
import Database.Persist.TH
import Database.Persist.EntityDef.Internal (EntityDef(..))

import Database.Persist.CRUD.Types as CRUD
import Database.Persist.CRUD.Core.Stolen
import qualified Database.Persist.CRUD.Core.TH as Core
import Database.Persist.CRUD.Options

-- | For each <Entity> creates definitions for:
--    * Functions of type 'Mod CommandFields (Command, Action m)' named
--        * create<Entity>Command
--        * read<Entity>Command
--        * update<Entity>Command
--        * delete<Entity>Command
--    * Functions of type 'Mod CommandFields (Command, Action m)':
--        * createCommands
--        * readCommands
--        * updateCommands
--        * deleteCommands
--    * By means of \"persistent-crud-core\" package, functions of type 'Action m' named
--        * create<Entity>Action
--        * read<Entity>Action
--        * update<Entity>Action
--        * delete<Entity>Action
mkPersistCRUD
    :: MkPersistSettings
    -> [UnboundEntityDef]
    -> Q [Dec]
mkPersistCRUD mps ents = do
    let -- ["create<Entity1>Command", "create<Entity2>Command", ... ]
        allCreateCmdExprs = pure $ ListE $ map (VarE . mkNameForEntity "create" "Command") ents
        -- ["read<Entity1>Command", "read<Entity2>Command", ... ]
        allReadCmdExprs = pure $ ListE $ map (VarE . mkNameForEntity "read" "Command") ents
        -- ["update<Entity1>Command", "update<Entity2>Command", ... ]
        allUpdateCmdExprs = pure $ ListE $ map (VarE . mkNameForEntity "update" "Command") ents
        -- ["delete<Entity1>Command", "delete<Entity2>Command", ... ]
        allDeleteCmdExprs = pure $ ListE $ map (VarE . mkNameForEntity "delete" "Command") ents

    -- create<Entity1>Command = ...
    -- create<Entity2>Command = ...
    createCommandDecs <- mconcat <$> forM ents (mkCommandDec mps "create" createCommandParser createCommandDefinition)

    -- read<Entity1>Command = ...
    -- read<Entity2>Command = ...
    readCommandDecs <- mconcat <$> forM ents (mkCommandDec mps "read" readCommandParser readCommandDefinition)

    -- update<Entity1>Command = ...
    -- update<Entity2>Command = ...
    updateCommandDecs <- mconcat <$> forM ents (mkCommandDec mps "update" updateCommandParser updateCommandDefinition)

    -- delete<Entity1>Command = ...
    -- delete<Entity2>Command = ...
    deleteCommandDecs <- mconcat <$> forM ents (mkCommandDec mps "delete" deleteCommandParser deleteCommandDefinition)

    allCommandDecs <- [d|
        listEntitiesCommand :: MonadIO m => Mod CommandFields (Command, Action m)
        listEntitiesCommand = command "list-entities" $ info (pure (ListEntities, const $ Core.listEntities ents)) (progDesc "List all known entities")

        createCommands :: MonadIO m => Mod CommandFields (Command, Action m)
        createCommands = mconcat $(allCreateCmdExprs)

        readCommands :: MonadIO m => Mod CommandFields (Command, Action m)
        readCommands = mconcat $(allReadCmdExprs)

        updateCommands :: MonadIO m => Mod CommandFields (Command, Action m)
        updateCommands = mconcat $(allUpdateCmdExprs)

        deleteCommands :: MonadIO m => Mod CommandFields (Command, Action m)
        deleteCommands = mconcat $(allDeleteCmdExprs)
      |]

    filterReadMDecs <- mconcat <$> forM ents (mkFilterDec mps)

    actionDecs <- Core.mkPersistCRUD mps ents

    return $ mconcat [
        actionDecs,
        filterReadMDecs,
        createCommandDecs,
        readCommandDecs,
        updateCommandDecs,
        deleteCommandDecs,
        allCommandDecs
      ]

-- | Variant of 'mkPersistCRUD' that takes 'EntityDef's instead of 'UnboundEntityDef's.
mkPersistCRUD'
    :: MkPersistSettings
    -> [EntityDef]
    -> Q [Dec]
mkPersistCRUD' mps ents = mkPersistCRUD mps (map unbindEntityDef ents)

createCommandParser :: MkParserDefinition
createCommandParser _ recordType = [|
    CRUD.Create <$> traverse (mkArg . fieldToArgument) (getEntityFields (entityDef (Nothing :: Maybe $recordType)))
  |]

type MkCommandDefinition = Q Pat -> Q Exp -> Q Exp -> UnboundEntityDef -> Q [Dec]
type MkParserDefinition = Q Exp -> Q Type -> Q Exp

createCommandDefinition :: MkCommandDefinition
createCommandDefinition funcName parser action ent = [d|
    $funcName = command ("create-" ++ entityNameString ent)
        (info (fmap (\cmd -> (cmd, $(action))) $(parser))
          (progDesc $ "Create an instance of " ++ entityNameString ent ++ " entity"))
  |]


readCommandParser :: MkParserDefinition
readCommandParser filterReaderName _ = [|
    CRUD.Read
      <$> many (option $filterReaderName (short 'f' <> long "filter" <> metavar "FILTER" <> help "Filtering condition"))
      <*> option auto (short 'l' <> metavar "INT" <> help "Number of rows to limit the output to" <> value 0)
  |]

readCommandDefinition :: MkCommandDefinition
readCommandDefinition funcName parser action ent = [d|
    $funcName = command ("read-" ++ entityNameString ent)
        (info (fmap (\cmd -> (cmd, $(action))) $(parser))
              (progDesc $ "List instances of " ++ entityNameString ent ++ " entity"))
  |]


updateCommandParser :: MkParserDefinition
updateCommandParser _ recordType = [|
    CRUD.Update <$> mkArg keyArgument <*> traverse (mkArg . fieldToArgument) (getEntityFields (entityDef (Nothing :: Maybe $recordType)))
  |]

updateCommandDefinition :: MkCommandDefinition
updateCommandDefinition funcName parser action ent = [d|
    $funcName = command ("update-" ++ entityNameString ent)
        (info (fmap (\cmd -> (cmd, $(action))) $(parser))
              (progDesc $ "Update an instance of " ++ entityNameString ent ++ " entity identified by given key with new values"))
  |]


deleteCommandParser :: MkParserDefinition
deleteCommandParser filterReaderName _ = [|
    CRUD.Delete
      <$> switch (short 'c' <> long "confirm-delete-everything" <> help "Remove all instances if no filter is provided")
      <*> many (option $filterReaderName (short 'f' <> long "filter" <> metavar "FILTER" <> help "Filtering condition"))
  |]

deleteCommandDefinition :: MkCommandDefinition
deleteCommandDefinition funcName parser action ent = [d|
    $funcName = command ("delete-" ++ entityNameString ent)
        (info (fmap (\cmd -> (cmd, $(action))) $(parser))
              (progDesc $ "Delete an instance of " ++ entityNameString ent ++ " entity identified by given key"))
  |]


commandDefinitionType :: Q Type
commandDefinitionType = [t|
    forall m . MonadIO m => Mod CommandFields (Command, Action m)
  |]


mkCommandDec :: MkPersistSettings -> String -> MkParserDefinition -> MkCommandDefinition -> UnboundEntityDef -> Q [Dec]
mkCommandDec mps commandName mkParser mkDefinition ent = do
  let funcName = mkNameForEntity commandName "Command" ent
      filterReaderName = pure $ VarE $ mkNameForEntity "" "FilterReadM" ent
      recordType = pure $ genericDataType mps (entityHaskell (unboundEntityDef ent)) backendT
      action = pure $ VarE $ mkNameForEntity commandName "Action" ent
      funcNamePat = pure $ VarP funcName

  defType <- commandDefinitionType
  let signature = SigD funcName defType
  def <- mkDefinition funcNamePat (mkParser filterReaderName recordType) action ent

  return (signature : def)

mkFilterDec :: MkPersistSettings -> UnboundEntityDef -> Q [Dec]
mkFilterDec mps ent = do
  let funcName = mkNameForEntity "" "FilterReadM" ent
      funcNameP = pure $ VarP funcName
      recordType = pure $ genericDataType mps (entityHaskell (unboundEntityDef ent)) backendT
  caseE <- do
        caseMatches <- forM (unboundEntityFields ent) $ \unboundField -> do
          let entityFieldName = filterConName mps ent unboundField
              fieldConName = mkName entityFieldName
              fieldCon = pure $ ConE fieldConName
          caseBranchBody <- [|do
              filter <- case readPersistFilterMaybe filterStr of
                             Just filter -> pure filter
                             _ -> readerAbort $ ErrorMsg ("Unknown filter operator: " <> filterStr)
              let field = persistFieldDef $fieldCon
              value <- ReadM $ local (const valueStr) $ unReadM $ do
                  case filter of
                    In -> FilterValues <$> many (fromPersistValueRight <$> fst (fieldToArgument field))
                    NotIn -> FilterValues <$> many (fromPersistValueRight <$> fst (fieldToArgument field))
                    _ -> FilterValue <$> (fromPersistValueRight <$> fst (fieldToArgument field))
              pure $ toDyn $ Filter $fieldCon value filter
            |]
          pure $ Match
            (LitP $ StringL entityFieldName)
            (NormalB caseBranchBody)
            []
        wildMatchBody <- [|readerAbort $ ErrorMsg ("Unknown entity field: " <> fieldStr)|]
        let wildMatch = Match WildP (NormalB wildMatchBody) []
        pure $ CaseE (VarE $ mkName "fieldStr") (caseMatches <> [wildMatch])

  defType <- [t|ReadM [Dynamic]|]
  let signature = SigD funcName defType
  definition <- [d|
      $funcNameP = do
          str0 <- readerAsk
          forM (L.splitOn "&" str0) $ \str -> do
            let (fieldStr0, rest) = break isFilterSymbol str
                fieldStr = (L.dropWhile isSpace . L.dropWhileEnd isSpace) fieldStr0
                (filterStr, valueStr) = span isFilterSymbol rest
                fields = getEntityFields (entityDef (Nothing :: Maybe $recordType))
            $(pure caseE)
    |]
  pure (signature : definition)

mkNameForEntity :: String -> String -> UnboundEntityDef -> Name
mkNameForEntity prefix suffix ent = mkName (prefix <> entName <> suffix)
  where
    entName = if null prefix
                 then toLower (head $ entityNameString ent) : tail (entityNameString ent)
                 else entityNameString ent

entityNameString :: UnboundEntityDef -> String
entityNameString = T.unpack . unEntityNameHS . getEntityHaskellName . unboundEntityDef


fromPersistValueRight :: PersistField a => PersistValue -> a
fromPersistValueRight x = case fromPersistValue x of
    Right r -> r
    Left e -> error ("Impossible happened: " <> T.unpack e)


fieldToArgument :: FieldDef -> (ReadM PersistValue, Mod ArgumentFields PersistValue)
fieldToArgument field = if isMaybe
    then maybefy arg
    else arg
  where
    arg = case fieldSqlType field of
      SqlString -> textArgument
      SqlInt32 -> intArgument
      SqlInt64 -> intArgument
      SqlBool -> boolArgument
      SqlDayTime -> timeArgument
    isMaybe = case fieldType field of
      FTApp (FTTypeCon _ "Maybe") _ -> True
      _ -> isFieldNullable field /= NotNullable
