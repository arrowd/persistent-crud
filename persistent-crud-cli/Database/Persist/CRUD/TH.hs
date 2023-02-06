{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ExplicitForAll #-}
module Database.Persist.CRUD.TH(
    mkPersistCRUD,
    mkPersistCRUD'
  )
where

import Control.Monad
import Control.Monad.IO.Class
import Data.Maybe (isJust)
import qualified Data.Text as T

import Language.Haskell.TH.Syntax

import Database.Persist
import Database.Persist.Quasi.Internal
import Database.Persist.TH
import Database.Persist.EntityDef.Internal (EntityDef(..))

import Database.Persist.CRUD.Types as CRUD
import Database.Persist.CRUD.Options

-- | For each <Entity> creates definitions for:
--    * Functions of type 'Mod CommandFields (Command, Action m)' named
--        * create<Entity>Command
--        * read<Entity>Command
--        * update<Entity>Command
--        * delete<Entity>Command
--    * Functions of type 'Action m' named
--        * create<Entity>Action
--        * read<Entity>Action
--        * update<Entity>Action
--        * delete<Entity>Action
--    * Functions of type 'Mod CommandFields (Command, Action m)':
--        * createCommands
--        * readCommands
--        * updateCommands
--        * deleteCommands
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
    createActionDecs <- mconcat <$> forM ents (mkActionDec mps "create" createAction)

    -- read<Entity1>Command = ...
    -- read<Entity2>Command = ...
    readCommandDecs <- mconcat <$> forM ents (mkCommandDec mps "read" readCommandParser readCommandDefinition)
    readActionDecs <- mconcat <$> forM ents (mkActionDec mps "read" readAction)

    -- update<Entity1>Command = ...
    -- update<Entity2>Command = ...
    updateCommandDecs <- mconcat <$> forM ents (mkCommandDec mps "update" updateCommandParser updateCommandDefinition)
    updateActionDecs <- mconcat <$> forM ents (mkActionDec mps "update" updateAction)

    -- delete<Entity1>Command = ...
    -- delete<Entity2>Command = ...
    deleteCommandDecs <- mconcat <$> forM ents (mkCommandDec mps "delete" deleteCommandParser deleteCommandDefinition)
    deleteActionDecs <- mconcat <$> forM ents (mkActionDec mps "delete" deleteAction)

    allCommandDecs <- [d|
        listEntitiesCommand :: MonadIO m => Mod CommandFields (Command, Action m)
        listEntitiesCommand = command "list-entities" $ info (pure (ListEntities, const $ listEntities ents)) (progDesc "List all known entities")

        createCommands :: MonadIO m => Mod CommandFields (Command, Action m)
        createCommands = mconcat $(allCreateCmdExprs)

        readCommands :: MonadIO m => Mod CommandFields (Command, Action m)
        readCommands = mconcat $(allReadCmdExprs)

        updateCommands :: MonadIO m => Mod CommandFields (Command, Action m)
        updateCommands = mconcat $(allUpdateCmdExprs)

        deleteCommands :: MonadIO m => Mod CommandFields (Command, Action m)
        deleteCommands = mconcat $(allDeleteCmdExprs)
      |]

    return $ mconcat [
        createCommandDecs,
        createActionDecs,
        readCommandDecs,
        readActionDecs,
        updateCommandDecs,
        updateActionDecs,
        deleteCommandDecs,
        deleteActionDecs,
        allCommandDecs
      ]

-- | Variant of 'mkPersistCRUD' that takes 'EntityDef's instead of 'UnboundEntityDef's.
mkPersistCRUD'
    :: MkPersistSettings
    -> [EntityDef]
    -> Q [Dec]
mkPersistCRUD' mps ents = mkPersistCRUD mps (map unbindEntityDef ents)


listEntities :: Applicative m => [UnboundEntityDef] -> m PersistValue
listEntities = pure . PersistList . map (PersistText . unEntityNameHS . getEntityHaskellName . unboundEntityDef)


createCommandParser :: Q Type -> Q Exp
createCommandParser recordType = [|
    CRUD.Create <$> traverse fieldToArgument (getEntityFields (entityDef (Nothing :: Maybe $recordType)))
  |]

type MkCommandDefinition = Q Pat -> Q Exp -> Q Exp -> UnboundEntityDef -> Q [Dec]

createCommandDefinition :: MkCommandDefinition
createCommandDefinition funcName parser action ent = [d|
    $funcName = command ("-" ++ entityNameString ent)
        (info (fmap (\cmd -> (cmd, $(action))) $(parser))
          (progDesc $ "Create an instance of " ++ entityNameString ent ++ " entity"))
  |]

createAction :: Q Type -> Q Exp
createAction recordType = [|
    do
      let Create args = cmd
          valOrErr = fromPersistValues args :: Either T.Text $recordType
      case valOrErr of
          Left err -> pure (PersistText err)
          Right val -> toPersistValue <$> insert val
  |]


readCommandParser :: Q Type -> Q Exp
readCommandParser = const [|
    CRUD.Read <$> option auto (short 'l' <> metavar "INT" <> help "Number of rows to limit the output to" <> value 0)
  |]

readCommandDefinition :: MkCommandDefinition
readCommandDefinition funcName parser action ent = [d|
    $funcName = command ("read-" ++ entityNameString ent)
        (info (fmap (\cmd -> (cmd, $(action))) $(parser))
              (progDesc $ "List instances of " ++ entityNameString ent ++ " entity"))
  |]

readAction :: Q Type -> Q Exp
readAction recordType = [|
    do
      let Read limit = cmd
          selectOpts = case limit of
            0 -> []
            _ -> [LimitTo $ fromIntegral limit]
      ents <- selectList [] selectOpts
      pure (PersistList (map toPersistValue (ents :: [Entity $recordType])))
  |]


updateCommandParser :: Q Type -> Q Exp
updateCommandParser recordType = [|
    CRUD.Update <$> mkArg keyArgument <*> traverse fieldToArgument (getEntityFields (entityDef (Nothing :: Maybe $recordType)))
  |]

updateCommandDefinition :: MkCommandDefinition
updateCommandDefinition funcName parser action ent = [d|
    $funcName = command ("update-" ++ entityNameString ent)
        (info (fmap (\cmd -> (cmd, $(action))) $(parser))
              (progDesc $ "Update an instance of " ++ entityNameString ent ++ " entity identified by given key with new values"))
  |]

updateAction :: Q Type -> Q Exp
updateAction recordType = [|
    do
      let CRUD.Update key args = cmd
          keyOrErr = fromPersistValue key :: Either T.Text (Key $recordType)
          valOrErr = fromPersistValues args :: Either T.Text $recordType
      case (,) <$> keyOrErr <*> valOrErr of
        Left err -> pure (PersistText err)
        Right (key', val) -> do
          existingEnt <- get key'
          case existingEnt of
            Nothing -> pure (PersistText "Requested key not found")
            Just _ -> do
              replace key' val
              pure (PersistBool True)
  |]


deleteCommandParser :: Q Type -> Q Exp
deleteCommandParser = const [|
    CRUD.Delete
      <$> switch (short 'c' <> long "check-if-exists" <> help "Check for key existance before deleting")
      <*> mkArg keyArgument
  |]

deleteCommandDefinition :: MkCommandDefinition
deleteCommandDefinition funcName parser action ent = [d|
    $funcName = command ("delete-" ++ entityNameString ent)
        (info (fmap (\cmd -> (cmd, $(action))) $(parser))
              (progDesc $ "Delete an instance of " ++ entityNameString ent ++ " entity identified by given key"))
  |]

deleteAction :: Q Type -> Q Exp
deleteAction recordType = [|
    do
      let CRUD.Delete {..} = cmd
          keyOrErr = fromPersistValue keyToDelete :: Either T.Text (Key $recordType)
      case keyOrErr of
        Left err -> pure (PersistText err)
        Right key' -> do
          exists <- if checkIfExists
                        then isJust <$> get key'
                        else pure True
          if exists
              then do
                delete key'
                pure (PersistBool True)
              else pure (PersistBool False)
  |]


commandDefinitionType :: Q Type
commandDefinitionType = [t|
    forall m . MonadIO m => Mod CommandFields (Command, Action m)
  |]

actionDefinitionType :: Q Type
actionDefinitionType = [t|
    forall m . MonadIO m => Action m
  |]

mkCommandDec :: MkPersistSettings -> String -> (Q Type -> Q Exp) -> MkCommandDefinition -> UnboundEntityDef -> Q [Dec]
mkCommandDec mps commandName mkParser mkDefinition ent = do
  let funcName = mkNameForEntity commandName "Command" ent
      recordType = pure $ genericDataType mps (entityHaskell (unboundEntityDef ent)) backendT
      action = pure $ VarE $ mkNameForEntity commandName "Action" ent
      funcNamePat = pure $ VarP funcName

  defType <- commandDefinitionType
  let signature = SigD funcName defType
  def <- mkDefinition funcNamePat (mkParser recordType) action ent

  return (signature : def)

mkActionDec :: MkPersistSettings -> String -> (Q Type -> Q Exp) -> UnboundEntityDef -> Q [Dec]
mkActionDec mps commandName mkAction ent = do
  let funcName = mkNameForEntity commandName "Action" ent
      recordType = pure $ genericDataType mps (entityHaskell (unboundEntityDef ent)) backendT

  defType <- actionDefinitionType
  body <- mkAction recordType

  let signature = SigD funcName defType
      definition = FunD funcName [Clause [VarP $ mkName "cmd"] (NormalB body) []]

  return [signature, definition]

mkNameForEntity :: String -> String -> UnboundEntityDef -> Name
mkNameForEntity prefix suffix ent = mkName (prefix <> entityNameString ent <> suffix)

entityNameString :: UnboundEntityDef -> String
entityNameString = T.unpack . unEntityNameHS . getEntityHaskellName . unboundEntityDef


fieldToArgument :: FieldDef -> Parser PersistValue
fieldToArgument field = mkArg $ case fieldSqlType field of
    SqlString -> maybefy textArgument
    SqlInt32 -> maybefy intArgument
    SqlInt64 -> maybefy intArgument
    SqlBool -> maybefy boolArgument
    SqlDayTime -> maybefy timeArgument
  where
    isMaybe = case fieldType field of
      FTApp (FTTypeCon _ "Maybe") _ -> True
      _ -> isFieldNullable field /= NotNullable
    maybefy (valReader, valMod) = if isMaybe
      then (maybeReader <|> valReader, valMod <> optionMod prependMaybeMetavar)
      else (valReader, valMod)
    (maybeReader, maybeMod) = maybeArgument
    prependMaybeMetavar optProps = optProps { propMetaVar = "(# | " <> propMetaVar optProps <> ")"}

-- Stolen from persistent itself

genericDataType
    :: MkPersistSettings
    -> EntityNameHS
    -> Type -- ^ backend
    -> Type
genericDataType mps name backend
    | mpsGeneric mps =
        ConT (mkEntityNameHSGenericName name) `AppT` backend
    | otherwise =
        ConT $ mkEntityNameHSName name

backendT :: Type
backendT = VarT backendName

mkEntityNameHSName :: EntityNameHS -> Name
mkEntityNameHSName =
    mkName . T.unpack . unEntityNameHS

mkEntityNameHSGenericName :: EntityNameHS -> Name
mkEntityNameHSGenericName name =
    mkName $ T.unpack (unEntityNameHS name <> "Generic")

backendName :: Name
backendName = mkName "backend"

