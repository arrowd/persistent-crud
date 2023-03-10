{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}
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
--    * create-<Entity> functions
--    * read-<Entity> functions
--    * update-<Entity> functions
--    * delete-<Entity> functions
-- | Also creates following definitions of type 'Mod CommandFields (Command, Action m)':
--    * createCommands
--    * readCommands
--    * updateCommands
--    * deleteCommands
mkPersistCRUD
    :: MkPersistSettings
    -> [UnboundEntityDef]
    -> Q [Dec]
mkPersistCRUD mps ents = do
    listEntitiesCommandDec <- [d|
        listEntitiesCommand :: MonadIO m => Mod CommandFields (Command, Action m)
        listEntitiesCommand = command "list-entities" $ info (pure (ListEntities, const $ listEntities ents)) (progDesc "List all known entities")
      |]
    createCommandsDec <- mkCreateCommands mps ents
    readCommandsDec <- mkReadCommands mps ents
    updateCommandsDec <- mkUpdateCommands mps ents
    deleteCommandsDec <- mkDeleteCommands mps ents

    return $ mconcat [
        listEntitiesCommandDec,
        createCommandsDec,
        readCommandsDec,
        updateCommandsDec,
        deleteCommandsDec
      ]

-- | Variant of 'mkPersistCRUD' that takes 'EntityDef's instead of 'UnboundEntityDef's.
mkPersistCRUD'
    :: MkPersistSettings
    -> [EntityDef]
    -> Q [Dec]
mkPersistCRUD' mps ents = mkPersistCRUD mps (map unbindEntityDef ents)


listEntities :: Applicative m => [UnboundEntityDef] -> m PersistValue
listEntities = pure . PersistList . map (PersistText . unEntityNameHS . getEntityHaskellName . unboundEntityDef)


mkCreateCommands :: MkPersistSettings -> [UnboundEntityDef] -> Q [Dec]
mkCreateCommands mps ents = do
  let allCreateCmdExprs = ListE <$> forM ents (mkCreateCommandExpr mps)
  [d| createCommands :: MonadIO m => Mod CommandFields (Command, Action m)
      createCommands = mconcat $(allCreateCmdExprs)
    |]

mkCreateCommandExpr :: MkPersistSettings -> UnboundEntityDef -> Q Exp
mkCreateCommandExpr mps ent = do
  let parserExpr = mkCreateParserExpr mps ent
  [|command ("create-" ++ entityNameString ent)
            (info $(parserExpr)
                  (progDesc $ "Create an instance of " ++ entityNameString ent ++ " entity"))
               |]

mkCreateParserExpr :: MkPersistSettings -> UnboundEntityDef -> Q Exp
mkCreateParserExpr mps ent = do
  let typ_ = pure $ genericDataType mps (entityHaskell (unboundEntityDef ent)) backendT
      -- TODO: ideally we would like to call tabulateEntityA here
      parser = [|traverse fieldToArgument (getEntityFields (entityDef (Nothing :: Maybe $typ_)))|]
      action = [|\(Create args) -> do
          let valOrErr = fromPersistValues args :: Either T.Text $typ_
          case valOrErr of
               Left err -> pure (PersistText err)
               Right val -> toPersistValue <$> insert val
        |]
  [|fmap (\args -> (Create args, $(action))) $(parser)|]


mkReadCommands :: MkPersistSettings -> [UnboundEntityDef] -> Q [Dec]
mkReadCommands mps ents = do
  let allReadCmdExprs = ListE <$> forM ents (mkReadCommandExpr mps)
  [d| readCommands :: MonadIO m => Mod CommandFields (Command, Action m)
      readCommands = mconcat $(allReadCmdExprs)
    |]

mkReadCommandExpr :: MkPersistSettings -> UnboundEntityDef -> Q Exp
mkReadCommandExpr mps ent = do
  let parserExpr = mkReadParserExpr mps ent
  [|command ("read-" ++ entityNameString ent)
            (info $(parserExpr)
                  (progDesc $ "List instances of " ++ entityNameString ent ++ " entity"))
               |]

mkReadParserExpr :: MkPersistSettings -> UnboundEntityDef -> Q Exp
mkReadParserExpr mps ent = do
  let typ_ = pure $ genericDataType mps (entityHaskell (unboundEntityDef ent)) backendT
      parser = [|option auto
                        (short 'l' <> metavar "INT" <> help "Number of rows to limit the output to" <> value 0)
                |]
      action = [|\(Read limit) -> do
          let selectOpts = case limit of
                0 -> []
                _ -> [LimitTo $ fromIntegral limit]
          ents <- selectList [] selectOpts
          pure (PersistList (map toPersistValue (ents :: [Entity $typ_])))
        |]
  [|fmap (\args -> (Read args, $(action))) $(parser)|]


mkUpdateCommands :: MkPersistSettings -> [UnboundEntityDef] -> Q [Dec]
mkUpdateCommands mps ents = do
  let allUpdateCmdExprs = ListE <$> forM ents (mkUpdateCommandExpr mps)
  [d| updateCommands :: MonadIO m => Mod CommandFields (Command, Action m)
      updateCommands = mconcat $(allUpdateCmdExprs)
    |]

mkUpdateCommandExpr :: MkPersistSettings -> UnboundEntityDef -> Q Exp
mkUpdateCommandExpr mps ent = do
  let parserExpr = mkUpdateParserExpr mps ent
  [|command ("update-" ++ entityNameString ent)
            (info $(parserExpr)
                  (progDesc $ "Updates an instance of " ++ entityNameString ent ++ " entity identified by given key with new values"))
               |]

mkUpdateParserExpr :: MkPersistSettings -> UnboundEntityDef -> Q Exp
mkUpdateParserExpr mps ent = do
  let typ_ = pure $ genericDataType mps (entityHaskell (unboundEntityDef ent)) backendT
      parser = [|CRUD.Update <$> mkArg keyArgument <*> traverse fieldToArgument (getEntityFields (entityDef (Nothing :: Maybe $typ_)))
                |]
      action = [|\(CRUD.Update key args) -> do
          let keyOrErr = fromPersistValue key :: Either T.Text (Key $typ_)
              valOrErr = fromPersistValues args :: Either T.Text $typ_
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
  [|fmap (\cmd -> (cmd, $(action))) $(parser)|]


mkDeleteCommands :: MkPersistSettings -> [UnboundEntityDef] -> Q [Dec]
mkDeleteCommands mps ents = do
  let allDeleteCmdExprs = ListE <$> forM ents (mkDeleteCommandExpr mps)
  [d| deleteCommands :: MonadIO m => Mod CommandFields (Command, Action m)
      deleteCommands = mconcat $(allDeleteCmdExprs)
    |]

mkDeleteCommandExpr :: MkPersistSettings -> UnboundEntityDef -> Q Exp
mkDeleteCommandExpr mps ent = do
  let parserExpr = mkDeleteParserExpr mps ent
  [|command ("delete-" ++ entityNameString ent)
            (info $(parserExpr)
                  (progDesc $ "Delete an instance of " ++ entityNameString ent ++ " entity identified by given key"))
               |]

mkDeleteParserExpr :: MkPersistSettings -> UnboundEntityDef -> Q Exp
mkDeleteParserExpr mps ent = do
  let typ_ = pure $ genericDataType mps (entityHaskell (unboundEntityDef ent)) backendT
      parser = [|CRUD.Delete
                    <$> switch (short 'c' <> long "check-if-exists" <> help "Check for key existance before deleting")
                    <*> mkArg keyArgument|]
      action = [|\(CRUD.Delete {..}) -> do
          let keyOrErr = fromPersistValue keyToDelete :: Either T.Text (Key $typ_)
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
  [|fmap (\cmd -> (cmd, $(action))) $(parser)|]


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

