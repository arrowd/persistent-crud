{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
module Database.Persist.CRUD.TH(
    mkPersistCRUD
  )
where

import Control.Monad
import Control.Monad.IO.Class
import qualified Data.Text as T

import Language.Haskell.TH.Syntax

import Options.Applicative

import Database.Persist
import Database.Persist.Quasi.Internal
import Database.Persist.TH
import Database.Persist.EntityDef.Internal (EntityDef(..))

import Database.Persist.CRUD.Types as CRUD

-- | For each <Entity> creates definitions for:
--    * create-<Entity> functions
--    * read-<Entity> functions
-- | Also creates following definitions of type 'Mod CommandFields (Command, Action m)':
--    * createCommands
--    * readCommands
mkPersistCRUD
    :: MkPersistSettings
    -> [EntityDef]
    -> Q [Dec]
mkPersistCRUD mps ents = do
    createCommandsDec <- mkCreateCommands mps (map unbindEntityDef ents)
    readCommandsDec <- mkReadCommands mps (map unbindEntityDef ents)
    updateCommandsDec <- mkUpdateCommands mps (map unbindEntityDef ents)

    return $ mconcat [
        createCommandsDec,
        readCommandsDec,
        updateCommandsDec
      ]


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
      action = [|\Read -> do
          ents <- selectList [] []
          pure (PersistList (map (PersistList . toPersistFields . entityVal) (ents :: [Entity $typ_])))
        |]
  [|pure (Read, $(action))|]


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
      parser = [|(:) <$> intArgument <*> traverse fieldToArgument (getEntityFields (entityDef (Nothing :: Maybe $typ_)))
                |]
      action = [|\(CRUD.Update (key:args)) -> do
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
  [|fmap (\args -> (CRUD.Update args, $(action))) $(parser)|]

entityNameString :: UnboundEntityDef -> String
entityNameString = T.unpack . unEntityNameHS . getEntityHaskellName . unboundEntityDef

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

