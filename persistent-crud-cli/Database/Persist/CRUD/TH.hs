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

import Database.Persist.CRUD.Types

-- | For each <Entity> creates definitions for:
--    * list-<Entity> functions
--    * create-<Entity> functions
-- | Also creates following definitions of type 'Mod CommandFields (Command, Action m)':
--    * listCommands
--    * createCommands
mkPersistCRUD
    :: MkPersistSettings
    -> [EntityDef]
    -> Q [Dec]
mkPersistCRUD mps ents = do
    listCommandsDec <- mkListCommands mps (map unbindEntityDef ents)
    createCommandsDec <- mkCreateCommands mps (map unbindEntityDef ents)

    return $ mconcat [
        listCommandsDec,
        createCommandsDec
      ]

mkListCommands :: MkPersistSettings -> [UnboundEntityDef] -> Q [Dec]
mkListCommands mps ents = do
  let allListCmdExprs = ListE <$> forM ents (mkListCommandExpr mps)
  [d| listCommands :: MonadIO m => Mod CommandFields (Command, Action m)
      listCommands = mconcat $(allListCmdExprs)
    |]

mkListCommandExpr :: MkPersistSettings -> UnboundEntityDef -> Q Exp
mkListCommandExpr mps ent = do
  let parserExpr = mkListParserExpr mps ent
  [|command ("list-" ++ entityNameString ent)
            (info $(parserExpr)
                  (progDesc $ "List instances of " ++ entityNameString ent ++ " entity"))
               |]

mkListParserExpr :: MkPersistSettings -> UnboundEntityDef -> Q Exp
mkListParserExpr mps ent = do
  let typ_ = pure $ genericDataType mps (entityHaskell (unboundEntityDef ent)) backendT
      action = [|\List -> do
          ents <- selectList [] []
          pure (PersistList (map (PersistList . toPersistFields . entityVal) (ents :: [Entity $typ_])))
        |]
  [|pure (List, $(action))|]


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

