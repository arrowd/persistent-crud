{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ExplicitForAll #-}
module Database.Persist.CRUD.Core.TH(
    mkPersistCRUD,
    mkPersistCRUD',
    listEntities
  )
where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader.Class
import Data.Char (toUpper, toLower)
import Data.Dynamic
import qualified Data.List as L
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

-- | For each <Entity> creates definitions for:
--    * Functions of type 'Action m' named
--        * create<Entity>Action
--        * read<Entity>Action
--        * update<Entity>Action
--        * delete<Entity>Action
mkPersistCRUD
    :: MkPersistSettings
    -> [UnboundEntityDef]
    -> Q [Dec]
mkPersistCRUD mps ents = fmap concat $ forM ents $ \ent -> do
    createActDec <- mkActionDec mps "create" createActionExp ent
    readActDec <- mkActionDec mps "read" readActionExp ent
    updateActDec <- mkActionDec mps "update" updateActionExp ent
    deleteActDec <- mkActionDec mps "delete" deleteActionExp ent
    return $ concat [createActDec, readActDec, updateActDec, deleteActDec]

-- | Variant of 'mkPersistCRUD' that takes 'EntityDef's instead of 'UnboundEntityDef's.
mkPersistCRUD'
    :: MkPersistSettings
    -> [EntityDef]
    -> Q [Dec]
mkPersistCRUD' mps ents = mkPersistCRUD mps (map unbindEntityDef ents)


listEntities :: Applicative m => [UnboundEntityDef] -> m PersistValue
listEntities = pure . PersistList . map (PersistText . unEntityNameHS . getEntityHaskellName . unboundEntityDef)


-- | The body that gets spliced into
-- create<Entity>Action cmd = { ... }
-- declaration.
createActionExp :: Q Type
                -- | ^ Type representing a Persistent Entity, like \"User\"
                -> Q Exp
createActionExp recordType = [|
    do
      let Create args = cmd
          valOrErr = fromPersistValues args :: Either T.Text $recordType
      case valOrErr of
          Left err -> pure (PersistText err)
          Right val -> toPersistValue <$> insert val
  |]

-- | The body that gets spliced into
-- read<Entity>Action cmd = { ... }
-- declaration.
readActionExp :: Q Type -> Q Exp
readActionExp recordType = [|
    do
      let Read {..} = cmd
          selectOpts = case readLimitTo of
            0 -> []
            _ -> [LimitTo $ fromIntegral readLimitTo]
          filter' = map (FilterOr . (map (`fromDyn` undefined))) filter :: [Filter $recordType]
      ents <- selectList filter' selectOpts
      pure (PersistList (map toPersistValue (ents :: [Entity $recordType])))
  |]

-- | The body that gets spliced into
-- update<Entity>Action cmd = { ... }
-- declaration.
updateActionExp :: Q Type -> Q Exp
updateActionExp recordType = [|
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

-- | The body that gets spliced into
-- delete<Entity>Action cmd = { ... }
-- declaration.
deleteActionExp :: Q Type -> Q Exp
deleteActionExp recordType = [|
    do
      let CRUD.Delete {..} = cmd
          filter' = map (FilterOr . (map (`fromDyn` undefined))) filter :: [Filter $recordType]
      case (null filter', confirmDeleteEverything) of
        (True, True) -> do
          deleteWhere ([] :: [Filter $recordType])
          pure $ PersistBool True
        (False, _) -> do
          deleteWhere filter'
          pure $ PersistBool True
        _ -> pure $ PersistBool False
  |]

-- | Type for CRUD actions: \"forall m . MonadIO m => Action m\"
actionDefinitionTy :: Q Type
actionDefinitionTy = [t|
    forall m . MonadIO m => Action m
  |]


-- | Creates a declaration named \"<actionName><Entity>Action\"
mkActionDec :: MkPersistSettings -> String -> (Q Type -> Q Exp) -> UnboundEntityDef -> Q [Dec]
mkActionDec mps actionName mkAction ent = do
  let funcName = mkDecName actionName "Action" ent
      recordType = pure $ genericDataType mps (entityHaskell (unboundEntityDef ent)) backendT

  defType <- actionDefinitionTy
  body <- mkAction recordType

  let -- <actionName><Entity>Action :: forall m . MonadIO m => Action m
      signature = SigD funcName defType
      -- <actionName><Entity>Action cmd = $(<actionName>ActionExp)
      definition = FunD funcName [Clause [VarP $ mkName "cmd"] (NormalB body) []]

  return [signature, definition]


mkDecName :: String -> String -> UnboundEntityDef -> Name
mkDecName prefix suffix ent = mkName (prefix <> entName <> suffix)
  where
    entName = T.unpack $ if null prefix
                 then upperFirst $ getEntName ent
                 else getEntName ent
    getEntName = unEntityNameHS . getEntityHaskellName . unboundEntityDef
