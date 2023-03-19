{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
module Database.Persist.CRUD.Core.Stolen where

import Data.Char (toUpper)
import qualified Data.Text as T
import Language.Haskell.TH.Syntax

import Database.Persist.EntityDef.Internal (EntityDef(..))
import Database.Persist.Names
import Database.Persist.Quasi.Internal
import Database.Persist.TH

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

upperFirst :: T.Text -> T.Text
upperFirst t =
    case T.uncons t of
        Just (a, b) -> T.cons (toUpper a) b
        Nothing -> t

filterConName
    :: MkPersistSettings
    -> UnboundEntityDef
    -> UnboundFieldDef
    -> String
filterConName mps (unboundEntityDef -> entity) field =
    filterConName' mps (entityHaskell entity) (unboundFieldNameHS field)

filterConName'
    :: MkPersistSettings
    -> EntityNameHS
    -> FieldNameHS
    -> String
filterConName' mps entity field = T.unpack name
    where
        name
            | field == FieldNameHS "Id" = entityName <> fieldName
            | mpsPrefixFields mps       = modifiedName
            | otherwise                 = fieldName

        modifiedName = mpsConstraintLabelModifier mps entityName fieldName
        entityName = unEntityNameHS entity
        fieldName = upperFirst $ unFieldNameHS field
