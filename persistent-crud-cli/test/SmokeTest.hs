{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.Reader
import Data.Text
import Data.Time.Clock
import Database.Persist.TH
import Database.Persist.CRUD
import Options.Applicative
import System.Environment

share [mkPersist sqlSettings, mkPersistCRUD sqlSettings] [persistLowerCase|
User
  name    Text
  age     Int

Dog
  name    Text
  owner   UserId

Contract
  started UTCTime
|]

optsParser :: Parser (Command, Action IO)
optsParser = hsubparser (listEntitiesCommand
    <> createCommands
    <> readCommands
    <> updateCommands
    <> helpCommand
    )

main = do
  args <- getArgs
  let parseResult = execParserPure (prefs showHelpOnEmpty) (info optsParser idm) args
  case parseResult of
    Success (ListEntities, act) -> runReaderT (act undefined) undefined >>= print
    Success (cmd, _) -> print cmd
    Failure f -> print $ (\(x, _, _) -> x) $ execFailure f ""
