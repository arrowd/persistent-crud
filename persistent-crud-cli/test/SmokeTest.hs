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

import Data.Text
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
|]

optsParser :: Parser (Command, Action IO)
optsParser = hsubparser (createCommands
    <> readCommands
    <> updateCommands
    )

main = do
  args <- getArgs
  let parseResult = execParserPure (prefs showHelpOnEmpty) (info optsParser idm) args
  case parseResult of
    Success (cmd, _) -> print cmd
    Failure f -> print $ (\(x, _, _) -> x) $ execFailure f ""
