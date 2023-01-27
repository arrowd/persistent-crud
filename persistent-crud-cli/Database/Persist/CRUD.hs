{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Database.Persist.CRUD (
    module Database.Persist.CRUD.TH,
    module Database.Persist.CRUD.Types,
    helpCommand,
    fieldToArgument
  )
where

import Database.Persist
import Database.Persist.Quasi.Internal (UnboundEntityDef, unboundEntityDef)
import Database.Persist.CRUD.TH
import Database.Persist.CRUD.Types
import Options.Applicative
import Options.Applicative.Help.Pretty

helpCommand = command "help" $ info args (progDesc "Print help on passing time values")
  where
    args = argument disabled (hidden <> metavar "TEXT" <> help "Arbitrary text values. To pass an empty string use \"\" for Unix-like shells and '\"\"' or \"''\" for PowerShell")
        <|> argument disabled (hidden <> metavar "INT" <> help "Positive and negative integers. To pass hex values use \"0x\" prefix.")
        <|> argument disabled (hidden <> metavar "BOOL" <> help "Boolean values. \"True\", \"true\" and \"1\" forms are allowed.")
        <|> argument disabled (hidden <> metavar "FORMAT#TIME" <> helpDoc timeHelp)
    timeHelp = Just $ vsep [
        "Time fields are specified in the \"format#value\" form" :: Doc,
        "All format options are listed at https://hackage.haskell.org/package/time/docs/Data-Time-Format.html#v:formatTime",
        "Some common examples:",
        "%s#123\t\t\tspecify time as Unix epoch",
        "%D#12/31/23\t\t\tspecify a triplet of month/day/year",
        "%F#2023-12-31\t\tspecify a triplet of year-month-day",
        "%c#Mon Feb 14 15:45:55 +0300 2023\tmost elaborate way to specify a date"
      ]

-- TODO: This is a suboptimal solution until it'd be possible to call 'tabulateEntityA' in the TH code
-- We also leak 'fieldToArgument' outside
fieldToArgument :: FieldDef -> Parser PersistValue
fieldToArgument field = case fieldType field of
    FTTypeCon _ "Text" -> $textArgument
    FTTypeCon _ "String" -> $textArgument
    FTTypeCon _ "Int" -> $intArgument
    FTTypeCon _ "Bool" -> $boolArgument
    FTTypeCon _ "UTCTime" -> $timeArgument
