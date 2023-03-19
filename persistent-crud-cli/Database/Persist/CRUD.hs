{-# LANGUAGE OverloadedStrings #-}
module Database.Persist.CRUD (
    module Database.Persist.CRUD.TH,
    module Database.Persist.CRUD.Types,
    helpCommand
  )
where

import Database.Persist
import Database.Persist.Quasi.Internal (UnboundEntityDef, unboundEntityDef)
import Database.Persist.CRUD.TH
import Database.Persist.CRUD.Types
import Options.Applicative
import Options.Applicative.Help.Pretty

helpCommand = command "help" $ info args (progDesc "Print help on how to values for various types")
  where
    args = argument disabled (hidden <> metavar "TEXT" <> help "Arbitrary text values. To pass an empty string use \"\" for Unix-like shells and '\"\"' or \"''\" for PowerShell.")
        <|> argument disabled (hidden <> metavar "INT" <> help "Positive and negative integers. To pass hex values use the \"0x\" prefix.")
        <|> argument disabled (hidden <> metavar "KEY" <> help "Same as INT.")
        <|> argument disabled (hidden <> metavar "BOOL" <> help "Boolean values. \"True\", \"true\" and \"1\" forms are allowed.")
        <|> argument disabled (hidden <> metavar "FORMAT#TIME" <> helpDoc timeHelp)
        <|> argument disabled (hidden <> metavar "(# | SOMETYPE)" <> help "Either SOMETYPE value or the \"#\" symbol that corresponds to Haskell's Nothing and SQL's NULL.")
        <|> argument disabled (hidden <> metavar "FILTER" <> helpDoc filterHelp)
    timeHelp = Just $ vsep [
        "Time fields are specified in the \"format#value\" form" :: Doc,
        "All possible format options are listed at https://hackage.haskell.org/package/time/docs/Data-Time-Format.html#v:formatTime",
        "Some common examples:",
        "%s#123\t\t\tspecify time as Unix epoch",
        "%D#12/31/23\t\t\tspecify a triplet of month/day/year",
        "%F#2023-12-31\t\tspecify a triplet of year-month-day",
        "%c#Mon Feb 14 15:45:55 +0300 2023\tmost elaborate way to specify a date"
      ]
    filterHelp = Just $ vsep [
        "Filters are triplets consisting of \"field\", \"operator\" and \"value\" separated by \"&\"" :: Doc,
        "Multiple \"-f\" flags are combined using OR operator",
        "Field is a CamelCase string in form of \"<EntityName><FieldName>\"",
        "Example:\t\t\t\"UserId\", \"EmailVerified\", etc.",
        "Operator is one of \"=\" or \"==\", \"!=\", \">\", \"<\", \">=\", \"<=\"",
        "Value is parsed by the same rules as types listed above",
        "Complete filter examples:",
        "\tUserName=Alice",
        "\tUserAge>=20&UserEmail!=#"
      ]
