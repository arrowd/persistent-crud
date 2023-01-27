{-# LANGUAGE TemplateHaskellQuotes #-}
module Database.Persist.CRUD.Options(
    module Options.Applicative,

    textArgument,
    int32Argument,
    int64Argument,
    boolArgument,
    timeArgument,

    relaxedBoolReadM
  )
where

import Options.Applicative
import qualified Data.Text as T
import Database.Persist.PersistValue
import Language.Haskell.TH.Syntax

textArgument :: Q Exp
textArgument = [|argument (PersistText . T.pack <$> str) (metavar "TEXT")|]
int32Argument :: Q Exp
int32Argument = [|argument (PersistInt64 <$> auto) (metavar "INT")|]
int64Argument :: Q Exp
int64Argument = [|argument (PersistInt64 <$> auto) (metavar "INT")|]
boolArgument :: Q Exp
boolArgument = [|argument (PersistBool <$> relaxedBoolReadM) (metavar "BOOL")|]
timeArgument :: Q Exp
timeArgument = [|argument (PersistUTCTime <$> maybeReader (\str -> case span (/= '#') str of
    (format, _:timeVal) -> parseTimeM True defaultTimeLocale format timeVal
    _ -> Nothing
    )) (metavar "FORMAT#TIME")
  |]

relaxedBoolReadM :: ReadM Bool
relaxedBoolReadM = auto
    <|> maybeReader lowercaseBool
    <|> integerBool
  where
    lowercaseBool "true" = Just True
    lowercaseBool "false" = Just False
    lowercaseBool _ = Nothing
    integerBool = auto >>= \case
        0 -> pure False
        1 -> pure True
        _ -> empty
