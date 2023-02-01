module Database.Persist.CRUD.Options(
    module Options.Applicative,
    optionMod,
    propMetaVar,

    mkArg,

    textArgument,
    keyArgument,
    intArgument,
    boolArgument,
    timeArgument,
    maybeArgument,

    relaxedBoolReadM
  )
where

import Options.Applicative
import Options.Applicative.Types
import Options.Applicative.Builder.Internal (optionMod)
import qualified Data.Text as T
import Data.Time.Format
import Database.Persist.PersistValue
import Language.Haskell.TH.Syntax

nothingSymbol :: String
nothingSymbol = "#"

mkArg = uncurry argument

textArgument :: (ReadM PersistValue, Mod ArgumentFields PersistValue)
textArgument = (PersistText . T.pack <$> str, metavar "TEXT")

keyArgument :: (ReadM PersistValue, Mod ArgumentFields PersistValue)
keyArgument = (PersistInt64 <$> auto, metavar "KEY")

intArgument :: (ReadM PersistValue, Mod ArgumentFields PersistValue)
intArgument = (PersistInt64 <$> auto, metavar "INT")

boolArgument :: (ReadM PersistValue, Mod ArgumentFields PersistValue)
boolArgument = (PersistBool <$> relaxedBoolReadM, metavar "BOOL")

timeArgument :: (ReadM PersistValue, Mod ArgumentFields PersistValue)
timeArgument = (PersistUTCTime <$> maybeReader (\str -> case span (/= '#') str of
    (format, _:timeVal) -> parseTimeM True defaultTimeLocale format timeVal
    _ -> Nothing
    ), metavar "FORMAT#TIME")

maybeArgument :: (ReadM PersistValue, Mod ArgumentFields PersistValue)
maybeArgument = (PersistNull <$ exactReadM nothingSymbol, metavar nothingSymbol)


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

exactReadM v = maybeReader $ \arg -> if v == arg then Just () else Nothing
