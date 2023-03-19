module Database.Persist.CRUD.Options(
    module Options.Applicative,
    readerAsk,
    maybeReader',
    ReadM(..),

    mkArg,
    isFilterSymbol,

    textArgument,
    keyArgument,
    intArgument,
    boolArgument,
    timeArgument,
    maybeArgument,

    maybefy,
    readPersistFilterMaybe,
    relaxedBoolReadM,
    exactReadM
  )
where

import Options.Applicative
import Options.Applicative.Types
import Options.Applicative.Builder.Internal (optionMod)
import qualified Data.Text as T
import Data.Time.Format
import Database.Persist.PersistValue
import Database.Persist.Types as PT
import Language.Haskell.TH.Syntax


maybeReader' :: Maybe a -> ReadM a
maybeReader' (Just x) = pure x
maybeReader' _ = readerAbort $ ErrorMsg "maybeReader': Nothing"


nothingSymbol :: Char
nothingSymbol = '#'

mkArg = uncurry argument

isFilterSymbol '=' = True
isFilterSymbol '!' = True
isFilterSymbol '<' = True
isFilterSymbol '>' = True
isFilterSymbol '/' = True
isFilterSymbol _ = False


textArgument :: (ReadM PersistValue, Mod ArgumentFields PersistValue)
textArgument = (PersistText . T.pack <$> str, metavar "TEXT")

keyArgument :: (ReadM PersistValue, Mod ArgumentFields PersistValue)
keyArgument = (PersistInt64 <$> auto, metavar "KEY")

intArgument :: (ReadM PersistValue, Mod ArgumentFields PersistValue)
intArgument = (PersistInt64 <$> auto, metavar "INT")

boolArgument :: (ReadM PersistValue, Mod ArgumentFields PersistValue)
boolArgument = (PersistBool <$> relaxedBoolReadM, metavar "BOOL")

timeArgument :: (ReadM PersistValue, Mod ArgumentFields PersistValue)
timeArgument = (PersistUTCTime <$> maybeReader (\str -> case span (/= nothingSymbol) str of
    (format, _:timeVal) -> parseTimeM True defaultTimeLocale format timeVal
    _ -> Nothing
    ), metavar ("FORMAT" ++ nothingSymbol : "TIME"))

maybeArgument :: (ReadM PersistValue, Mod ArgumentFields PersistValue)
maybeArgument = (PersistNull <$ exactReadM [nothingSymbol], metavar [nothingSymbol])

maybefy (valReader, valMod) = (maybeReader <|> valReader, valMod <> optionMod prependMaybeMetavar)
  where
    (maybeReader, _) = maybeArgument
    prependMaybeMetavar optProps = optProps { propMetaVar = "(# | " <> propMetaVar optProps <> ")"}


readPersistFilterMaybe :: String -> Maybe PersistFilter
readPersistFilterMaybe "=" = pure PT.Eq
readPersistFilterMaybe "==" = pure PT.Eq
readPersistFilterMaybe "!=" = pure PT.Ne
readPersistFilterMaybe ">" = pure PT.Gt
readPersistFilterMaybe "<" = pure PT.Lt
readPersistFilterMaybe ">=" = pure PT.Ge
readPersistFilterMaybe "<=" = pure PT.Le
readPersistFilterMaybe _ = Nothing

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

exactReadM v = maybeReader $ \arg -> if v == arg then Just arg else Nothing
