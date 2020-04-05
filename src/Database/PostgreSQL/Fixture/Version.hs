module Database.PostgreSQL.Fixture.Version
  ( Version (..),
    versionText,
    versionCompare,
    versionParse,
    versionLineParse,
  )
where

import Data.Attoparsec.Text
  ( Parser,
    choice,
    decimal,
    isEndOfLine,
    isHorizontalSpace,
    option,
    parseOnly,
    skipSpace,
    skipWhile,
    skipWhile,
    string,
    takeTill,
  )
import qualified Data.Text as Text
import Data.Text (Text)
import Text.Printf (printf)

data Version
  = Version Int Int (Maybe Int)
  | Unknown Text
  deriving (Eq, Show)

-- | Return the version string.
versionText :: Version -> Text
versionText (Version major minor Nothing) = Text.pack $ printf "%d.%d" major minor
versionText (Version major minor (Just patch)) = Text.pack $ printf "%d.%d.%d" major minor patch
versionText (Unknown version_) = version_ <> " (unknown)"

-- | Parse a version, e.g. `9.6.17`.
versionParse :: Text -> Version
versionParse text =
  case parseOnly versionParser text of
    Left _err -> Unknown text
    Right vn -> vn

-- | Parse a version line.
--
-- e.g. the output from `pg_ctl --version` or `pg_isready --version`.
versionLineParse :: Text -> Version
versionLineParse text =
  case parseOnly versionLineParser text of
    Left _err -> Unknown text
    Right vn -> vn

versionParser :: Parser Version
versionParser = do
  major <- decimal
  minor <- string "." *> decimal
  patch <- option Nothing (fmap Just $ string "." *> decimal)
  pure $ Version major minor patch

versionLineParser :: Parser Version
versionLineParser = do
  skipNonSpace *> skipSpace *> skipNonSpace *> skipSpace
  choice [versionParser, Unknown <$> takeTill isEndOfLine]
  where
    skipNonSpace = skipWhile (not . isHorizontalSpace)

versionCompare :: Version -> Version -> Maybe Ordering
versionCompare a b =
  case (a, b) of
    (Version majora minora patcha, Version majorb minorb patchb) ->
      case compare majora majorb of
        EQ -> case compare minora minorb of
          EQ -> case (patcha, patchb) of
            (Just pa, Just pb) -> Just $ compare pa pb
            (Just _, Nothing) -> Just GT
            (Nothing, Just _) -> Just LT
            (Nothing, Nothing) -> Just EQ
          result -> Just result
        result -> Just result
    (_, _) ->
      Nothing
