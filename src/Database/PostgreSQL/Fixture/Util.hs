module Database.PostgreSQL.Fixture.Util
  ( augmentEnvironment,
  )
where

import qualified Data.Map.Strict as Dict
import qualified System.Environment as Environment

augmentEnvironment :: [(String, String)] -> IO [(String, String)]
augmentEnvironment overrides = do
  environment <- Environment.getEnvironment
  let overridesDict = Dict.fromList overrides
  let environmentDict = Dict.fromList environment
  let combinedDict = Dict.union overridesDict environmentDict
  pure $ Dict.toList combinedDict
