module Main where

import Data.Semigroup ((<>))
import Options.Applicative

data Options
  = Options
      { hello :: String,
        quiet :: Bool,
        enthusiasm :: Int
      }
  deriving (Show)

optionsParser :: Parser Options
optionsParser =
  Options
    <$> strOption
      ( long "hello"
          <> metavar "TARGET"
          <> help "Target for the greeting"
      )
    <*> switch
      ( long "quiet"
          <> short 'q'
          <> help "Whether to be quiet"
      )
    <*> option
      auto
      ( long "enthusiasm"
          <> help "How enthusiastically to greet"
          <> showDefault
          <> value 1
          <> metavar "INT"
      )

exec :: Options -> IO ()
exec options = do
  putStrLn $ ">>>" ++ show options ++ "<<<"
  pure ()

main :: IO ()
main = do
  execParser opts >>= exec
  where
    opts =
      info
        optionsParser
        (fullDesc <> progDesc "Hello")
