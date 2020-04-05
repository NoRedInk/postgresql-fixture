{-# LANGUAGE OverloadedStrings #-}

module Database.PostgreSQL.FixtureSpec
  ( main,
  )
where

import Control.Applicative (pure)
import Data.Acquire (with)
import qualified Database.PostgreSQL.Fixture as Fixture
import qualified Database.PostgreSQL.Fixture.ConsumersSpec as ConsumersSpec
import qualified Database.PostgreSQL.Fixture.Settings as Fixture.Settings
import Database.PostgreSQL.Fixture.Util (augmentEnvironment)
import qualified Database.PostgreSQL.Simple as Simple
import System.Directory (doesDirectoryExist)
import System.IO (IO)
import qualified System.Process.Typed as Process
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)
import Text.Show (show)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Tests"
    [ ephemeralClusterTests,
      ConsumersSpec.consumersTests
    ]

ephemeralClusterTests :: TestTree
ephemeralClusterTests =
  testGroup
    "ephemeralCluster"
    [ testCase "the cluster is ready" $ do
        with Fixture.ephemeralCluster pgIsReady,
      testCase "the cluster can be connected to with the returned settings" $ do
        results <-
          with
            Fixture.ephemeralCluster
            ( \ephemeralConnectionSettings ->
                with
                  (Fixture.simpleConnection ephemeralConnectionSettings)
                  (\ephemeralConnection -> Simple.query_ ephemeralConnection "SELECT 1234")
            )
        assertEqual "" [Simple.Only (1234 :: Int)] results,
      testCase "cluster is created in directory reported in `pgHost` field" $ do
        pgHostDirExists <-
          with
            Fixture.ephemeralCluster
            (doesDirectoryExist . Fixture.Settings.unPgHost . Fixture.Settings.pgHost)
        assertBool "" pgHostDirExists,
      testCase "cluster directory is removed on release" $ do
        pgHostDir <-
          with
            Fixture.ephemeralCluster
            (pure . Fixture.Settings.unPgHost . Fixture.Settings.pgHost)
        pgHostDirExists <- doesDirectoryExist pgHostDir
        assertBool "" (not pgHostDirExists)
    ]

pgIsReady :: Fixture.Settings.ConnectionSettings -> IO ()
pgIsReady settings = do
  environment <-
    augmentEnvironment
      [ ("PGHOST", Fixture.Settings.unPgHost $ Fixture.Settings.pgHost settings),
        ("PGPORT", show $ Fixture.Settings.unPgPort $ Fixture.Settings.pgPort settings),
        ("PGPASSWORD", ""),
        ("PGUSER", Fixture.Settings.unPgUser $ Fixture.Settings.pgUser settings),
        ("PGDATABASE", Fixture.Settings.unPgDatabase $ Fixture.Settings.pgDatabase settings)
      ]
  Process.runProcess_
    ( Process.setEnv environment $
        Process.proc "pg_isready" ["--quiet", "--timeout=20"]
    )
