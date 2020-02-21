module Database.PostgreSQL.FixtureSpec (tests) where

import Data.Acquire (with)
import qualified Database.PostgreSQL.Simple as Simple
import qualified Expect
import qualified MySQL.Internal
import Nri.Prelude
import qualified Postgres.Internal
import Postgres.Internal (augmentEnvironment)
import qualified Postgres.Settings
import System.Directory (doesDirectoryExist)
import qualified System.Process.Typed as Process
import Test (Test, describe, test)

tests :: Test
tests =
  describe
    "Internal PostgreSQL tests"
    [ ephemeralClusterTests,
      setupProxyDatabaseTests
    ]

ephemeralClusterTests :: Test
ephemeralClusterTests =
  describe
    "ephemeralCluster"
    [ test "the cluster is ready" <| \() ->
        with Postgres.Internal.ephemeralCluster pgIsReady
          |> Expect.withIO (always Expect.pass),
      test "the cluster can be connected to with the returned settings" <| \() ->
        with
          Postgres.Internal.ephemeralCluster
          ( \ephemeralConnectionSettings ->
              with
                (Postgres.Internal.simpleConnection ephemeralConnectionSettings)
                (\ephemeralConnection -> Simple.query_ ephemeralConnection "SELECT 1234")
          )
          |> Expect.withIO (Expect.equal [Simple.Only (1234 :: Int)]),
      test "cluster is created in directory reported in `pgHost` field" <| \() ->
        with
          Postgres.Internal.ephemeralCluster
          (Postgres.Settings.pgHost >> Postgres.Settings.unPgHost >> toS >> doesDirectoryExist)
          |> Expect.withIO Expect.true,
      test "cluster directory is removed on release" <| \() ->
        with
          Postgres.Internal.ephemeralCluster
          (Postgres.Settings.pgHost >> Postgres.Settings.unPgHost >> toS >> pure)
          |> andThen doesDirectoryExist
          |> Expect.withIO Expect.false
    ]

setupProxyDatabaseTests :: Test
setupProxyDatabaseTests =
  describe
    "setupProxyDatabase"
    [ test "it can be connected to" <| \() ->
        with
          MySQL.Internal.ephemeralMySQL
          ( \mysqlSettings ->
              with
                ( Postgres.Internal.setupProxyDatabase
                    []
                    (Postgres.Settings.defaultSettings |> Postgres.Settings.pgConnection)
                    mysqlSettings
                )
                pgIsReady
          )
          |> Expect.withIO (always Expect.pass),
      test "it can import foreign postgres schemas" <| \() ->
        with
          MySQL.Internal.ephemeralMySQL
          ( \mysqlSettings ->
              with
                Postgres.Internal.ephemeralCluster
                ( \ephemeralConnectionSettings -> do
                    with (Postgres.Internal.simpleConnection ephemeralConnectionSettings)
                      <| \connection ->
                        void <| Simple.execute_ connection "CREATE SCHEMA test_schema"
                    with
                      ( Postgres.Internal.setupProxyDatabase
                          ["test_schema"]
                          ephemeralConnectionSettings
                          mysqlSettings
                      )
                      pgIsReady
                )
          )
          |> Expect.withIO (always Expect.pass)
    ]

pgIsReady :: Postgres.Settings.ConnectionSettings -> IO ()
pgIsReady settings = do
  environment <-
    augmentEnvironment
      [ ("PGHOST", Postgres.Settings.pgHost settings |> Postgres.Settings.unPgHost |> toS),
        ("PGPORT", Postgres.Settings.pgPort settings |> Postgres.Settings.unPgPort |> show),
        ("PGPASSWORD", ""),
        ("PGUSER", Postgres.Settings.pgUser settings |> Postgres.Settings.unPgUser |> toS),
        ("PGDATABASE", Postgres.Settings.pgDatabase settings |> Postgres.Settings.unPgDatabase |> toS)
      ]
  Process.runProcess_
    ( Process.proc "pg_isready" []
        |> Process.setEnv environment
    )
