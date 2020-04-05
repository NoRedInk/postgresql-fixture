module Main (main) where

import qualified Database.PostgreSQL.Fixture.ConsumersSpec as ConsumersSpec
import qualified Database.PostgreSQL.FixtureSpec as FixtureSpec
import Test.Tasty (TestTree, defaultMain, testGroup)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Tests"
    [ FixtureSpec.ephemeralClusterTests,
      ConsumersSpec.consumersTests
    ]
