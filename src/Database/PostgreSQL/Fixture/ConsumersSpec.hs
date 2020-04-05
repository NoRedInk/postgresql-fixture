{-# LANGUAGE OverloadedStrings #-}

module Database.PostgreSQL.Fixture.ConsumersSpec
  ( consumersTests,
  )
where

import Data.Acquire (with)
import Database.PostgreSQL.Fixture.Consumers
  ( Consumer (Consumer),
    ConsumerType (Persistent, Runtime),
    acquire,
    release,
  )
import Database.PostgreSQL.Fixture.Util (temporaryDirectory)
import System.Directory (listDirectory)
import qualified System.Posix.Process (getProcessID)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, HasCallStack, assertBool, assertEqual, testCase)

consumersTests :: TestTree
consumersTests =
  testGroup
    "acquire and release"
    [ testCase
        "acquire persistent consumer"
        ( assertEqual "" ["foobar"]
            =<< (with temporaryDirectory)
              ( \tempDir ->
                  acquire (Consumer tempDir (Persistent "foobar"))
                    >> listDirectory tempDir
              )
        ),
      testCase
        "acquire runtime consumer"
        ( do
            pid <- System.Posix.Process.getProcessID
            assertEqual "" ["pid." <> show pid]
              =<< (with temporaryDirectory)
                ( \tempDir ->
                    acquire (Consumer tempDir Runtime)
                      >> listDirectory tempDir
                )
        ),
      testCase
        "release persistent consumer"
        ( assertEmpty
            =<< (with temporaryDirectory)
              ( \tempDir -> do
                  let consumer = Consumer tempDir $ Persistent "foobar"
                  acquire consumer >> release consumer >> listDirectory tempDir
              )
        ),
      testCase
        "release runtime consumer"
        ( assertEmpty
            =<< (with temporaryDirectory)
              ( \tempDir -> do
                  let consumer = Consumer tempDir Runtime
                  acquire consumer >> release consumer >> listDirectory tempDir
              )
        )
    ]

assertEmpty :: HasCallStack => [a] -> Assertion
assertEmpty = assertBool "List is not empty" . null
