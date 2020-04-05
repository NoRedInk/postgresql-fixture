{-# LANGUAGE OverloadedStrings #-}

module Database.PostgreSQL.Fixture.ConsumersSpec
  ( consumersTests,
  )
where

import Data.Acquire (with)
import Database.PostgreSQL.Fixture.Consumers
  ( Consumer (Consumer),
    ConsumerType (Persistent, Runtime),
    Resource (Resource),
    acquire,
    inUse,
    release,
  )
import Database.PostgreSQL.Fixture.Util (temporaryDirectory)
import System.Directory (listDirectory)
import qualified System.Posix.Process (getProcessID)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
  ( Assertion,
    HasCallStack,
    assertBool,
    assertEqual,
    testCase,
    testCaseSteps,
  )

consumersTests :: TestTree
consumersTests =
  testGroup
    "consumers"
    [ acquireTests,
      releaseTests,
      inUseTests
    ]

acquireTests :: TestTree
acquireTests =
  testGroup
    "acquire and release"
    [ testCase
        "acquire persistent consumer"
        ( assertEqual "" ["foobar"]
            =<< (with temporaryDirectory)
              ( \tempDir -> do
                  let resource = Resource tempDir
                  acquire $ Consumer resource (Persistent "foobar")
                  listDirectory tempDir
              )
        ),
      testCase
        "acquire runtime consumer"
        ( do
            pid <- System.Posix.Process.getProcessID
            assertEqual "" ["pid." <> show pid]
              =<< (with temporaryDirectory)
                ( \tempDir -> do
                    let resource = Resource tempDir
                    acquire $ Consumer resource Runtime
                    listDirectory tempDir
                )
        )
    ]

releaseTests :: TestTree
releaseTests =
  testGroup
    "release"
    [ testCase
        "release persistent consumer"
        ( assertEmpty
            =<< (with temporaryDirectory)
              ( \tempDir -> do
                  let resource = Resource tempDir
                  let consumer = Consumer resource $ Persistent "foobar"
                  acquire consumer >> release consumer
                  listDirectory tempDir
              )
        ),
      testCase
        "release runtime consumer"
        ( assertEmpty
            =<< (with temporaryDirectory)
              ( \tempDir -> do
                  let resource = Resource tempDir
                  let consumer = Consumer resource Runtime
                  acquire consumer >> release consumer
                  listDirectory tempDir
              )
        )
    ]

inUseTests :: TestTree
inUseTests =
  testGroup
    "inUse"
    [ testCase
        "persistent consumer"
        ( assertTrue "not in use"
            =<< (with temporaryDirectory)
              ( \tempDir -> do
                  let resource = Resource tempDir
                  acquire (Consumer resource (Persistent "foobar"))
                  inUse resource
              )
        ),
      testCase
        "runtime consumer"
        ( assertTrue "not in use"
            =<< (with temporaryDirectory)
              ( \tempDir -> do
                  let resource = Resource tempDir
                  acquire (Consumer resource Runtime)
                  inUse resource
              )
        ),
      testCase
        "persistent consumer, released"
        ( assertFalse "in use"
            =<< (with temporaryDirectory)
              ( \tempDir -> do
                  let resource = Resource tempDir
                  let consumer = Consumer resource (Persistent "foobar")
                  acquire consumer >> release consumer
                  inUse resource
              )
        ),
      testCase
        "runtime consumer, released"
        ( assertFalse "in use"
            =<< (with temporaryDirectory)
              ( \tempDir -> do
                  let resource = Resource tempDir
                  let consumer = Consumer resource Runtime
                  acquire consumer >> release consumer
                  inUse resource
              )
        ),
      testCaseSteps "acquire, release, interleaved" $ \step ->
        with temporaryDirectory $ \tempDir -> do
          let resource = Resource tempDir
          let consumer1 = Consumer resource (Persistent "foobar")
          let consumer2 = Consumer resource Runtime
          --
          step "before acquiring resource"
          assertFalse "in use" =<< inUse resource
          --
          step "first consumer acquire"
          acquire consumer1
          assertTrue "not in use" =<< inUse resource
          --
          step "second consumer acquire"
          acquire consumer2
          assertTrue "not in use" =<< inUse resource
          --
          step "first consumer release"
          release consumer1
          assertTrue "not in use" =<< inUse resource
          --
          step "second consumer release"
          release consumer2
          assertFalse "in use" =<< inUse resource
    ]

assertTrue :: HasCallStack => String -> Bool -> Assertion
assertTrue message =
  assertBool message

assertFalse :: HasCallStack => String -> Bool -> Assertion
assertFalse message =
  assertBool message . not

assertEmpty :: HasCallStack => [a] -> Assertion
assertEmpty = assertBool "List is not empty" . null
