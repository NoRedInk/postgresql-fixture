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
    [ acquireTest "acquire persistent consumer" $ Persistent "foobar",
      acquireTest "acquire runtime consumer" Runtime,
      testCase "acquire persistent consumer, naming"
        $ with temporaryDirectory
        $ \tempDir -> do
          let resource = Resource tempDir
          acquire $ Consumer resource $ Persistent "alice"
          assertEqual "" ["alice"] =<< listDirectory tempDir,
      testCase "acquire runtime consumer, naming"
        $ with temporaryDirectory
        $ \tempDir -> do
          pid <- System.Posix.Process.getProcessID
          let resource = Resource tempDir
          acquire $ Consumer resource Runtime
          assertEqual "" ["pid." <> show pid] =<< listDirectory tempDir
    ]
  where
    acquireTest desc consumerType =
      testCaseSteps desc $ \step ->
        with temporaryDirectory $ \tempDir -> do
          let resource = Resource tempDir
          let consumer = Consumer resource consumerType
          --
          step "acquire"
          acquire consumer
          assertNonEmpty =<< listDirectory tempDir
          --
          step "acquire again"
          acquire consumer
          assertNonEmpty =<< listDirectory tempDir

releaseTests :: TestTree
releaseTests =
  testGroup
    "release"
    [ releaseTest "release persistent consumer" $ Persistent "foobar",
      releaseTest "release runtime consumer" Runtime
    ]
  where
    releaseTest desc consumerType =
      testCaseSteps desc $ \step ->
        with temporaryDirectory $ \tempDir -> do
          let resource = Resource tempDir
          let consumer = Consumer resource consumerType
          --
          step "acquire and release"
          acquire consumer >> release consumer
          assertEmpty =<< listDirectory tempDir
          --
          step "release again"
          release consumer
          assertEmpty =<< listDirectory tempDir

inUseTests :: TestTree
inUseTests =
  testGroup
    "inUse"
    [ acquiredTest "persistent consumer" $ Persistent "foobar",
      acquiredTest "runtime consumer" Runtime,
      acquiredAndReleasedTest "persistent consumer, released" $ Persistent "foobar",
      acquiredAndReleasedTest "runtime consumer, released" Runtime,
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
  where
    acquiredTest desc consumerType =
      testCase desc $ with temporaryDirectory $ \tempDir -> do
        let resource = Resource tempDir
        acquire (Consumer resource consumerType)
        assertTrue "not in use" =<< inUse resource
    acquiredAndReleasedTest desc consumerType =
      testCase desc $ with temporaryDirectory $ \tempDir -> do
        let resource = Resource tempDir
        let consumer = Consumer resource consumerType
        acquire consumer >> release consumer
        assertFalse "in use" =<< inUse resource

assertTrue :: HasCallStack => String -> Bool -> Assertion
assertTrue =
  assertBool

assertFalse :: HasCallStack => String -> Bool -> Assertion
assertFalse message =
  assertBool message . not

assertEmpty :: HasCallStack => [a] -> Assertion
assertEmpty = assertBool "not empty" . null

assertNonEmpty :: HasCallStack => [a] -> Assertion
assertNonEmpty = assertBool "empty" . not . null
