module Database.PostgreSQL.Fixture.VersionSpec
  ( versionTests,
  )
where

import Database.PostgreSQL.Fixture.Version
  ( Version (Unknown, Version),
    versionCompare,
    versionLineParse,
    versionParse,
    versionText,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
  ( assertEqual,
    testCase,
  )

versionTests :: TestTree
versionTests =
  testGroup
    "consumers"
    [ versionCompareTests,
      versionLineParseTests,
      versionParseTests,
      versionTextTests
    ]

versionCompareTests :: TestTree
versionCompareTests =
  -- TODO: use QuickCheck?
  testGroup
    "versionCompare"
    [ testCase "equal"
        $ assertVersion (Just EQ)
        $ versionCompare (v 10 0) (v 10 0),
      testGroup
        "major differences"
        [ testCase "less than"
            $ assertVersion (Just LT)
            $ versionCompare (v 10 0) (v 11 0),
          testCase "greater than"
            $ assertVersion (Just GT)
            $ versionCompare (v 10 0) (v 9 0)
        ],
      testGroup
        "minor differences"
        [ testCase "less than"
            $ assertVersion (Just LT)
            $ versionCompare (v 10 0) (v 11 0),
          testCase "greater than"
            $ assertVersion (Just GT)
            $ versionCompare (v 10 0) (v 9 0)
        ],
      testGroup
        "patch differences"
        [ testCase "less than"
            $ assertVersion (Just LT)
            $ versionCompare (vp 9 6 17) (vp 9 6 18),
          testCase "greater than"
            $ assertVersion (Just GT)
            $ versionCompare (vp 9 6 17) (vp 9 6 16)
        ],
      testGroup
        "not discernible"
        [ testCase "unknown on left"
            $ assertVersion Nothing
            $ versionCompare (Unknown "Alice") (v 10 0),
          testCase "unknown on right"
            $ assertVersion Nothing
            $ versionCompare (v 10 0) (Unknown "Bob"),
          testCase "unknown on both"
            $ assertVersion Nothing
            $ versionCompare (Unknown "Carol") (Unknown "Dave")
        ]
    ]
  where
    assertVersion = assertEqual "version mismatch"
    vp major minor patch = Version major minor $ Just patch
    v major minor = Version major minor Nothing

versionLineParseTests :: TestTree
versionLineParseTests =
  testGroup
    "versionLineParse"
    [ testCase "version below 10"
        $ assertEqual "version mismatch" (Version 9 6 $ Just 17)
        $ versionLineParse "pg_ctl (PostgreSQL) 9.6.17\n",
      testCase "version above 10"
        $ assertEqual "version mismatch" (Version 10 12 Nothing)
        $ versionLineParse "pg_ctl (PostgreSQL) 10.12\n",
      testCase "version unknown"
        $ assertEqual "version mismatch" (Unknown "Bob")
        $ versionLineParse "pg_ctl (PostgreSQL) Bob\n",
      testCase "version garbled"
        $ assertEqual "version mismatch" (Unknown "")
        $ versionLineParse "something"
    ]

versionParseTests :: TestTree
versionParseTests =
  testGroup
    "versionParse"
    [ testCase "version below 10"
        $ assertEqual "version mismatch" (Version 9 6 $ Just 17)
        $ versionParse "9.6.17",
      testCase "version above 10"
        $ assertEqual "version mismatch" (Version 10 12 Nothing)
        $ versionParse "10.12",
      testCase "version unknown"
        $ assertEqual "version mismatch" (Unknown "Carol")
        $ versionParse "Carol"
    ]

versionTextTests :: TestTree
versionTextTests =
  testGroup
    "versionText"
    [ testCase "version below 10"
        $ assertEqual "version mismatch" "9.6.17"
        $ versionText
        $ Version 9 6 (Just 17),
      testCase "version above 10"
        $ assertEqual "version mismatch" "10.12"
        $ versionText
        $ Version 10 12 Nothing,
      testCase "version unknown"
        $ assertEqual "version mismatch" "Alice (unknown)"
        $ versionText
        $ Unknown "Alice"
    ]
