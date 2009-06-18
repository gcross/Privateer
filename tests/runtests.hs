-- @+leo-ver=4-thin
-- @+node:gcross.20090615091711.17:@thin runtests.hs
-- @@language Haskell

-- @<< Language Extensions >>
-- @+node:gcross.20090615091711.22:<< Language Extensions >>
{-# LANGUAGE ScopedTypeVariables #-}
-- @-node:gcross.20090615091711.22:<< Language Extensions >>
-- @nl

module Main where

-- @<< Imports >>
-- @+node:gcross.20090615091711.23:<< Imports >>
import Control.Exception
import Control.Monad
import Control.Monad.Trans

import Data.Bits
import Data.Function
import Data.List as List
import Data.Maybe
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

import Debug.Trace

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import Test.Framework.Providers.QuickCheck
import Test.QuickCheck

import Data.VariableLayout
-- @-node:gcross.20090615091711.23:<< Imports >>
-- @nl

-- @+others
-- @+node:gcross.20090615091711.24:assertThrowsError
assertThrowsError :: a -> Assertion
assertThrowsError expr =
    try ( Control.Exception.evaluate expr ) >>= handler
  where
    handler :: Either SomeException a -> Assertion
    handler (Left _) = return ()
    handler (Right _) = assertFailure $ "Error was not thrown!"
-- @-node:gcross.20090615091711.24:assertThrowsError
-- @+node:gcross.20090615091711.25:SmallInt
newtype SmallInt = SI Int deriving (Show,Eq)
instance Arbitrary SmallInt where
    arbitrary = choose (0,31) >>= return.SI
    coarbitrary (SI n) = coarbitrary n
-- @-node:gcross.20090615091711.25:SmallInt
-- @+node:gcross.20090615091711.19:Tests
tests =
    -- @    @+others
    -- @+node:gcross.20090615091711.20:fragmentBlocks
    [testGroup "fragmentBlocks"
        -- @    @+others
        -- @+node:gcross.20090615091711.21:blocks have advertized alignment
        [testProperty "blocks have advertized alignment" $
            \((SI final_alignment,starting_offset) :: (SmallInt,Int)) ->  starting_offset >= 0 ==>
                all (\(alignment,offset) -> (== offset) . (`shiftL` alignment) . (`shiftR` alignment) $  offset)
                    $ fragmentBlocks final_alignment starting_offset
        -- @-node:gcross.20090615091711.21:blocks have advertized alignment
        -- @+node:gcross.20090615091711.28:blocks don't exceed final alignment
        ,testProperty "blocks don't exceed final alignment" $
            \((SI final_alignment,starting_offset) :: (SmallInt,Int)) ->  starting_offset >= 0 ==>
                all ((< final_alignment) . fst)
                    $ fragmentBlocks final_alignment starting_offset
        -- @-node:gcross.20090615091711.28:blocks don't exceed final alignment
        -- @+node:gcross.20090615091711.26:only the right blocks are created
        ,testProperty "only the right blocks are created" $
            \((SI final_alignment,starting_offset) :: (SmallInt,Int)) ->  starting_offset >= 0 ==>
                let blocks = fragmentBlocks final_alignment starting_offset
                in (`all` [0..final_alignment-1]) $
                    \n -> (starting_offset `testBit` n ==) . isJust . List.lookup n $ blocks
        -- @-node:gcross.20090615091711.26:only the right blocks are created
        -- @-others
        ]
    -- @-node:gcross.20090615091711.20:fragmentBlocks
    -- @+node:gcross.20090615091711.34:allocateBlock
    ,testGroup "allocateBlock"
        -- @    @+others
        -- @+node:gcross.20090615091711.37:bad input
        [testCase "bad input" $ assertThrowsError (allocateBlock 1 0 [])
        -- @-node:gcross.20090615091711.37:bad input
        -- @+node:gcross.20090615091711.35:null
        ,testCase "null" $ assertEqual "is the correct result returned?" Nothing (allocateBlock 0 1 [])
        -- @-node:gcross.20090615091711.35:null
        -- @+node:gcross.20090615091711.36:simplest
        ,testCase "simplest" $
            assertEqual "is the correct offset returned?"
                (Just 0)
                (fmap snd . allocateBlock 0 1 $ [(0,Seq.singleton 0)])
        -- @-node:gcross.20090615091711.36:simplest
        -- @-others
        ]
    -- @-node:gcross.20090615091711.34:allocateBlock
    -- @-others
    ]

-- @-node:gcross.20090615091711.19:Tests
-- @-others

main = defaultMain tests
-- @-node:gcross.20090615091711.17:@thin runtests.hs
-- @-leo
