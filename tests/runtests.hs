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
import Data.Word

import Debug.Trace

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import Test.Framework.Providers.QuickCheck
import Test.QuickCheck

import Algorithm.VariableLayout
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
-- @+node:gcross.20090615091711.40:Generators
-- @+node:gcross.20090615091711.25:SmallInt
newtype SmallInt = SI Int deriving (Show,Eq)
instance Arbitrary SmallInt where
    arbitrary = choose (0,31) >>= return.SI
    coarbitrary (SI n) = coarbitrary n
-- @-node:gcross.20090615091711.25:SmallInt
-- @+node:gcross.20090615091711.43:Word
instance Arbitrary Word where
  arbitrary     = sized $ \n -> choose (0,n) >>= return.fromIntegral
  coarbitrary n = variant.fromIntegral $ 2*n
-- @-node:gcross.20090615091711.43:Word
-- @+node:gcross.20090615091711.41:BlockList
newtype BlockListType = BL [(Alignment,Seq Offset)] deriving (Show,Eq)
instance Arbitrary BlockListType where
    arbitrary =
        (arbitrary :: Gen [[Offset]])
        >>=
        return
        .
        BL
        .
        map (
            \(alignment,offsets) -> (alignment,
                Seq.fromList
                .
                map (`makeAligned` alignment)
                $
                offsets
            )
        )
        .
        zip [0..]
    --coarbitrary (SI n) = coarbitrary n
-- @-node:gcross.20090615091711.41:BlockList
-- @-node:gcross.20090615091711.40:Generators
-- @+node:gcross.20090615091711.19:Tests
tests =
    -- @    @+others
    -- @+node:gcross.20090615091711.20:fragmentBlocks
    [testGroup "fragmentBlocks"
        -- @    @+others
        -- @+node:gcross.20090615091711.21:blocks have advertized alignment
        [testProperty "blocks have advertized alignment" $
            \((SI final_alignment,starting_offset) :: (SmallInt,Offset)) ->
                all (\(alignment,offset) -> offset == offset `makeAligned` alignment)
                    $ fragmentBlocks final_alignment starting_offset
        -- @-node:gcross.20090615091711.21:blocks have advertized alignment
        -- @+node:gcross.20090615091711.28:blocks don't exceed final alignment
        ,testProperty "blocks don't exceed final alignment" $
            \((SI final_alignment,starting_offset) :: (SmallInt,Offset)) ->
                all ((<= final_alignment) . fst)
                    $ fragmentBlocks final_alignment starting_offset
        -- @-node:gcross.20090615091711.28:blocks don't exceed final alignment
        -- @+node:gcross.20090615091711.26:only the right blocks are created
        ,testProperty "only the right blocks are created" $
            \((SI final_alignment,starting_offset) :: (SmallInt,Offset)) ->
                let final_offset = (`shiftL` final_alignment) . (+1) . (`shiftR` final_alignment) $ starting_offset
                    complement_offset = (final_offset - starting_offset)
                in
                    (`all` [0..final_alignment])
                    .
                    (\blocks n -> (complement_offset `testBit` n ==) . isJust . List.lookup n $ blocks)
                    $
                    fragmentBlocks final_alignment starting_offset
        -- @-node:gcross.20090615091711.26:only the right blocks are created
        -- @+node:gcross.20090615091711.38:sizes add up
        ,testProperty "sizes add up" $
            \((SI final_alignment,starting_offset) :: (SmallInt,Offset)) ->
                (\x ->
                    let correct_answer =
                            (\x -> x - starting_offset)
                            .
                            (`shiftL` final_alignment)
                            .
                            (+1)
                            .
                            (`shiftR` final_alignment)
                            $
                            starting_offset
                    in correct_answer == x
                )
                .
                sum
                .
                map (bit . fst)
                .
                fragmentBlocks final_alignment
                $
                starting_offset
        -- @-node:gcross.20090615091711.38:sizes add up
        -- @-others
        ]
    -- @-node:gcross.20090615091711.20:fragmentBlocks
    -- @+node:gcross.20090615091711.34:allocateBlock
    ,testGroup "allocateBlock"
        -- @    @+others
        -- @+node:gcross.20090615091711.35:null
        [testCase "null" $ assertEqual "is the correct result returned?" Nothing (allocateBlock 0 1 [])
        -- @-node:gcross.20090615091711.35:null
        -- @+node:gcross.20090615091711.36:simplest
        ,testCase "simplest" $
            assertEqual "is the correct offset returned?"
                (Just 0)
                (fmap snd . allocateBlock 0 1 $ [(0,Seq.singleton 0)])
        -- @-node:gcross.20090615091711.36:simplest
        -- @+node:gcross.20090615091711.44:correct alignment returned
        ,testProperty "correct alignment returned" $
            \((SI requested_alignment,requested_size,BL blocks) :: (SmallInt,Offset,BlockListType)) ->
                case allocateBlock requested_alignment requested_size blocks of
                    Nothing -> True
                    Just (_,offset) -> (offset == offset `makeAligned` requested_alignment)
        -- @-node:gcross.20090615091711.44:correct alignment returned
        -- @+node:gcross.20090618135417.2:size of free space is reduced
        ,testProperty "size of free space is reduced" $
            \((SI requested_alignment,requested_size,BL blocks) :: (SmallInt,Offset,BlockListType)) ->
                case allocateBlock requested_alignment requested_size blocks of
                    Nothing -> True
                    Just (new_blocks,_) -> totalSpaceInBlocks new_blocks == totalSpaceInBlocks blocks - requested_size
        -- @-node:gcross.20090618135417.2:size of free space is reduced
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
