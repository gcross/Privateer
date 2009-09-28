-- @+leo-ver=4-thin
-- @+node:gcross.20090928152056.1491:@thin VariableLayoutTests.hs
-- @@language Haskell

-- @<< Language Extensions >>
-- @+node:gcross.20090615091711.22:<< Language Extensions >>
{-# LANGUAGE ScopedTypeVariables #-}
-- @-node:gcross.20090615091711.22:<< Language Extensions >>
-- @nl

module VariableLayoutTests(tests) where

-- @<< Imports >>
-- @+node:gcross.20090615091711.23:<< Imports >>
import Control.Arrow ((&&&),second)
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
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck

import Algorithm.GlobalVariablePrivatization.VariableLayout

import Control.Exception
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
    arbitrary = choose (0,bitSize (undefined :: Offset) - 1) >>= return.SI
    -- coarbitrary (SI n) = coarbitrary n
-- @-node:gcross.20090615091711.25:SmallInt
-- @+node:gcross.20090615091711.43:Word
instance Arbitrary Word where
  arbitrary     = sized $ \n -> choose (0,n) >>= return.fromIntegral
  -- coarbitrary n = variant.fromIntegral $ 2*n
-- @-node:gcross.20090615091711.43:Word
-- @+node:gcross.20090718130736.2:SizeType
newtype SizeType = ST Size deriving (Show,Eq)
instance Arbitrary SizeType where
  arbitrary     = sized $ \n -> if n == 0 then return . ST $ 1 else choose (1,n) >>= return . ST . fromIntegral
  -- coarbitrary n = variant.fromIntegral $ 2*n
-- @-node:gcross.20090718130736.2:SizeType
-- @+node:gcross.20090615091711.41:BlockListType
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
    -- coarbitrary (SI n) = coarbitrary n
-- @-node:gcross.20090615091711.41:BlockListType
-- @-node:gcross.20090615091711.40:Generators
-- @+node:gcross.20090615091711.19:Tests
tests =
    -- @    @+others
    -- @+node:gcross.20090715105401.10:minimumAlignment
    [testGroup "minimumAlignment"
        -- @    @+others
        -- @+node:gcross.20090715105401.11:not too large
        [testProperty "not too large" $
            \(ST size) -> size `shiftR` (minimumAlignment size - 1) /= 0
        -- @-node:gcross.20090715105401.11:not too large
        -- @+node:gcross.20090715105401.12:not too small
        ,testProperty "not too small" $
            \(ST size) ->
                let minimum_alignment = minimumAlignment size
                in (size == bit minimum_alignment || size `shiftR` minimum_alignment == 0)
        -- @-node:gcross.20090715105401.12:not too small
        -- @-others
        ]
    -- @-node:gcross.20090715105401.10:minimumAlignment
    -- @+node:gcross.20090615091711.20:fragmentBlocks
    ,testGroup "fragmentBlocks"
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
                    (`all` [0..final_alignment-1])
                    .
                    (\blocks n -> (complement_offset `testBit` n ==) . isJust . List.lookup n $ blocks)
                    $
                    fragmentBlocks final_alignment starting_offset
        -- @-node:gcross.20090615091711.26:only the right blocks are created
        -- @+node:gcross.20090615091711.38:sizes add up
        ,testProperty "sizes add up" $
            \((SI final_alignment,starting_offset) :: (SmallInt,Offset)) ->
            (starting_offset > 0) && (starting_offset <= bit final_alignment)
            ==>
                (== (bit final_alignment) - starting_offset)
                .
                sum
                .
                map (bit . fst)
                .
                fragmentBlocks final_alignment
                $
                starting_offset
        -- @-node:gcross.20090615091711.38:sizes add up
        -- @+node:gcross.20090715105401.31:offsets monotonically decrease
        ,testProperty "offsets monotonically decrease" $
            \((SI final_alignment,starting_offset) :: (SmallInt,Offset)) ->
                case fragmentBlocks final_alignment starting_offset of
                    [] -> True
                    [_] -> True
                    (_,this):rest ->
                        let check _ [] = True
                            check previous ((_,this):rest) =
                                if this < previous
                                    then check this rest
                                    else False
                        in check this rest
        -- @-node:gcross.20090715105401.31:offsets monotonically decrease
        -- @-others
        ]
    -- @-node:gcross.20090615091711.20:fragmentBlocks
    -- @+node:gcross.20090615091711.34:allocateBlock
    ,testGroup "allocateBlock"
        -- @    @+others
        -- @+node:gcross.20090615091711.35:null
        [testCase "null" $ assertEqual "is the correct result returned?" Nothing (allocateBlock [] (0,1))
        -- @-node:gcross.20090615091711.35:null
        -- @+node:gcross.20090615091711.36:simplest
        ,testCase "simplest" $
            assertEqual "is the correct offset returned?"
                (Just 0)
                (fmap snd . allocateBlock [(0,Seq.singleton 0)] $ (0,1))
        -- @-node:gcross.20090615091711.36:simplest
        -- @+node:gcross.20090715105401.8:always successful with large block (1)
        ,testProperty "always successful with large block (1)" $
            \(SI requested_alignment,ST requested_size) -> bit requested_alignment >= requested_size ==>
                isJust . allocateBlock initialBlockList $ (requested_alignment,requested_size)
        -- @-node:gcross.20090715105401.8:always successful with large block (1)
        -- @+node:gcross.20090715105401.13:always successful with large block (2)
        ,testProperty "always successful with large block (2)" $
            \(ST requested_size) -> 
                isJust . allocateBlock initialBlockList $ (minimumAlignment requested_size,requested_size)
        -- @-node:gcross.20090715105401.13:always successful with large block (2)
        -- @+node:gcross.20090615091711.44:correct alignment returned
        ,testProperty "correct alignment returned" $
            \(SI requested_alignment,ST requested_size,BL blocks) ->
                case allocateBlock blocks (requested_alignment,requested_size) of
                    Nothing -> True
                    Just (_,offset) -> (offset == offset `makeAligned` requested_alignment)
        -- @-node:gcross.20090615091711.44:correct alignment returned
        -- @+node:gcross.20090618135417.2:size of free space is reduced (1)
        ,testProperty "size of free space is reduced (1)" $
            \(SI requested_alignment,ST requested_size,BL blocks) ->
                case allocateBlock blocks (requested_alignment,requested_size) of
                    Nothing -> True
                    Just (new_blocks,_) -> totalSpaceInBlocks new_blocks == totalSpaceInBlocks blocks - requested_size
        -- @-node:gcross.20090618135417.2:size of free space is reduced (1)
        -- @+node:gcross.20090715105401.33:size of free space is reduced (2)
        ,testProperty "size of free space is reduced (2)" $
            \(SI requested_alignment,BL blocks) ->
                let requested_size = bit requested_alignment in
                case allocateBlock blocks (requested_alignment,requested_size) of
                    Nothing -> True
                    Just (new_blocks,_) -> totalSpaceInBlocks new_blocks == totalSpaceInBlocks blocks - requested_size
        -- @-node:gcross.20090715105401.33:size of free space is reduced (2)
        -- @-others
        ]
    -- @-node:gcross.20090615091711.34:allocateBlock
    -- @+node:gcross.20090715105401.14:allocateNamedBlocks
    ,testGroup "allocateNamedBlocks"
        -- @    @+others
        -- @+node:gcross.20090715105401.15:correct alignment returned
        [testProperty "correct alignment returned" $
            \(boxed_sizes :: [SizeType]) ->
                let sizes = [size | ST size <- boxed_sizes]
                    variables = zip [0..] sizes
                    requests = map (second (minimumAlignment &&& id)) variables
                in case allocateNamedBlocks initialBlockList requests of
                    Nothing -> True
                    Just (_,offsets) -> all (\(name,offset) -> offset == offset `makeAligned` (minimumAlignment $ fromJust $ lookup name variables)) offsets
        -- @-node:gcross.20090715105401.15:correct alignment returned
        -- @+node:gcross.20090715105401.16:size of free space is reduced (1)
        ,testProperty "size of free space is reduced (1)" $
            \(boxed_sizes :: [SizeType]) ->
                let sizes = [size | ST size <- boxed_sizes]
                    variables = zip [0..] sizes
                    requests = map (second (minimumAlignment &&& id)) variables
                in case allocateNamedBlocks initialBlockList requests of
                    Nothing -> True
                    Just (new_blocks,_) -> (totalSpaceInBlocks new_blocks :: Integer) == (bit . fst . head $ initialBlockList) - toInteger (sum sizes)
        -- @-node:gcross.20090715105401.16:size of free space is reduced (1)
        -- @+node:gcross.20090715105401.17:size of free space is reduced (2)
        ,testProperty "size of free space is reduced (2)" $
            \((boxed_sizes,BL old_blocks) :: ([SizeType],BlockListType)) ->
                let sizes = [size | ST size <- boxed_sizes]
                    variables = zip [0..] sizes
                    requests = map (second (minimumAlignment &&& id)) variables
                in case allocateNamedBlocks old_blocks requests of
                    Nothing -> True
                    Just (new_blocks,_) -> (totalSpaceInBlocks new_blocks :: Integer) == (totalSpaceInBlocks old_blocks :: Integer) - toInteger (sum sizes)
        -- @-node:gcross.20090715105401.17:size of free space is reduced (2)
        -- @+node:gcross.20090715105401.22:total space > space allocated
        ,testProperty "total space > space allocated" $
            \(boxed_sizes :: [SizeType]) ->
                let sizes = [size | ST size <- boxed_sizes]
                    variables = zip [0..] (filter (>0) sizes)
                    requests = map (second (minimumAlignment &&& id)) variables
                in (not . null) variables ==> case allocateNamedBlocks initialBlockList requests of
                    Nothing -> True
                    Just (new_blocks,offsets) -> totalSpaceRequired requests offsets >= sum sizes
        -- @-node:gcross.20090715105401.22:total space > space allocated
        -- @-others
        ]
    -- @-node:gcross.20090715105401.14:allocateNamedBlocks
    -- @-others
    ]

-- @-node:gcross.20090615091711.19:Tests
-- @-others

-- @-node:gcross.20090928152056.1491:@thin VariableLayoutTests.hs
-- @-leo
