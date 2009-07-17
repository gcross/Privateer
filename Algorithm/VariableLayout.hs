-- @+leo-ver=4-thin
-- @+node:gcross.20090615091711.11:@thin VariableLayout.hs
-- @@language Haskell

module Algorithm.VariableLayout where

-- @<< Imports >>
-- @+node:gcross.20090615091711.12:<< Imports >>
import Control.Arrow
import Control.Exception

import Data.Accessor
import Data.Bits
import qualified Data.Foldable as Foldable
import Data.Function
import qualified Data.List as List
import Data.List.PointedList
import qualified Data.Map as Map
import Data.Sequence (Seq, (|>), ViewL(EmptyL,(:<)))
import qualified Data.Sequence as Seq
import Data.Word
-- @-node:gcross.20090615091711.12:<< Imports >>
-- @nl

-- @<< Types >>
-- @+node:gcross.20090615091711.13:<< Types >>
type Offset = Word
type Size = Offset
type Alignment = Int
type Request = (Alignment,Size)
type RequestList = [Request]
type BlockList = [(Alignment,Seq Offset)]
type BlockZipper = PointedList (Alignment,Seq Offset)
-- @-node:gcross.20090615091711.13:<< Types >>
-- @nl

-- @+others
-- @+node:gcross.20090715105401.2:Algorithm functions
-- @+node:gcross.20090715105401.3:initialBlockList
initialBlockList :: BlockList
initialBlockList = [(bitSize (undefined :: Offset),Seq.singleton 0)]
-- @-node:gcross.20090715105401.3:initialBlockList
-- @+node:gcross.20090615091711.29:fragmentBlocks
fragmentBlocks :: Alignment -> Offset -> [(Alignment,Offset)]
fragmentBlocks final_alignment starting_offset = go final_alignment
  where
    final_offset = (`shiftL` final_alignment) . (+1) . (`shiftR` final_alignment) $ starting_offset
    complement_offset = (final_offset - starting_offset)
    go alignment
        | alignment < 0
            = []
        | not (complement_offset `testBit` alignment)
            = go (alignment-1)
        | otherwise
            = (alignment,final_offset - (bit alignment))
                : go (alignment-1)
-- @-node:gcross.20090615091711.29:fragmentBlocks
-- @+node:gcross.20090615091711.14:allocateBlock
allocateBlock :: BlockList -> Request -> Maybe (BlockList,Offset)
allocateBlock block_list (requested_alignment,requested_size)
 | requested_size == 0 = Just (block_list,0)
 | bit requested_alignment < requested_size = Nothing
 | otherwise =
    assert(requested_size `shiftR` (requested_alignment+1) == 0)
    $
    fromList block_list
    >>=
    findBlock
    >>=
    return . mergeFragments
  where
    findBlock :: BlockZipper -> Maybe (BlockZipper,Alignment,Offset)
    findBlock block_zipper =
        let (alignment,offsets) = focus block_zipper
        in if alignment < requested_alignment
            then next block_zipper >>= findBlock
            else
                case Seq.viewl offsets of
                    EmptyL -> delete block_zipper >>= findBlock
                    offset :< remaining_offsets ->
                        Just ((focusA ^= (alignment,remaining_offsets)) block_zipper, alignment, offset)

    mergeFragments :: (BlockZipper,Alignment,Offset) -> (BlockList,Offset)
    mergeFragments (block_zipper,alignment,offset)
        | bit requested_alignment == requested_size
            = (toList block_zipper,offset)
        | otherwise
            = (go (fragmentBlocks alignment (offset+requested_size)) block_zipper, offset)
      where
        go :: [(Alignment,Offset)] -> BlockZipper -> BlockList
        go fragment_list =
            case fragment_list of
                [] -> toList
                (fragment_alignment,fragment_offset):remaining_fragments ->
                    let go2 :: BlockZipper -> BlockZipper
                        go2 block_zipper
                                | block_alignment > fragment_alignment =
                                    case previous block_zipper of
                                        Nothing -> insertLeft block_with_only_this_fragment block_zipper
                                        Just previous_block_zipper -> go2 previous_block_zipper
                                | block_alignment == fragment_alignment =
                                    (focusA^=(block_alignment,block_offsets |> fragment_offset)) block_zipper
                                | otherwise = -- block_alignment < fragment_alignment =
                                    insertRight block_with_only_this_fragment block_zipper
                              where
                                (block_alignment,block_offsets) = focus block_zipper
                                block_with_only_this_fragment = (fragment_alignment, Seq.singleton fragment_offset)
                    in go remaining_fragments . go2
-- @-node:gcross.20090615091711.14:allocateBlock
-- @+node:gcross.20090715105401.5:allocateBlocks
allocateBlocks :: BlockList -> RequestList -> Maybe (BlockList,[Offset])
allocateBlocks initial_blocklist requests = go requests (initial_blocklist,Seq.empty)
  where
    go :: RequestList -> (BlockList,Seq Offset) -> Maybe (BlockList,[Offset])
    go [] (block_list,offsets) = Just (block_list,Foldable.toList offsets)
    go (request:remaining_requests) (block_list,offsets) =
        allocateBlock block_list request >>= go remaining_requests . second (offsets |>)
-- @-node:gcross.20090715105401.5:allocateBlocks
-- @+node:gcross.20090715105401.6:allocateNamedBlocks
allocateNamedBlocks :: BlockList -> [(a,Request)] -> Maybe (BlockList,[(a,Offset)])
allocateNamedBlocks block_list named_requests =
    let (names,requests) = unzip . List.sortBy (compare `on` (snd . snd)) $ named_requests
    in allocateBlocks block_list requests >>= return . second (zip names)
-- @-node:gcross.20090715105401.6:allocateNamedBlocks
-- @+node:gcross.20090615091711.39:totalSpaceInBlocks
totalSpaceInBlocks :: (Integral a, Bits a) => BlockList -> a
totalSpaceInBlocks = sum . map (\(alignment,offsets) -> ((bit alignment) * (fromIntegral . toInteger . Seq.length $ offsets)))
-- @-node:gcross.20090615091711.39:totalSpaceInBlocks
-- @+node:gcross.20090715105401.21:totalSpaceRequired
totalSpaceRequired :: Ord a => [(a,Size)] -> [(a,Offset)] -> Size
totalSpaceRequired sizes offsets =
    let sizes_map = Map.fromList sizes
        offsets_map = Map.fromList offsets
    in maximum . Map.elems $ Map.unionWith (+) sizes_map offsets_map
-- @-node:gcross.20090715105401.21:totalSpaceRequired
-- @-node:gcross.20090715105401.2:Algorithm functions
-- @+node:gcross.20090715105401.4:Helper functions
-- @+node:gcross.20090715105401.9:minimumAlignment
minimumAlignment :: Bits a => a -> Alignment
minimumAlignment 0 = 1
minimumAlignment size = go (size `shiftR` 1) 1
  where
    go :: Bits a => a -> Alignment -> Alignment
    go 0 alignment = alignment
    go size alignment = go (size `shiftR` 1) (alignment+1)
-- @-node:gcross.20090715105401.9:minimumAlignment
-- @+node:gcross.20090615091711.45:makeAligned
makeAligned :: Bits a => a -> Int -> a
makeAligned number alignment = (`shiftL` alignment) . (`shiftR` alignment) $ number
-- @-node:gcross.20090615091711.45:makeAligned
-- @+node:gcross.20090615091711.33:toList
toList :: PointedList a -> [a]
toList (PointedList prefix head tail) = go prefix (head:tail)
  where
    go [] list = list
    go (head:remainder) tail = go remainder (head:tail)
-- @-node:gcross.20090615091711.33:toList
-- @-node:gcross.20090715105401.4:Helper functions
-- @-others
-- @-node:gcross.20090615091711.11:@thin VariableLayout.hs
-- @-leo
