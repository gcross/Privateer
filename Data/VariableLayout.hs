-- @+leo-ver=4-thin
-- @+node:gcross.20090615091711.11:@thin VariableLayout.hs
-- @@language Haskell

module Data.VariableLayout where

-- @<< Imports >>
-- @+node:gcross.20090615091711.12:<< Imports >>
import Control.Exception

import Data.Accessor
import Data.Bits
import Data.List.PointedList
import Data.Sequence (Seq, (|>), ViewL(EmptyL,(:<)))
import qualified Data.Sequence as Seq

-- @-node:gcross.20090615091711.12:<< Imports >>
-- @nl

-- @<< Types >>
-- @+node:gcross.20090615091711.13:<< Types >>
type Offset = Int
type Alignment = Int
type BlockList = [(Alignment,Seq Offset)]
type BlockZipper = PointedList (Alignment,Seq Offset)
-- @-node:gcross.20090615091711.13:<< Types >>
-- @nl

-- @+others
-- @+node:gcross.20090615091711.29:fragmentBlocks
fragmentBlocks :: Alignment -> Offset -> [(Alignment,Offset)]
fragmentBlocks alignment offset
    | alignment <= 0
        = []
    | not (offset `testBit` (alignment-1))
        = fragmentBlocks (alignment-1) offset
    | otherwise
        = ((alignment-1),(`shiftL` (alignment-1)) . (`shiftR` (alignment-1)) $ offset)
            : fragmentBlocks (alignment-1) offset
-- @-node:gcross.20090615091711.29:fragmentBlocks
-- @+node:gcross.20090615091711.14:allocateBlock
allocateBlock :: Alignment -> Offset -> BlockList -> Maybe (BlockList,Offset)
allocateBlock requested_alignment requested_size block_list = assert (requested_size `shiftR` requested_alignment > 0) $
    fromList block_list
    >>=
    findBlock
    >>=
    return . mergeFragments
  where
    findBlock :: BlockZipper -> Maybe (BlockZipper,Offset)
    findBlock block_zipper = 
        let (alignment,offsets) = focus block_zipper
        in if alignment < requested_alignment
            then next block_zipper >>= findBlock
            else
                case Seq.viewl offsets of
                    EmptyL -> delete block_zipper >>= findBlock
                    offset :< remaining_offsets ->
                        Just ((focusA ^= (alignment,remaining_offsets)) block_zipper, offset)

    mergeFragments :: (BlockZipper,Offset) -> (BlockList,Offset)
    mergeFragments (block_zipper,offset) = (go (fragmentBlocks requested_alignment (offset+requested_size)) block_zipper, offset)
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
                                | block_alignment < fragment_alignment =
                                    insertRight block_with_only_this_fragment block_zipper
                              where
                                (block_alignment,block_offsets) = focus block_zipper
                                block_with_only_this_fragment = (fragment_alignment, Seq.singleton fragment_offset)
                    in go remaining_fragments . go2
-- @-node:gcross.20090615091711.14:allocateBlock
-- @+node:gcross.20090615091711.32:PointedList
-- @+node:gcross.20090615091711.31:first / last
--first :: PointedList a -> PointedList a
--first plist = maybe plist first prev

--last :: PointedList a -> PointedList a
--last plist = maybe plist last next
-- @-node:gcross.20090615091711.31:first / last
-- @+node:gcross.20090615091711.33:toList
toList (PointedList prefix head tail) = go prefix (head:tail)
  where
    go [] list = list
    go (head:remainder) tail = go remainder (head:tail)
-- @-node:gcross.20090615091711.33:toList
-- @-node:gcross.20090615091711.32:PointedList
-- @-others
-- @-node:gcross.20090615091711.11:@thin VariableLayout.hs
-- @-leo
