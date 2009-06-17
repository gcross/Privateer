-- @+leo-ver=4-thin
-- @+node:gcross.20090615091711.11:@thin VariableLayout.hs
-- @@language Haskell

module Data.VariableLayout where

-- @<< Imports >>
-- @+node:gcross.20090615091711.12:<< Imports >>
import Data.Bits
-- @-node:gcross.20090615091711.12:<< Imports >>
-- @nl

-- @<< Types >>
-- @+node:gcross.20090615091711.13:<< Types >>
type Offset = Int
type Alignment = Int
-- @-node:gcross.20090615091711.13:<< Types >>
-- @nl

-- @+others
-- @+node:gcross.20090615091711.14:fragmentBlocks
fragmentBlocks :: Alignment -> Offset -> [(Alignment,Offset)]
fragmentBlocks final_alignment starting_offset = go 0 starting_offset
  where
    go current_alignment current_offset
        | current_alignment >= final_alignment
            = []
        | not (current_offset `testBit` current_alignment)
            = go (current_alignment+1) current_offset
        | otherwise
            = (current_alignment,current_offset)
                : go (current_alignment+1) (current_offset `clearBit` current_alignment)
-- @-node:gcross.20090615091711.14:fragmentBlocks
-- @-others
-- @-node:gcross.20090615091711.11:@thin VariableLayout.hs
-- @-leo
