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
fragmentBlocks alignment offset 
    | alignment <= 0
        = []
    | not (offset `testBit` (alignment-1))
        = fragmentBlocks (alignment-1) offset
    | otherwise
        = ((alignment-1),(`shiftL` (alignment-1)) . (`shiftR` (alignment-1)) $ offset)
            : fragmentBlocks (alignment-1) offset
-- @-node:gcross.20090615091711.14:fragmentBlocks
-- @-others
-- @-node:gcross.20090615091711.11:@thin VariableLayout.hs
-- @-leo
