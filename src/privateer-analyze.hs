-- @+leo-ver=4-thin
-- @+node:gcross.20090523222635.11:@thin privateer-analyze.hs
-- @@language Haskell

module Main where

-- @<< Imports >>
-- @+node:gcross.20090523222635.12:<< Imports >>
import Language.C
import System

import Algorithm.GlobalVariablePrivatization.SizeAnalysis
-- @-node:gcross.20090523222635.12:<< Imports >>
-- @nl

-- @+others
-- @+node:gcross.20090523222635.19:main
main = do
    args <- getArgs
    let (input_filename, output_filename) = 
            case args of
                [] -> error "Must supply the name of the source file to analyze."
                i:[] -> (i, i++"-analyze.c")
                i:o:_ -> (i,o)
    processFile input_filename output_filename
-- @-node:gcross.20090523222635.19:main
-- @-others
-- @-node:gcross.20090523222635.11:@thin privateer-analyze.hs
-- @-leo
