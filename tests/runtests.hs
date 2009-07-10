-- @+leo-ver=4-thin
-- @+node:gcross.20090411002248.2:@thin runtests.hs
-- @@language Haskell

module Main where

import Test.Framework

import CommonTests
import SizeAnalysisTests
import RunAnalysisTests

main = do
    defaultMain
        [   testGroup "module CommonTests" CommonTests.tests
        ,   testGroup "module SizeAnalysisTests" SizeAnalysisTests.tests
        ,   testGroup "module RunAnalysisTests" RunAnalysisTests.tests
        ]
-- @-node:gcross.20090411002248.2:@thin runtests.hs
-- @-leo
