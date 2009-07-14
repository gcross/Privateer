-- @+leo-ver=4-thin
-- @+node:gcross.20090411002248.2:@thin runtests.hs
-- @@language Haskell

module Main where

import Test.Framework

import CommonTests
import SizeAnalysisTests
import RunAnalysisTests
import PrivatizationTests

main = do
    run_analysis_tests <- RunAnalysisTests.makeTests
    defaultMain
        [   testGroup "module CommonTests" CommonTests.tests
        ,   testGroup "module SizeAnalysisTests" SizeAnalysisTests.tests
        ,   testGroup "module RunAnalysisTests" run_analysis_tests
        ,   testGroup "module PrivatizationTests" PrivatizationTests.tests
        ]
-- @-node:gcross.20090411002248.2:@thin runtests.hs
-- @-leo
