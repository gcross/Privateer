-- @+leo-ver=4-thin
-- @+node:gcross.20090709200011.8:@thin PrivatizationTests.hs
-- @@language haskell

-- @<< Language extensions >>
-- @+node:gcross.20090709200011.9:<< Language extensions >>
{-# LANGUAGE ScopedTypeVariables #-}
-- @-node:gcross.20090709200011.9:<< Language extensions >>
-- @nl

module PrivatizationTests(tests) where

-- @<< Imports >>
-- @+node:gcross.20090709200011.10:<< Imports >>
import Control.Exception
import Control.Monad
import Control.Monad.Trans
import qualified Data.ByteString as S
import Data.Either.Unwrap
import Language.C
import Language.C.System.GCC
import Language.C.System.Preprocess
import System
import System.Directory
import System.Exit
import System.FilePath
import System.IO
import System.Process
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import Text.PrettyPrint
import Text.XML.Expat.Tree

import Algorithm.GlobalVariablePrivatization.Privatization

import CommonTestUtils
-- @-node:gcross.20090709200011.10:<< Imports >>
-- @nl

-- @+others
-- @+node:gcross.20090709200011.11:makeTestFromSource
makeTestFromSource :: String -> Assertion
makeTestFromSource source = do
    temporary_directory <- getTemporaryDirectory
    (source_filepath, source_handle) <- openTempFile temporary_directory "test.c"
    (executable_filepath, executable_handle) <-  openTempFile temporary_directory "test"
    finally (do
        hClose executable_handle
        hPutStr source_handle source
        hClose source_handle
        (rawSystem "cc" [source_filepath,"-o",executable_filepath]) >>=
            assertEqual "Were we able to compute the source code?" ExitSuccess
        (_, Just process_output, _, process_handle) <-
            createProcess (proc executable_filepath []) { std_out = CreatePipe }
        exit_code <- waitForProcess process_handle
        when (exit_code /= ExitSuccess) $ hGetContents process_output >>= assertFailure
      ) (deleteTemporaries
            [   source_filepath
            ,   executable_filepath
            ]
      )
-- @-node:gcross.20090709200011.11:makeTestFromSource
-- @+node:gcross.20090709200011.12:Tests
tests =
    [   testCase "test" test_test
    ,   testCase "makeAccessor" test_makeAccessor
    ]
-- @+node:gcross.20090709200011.13:test
test_test = makeTestFromSource . unlines $
    ["#include <stdio.h>"
    ,"int main() { printf(\"Nothing to see here;  move along.\"); return 0; }"
    ]
-- @-node:gcross.20090709200011.13:test
-- @+node:gcross.20090709200011.15:makeAccessor
test_makeAccessor =
    let accessor_source = render . pretty $
            let size_const = CConst (CIntConst (cInteger.toInteger $ 6) internalNode)
                indirections = [CArrDeclr [] (CArrSize False size_const) internalNode]
            in makeAccessor "getGlobals" False "global_variable" "global_variable_type" indirections 6
        source = unlines
            [""
            ,"#include <stdio.h>"
            ,"#include <stdlib.h>"
            ,""
            ,"char* global_string = \"wrong\\0right\";"
            ,"void* getGlobals() { return global_string; }"
            ,""
            ,"typedef char global_variable_type;"
            ,""
            ]
            ++ accessor_source ++ unlines
            [""
            ,"int main() {"
            ,"  char (*string)[6] = __access__global_variable();"
            ,"  printf(\"Expected 'right' but got '%s'\",string);"
            ,"  return strcmp(string,\"right\");"
            ,"}"
            ]
    in makeTestFromSource source
-- @-node:gcross.20090709200011.15:makeAccessor
-- @-node:gcross.20090709200011.12:Tests
-- @-others

main = defaultMain tests
-- @-node:gcross.20090709200011.8:@thin PrivatizationTests.hs
-- @-leo
