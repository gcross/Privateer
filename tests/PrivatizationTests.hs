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
import Control.Monad.RWS
import qualified Data.ByteString as S
import Data.Either.Unwrap
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
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

import Algorithm.GlobalVariablePrivatization.Privatization

import CommonTestUtils
-- @-node:gcross.20090709200011.10:<< Imports >>
-- @nl

-- @+others
-- @+node:gcross.20090709200011.36:Test makers
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
            assertEqual "Were we able to compile the source code?" ExitSuccess
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
-- @+node:gcross.20090709200011.35:makePrivatizeExprTest
makePrivatizeExprTest :: Set String -> Set String -> String -> String -> Assertion
makePrivatizeExprTest global_variables local_static_variables original_code privatized_code =
    assertEqual "is the privatized code correct?" privatized_code
    .
    unwords
    .
    words
    .
    render
    .
    pretty
    .
    (\(x,_,_) -> x)
    .
    (\expr -> runRWS (privatizeExpr expr) undefined (global_variables,local_static_variables))
    .
    parseExpression
    $
    original_code
-- @-node:gcross.20090709200011.35:makePrivatizeExprTest
-- @+node:gcross.20090709200011.41:makePrivatizeStmtTest
makePrivatizeStmtTest :: String -> Map String Int -> Set String -> Set String -> String -> String -> Assertion
makePrivatizeStmtTest
    module_data_accessor_name
    local_static_variable_index_map
    global_variables local_static_variables
    original_code privatized_code
    =
    assertEqual "is the privatized code correct?" (unwords . words . render . pretty . parseStatement $ privatized_code)
    .
    unwords
    .
    words
    .
    render
    .
    pretty
    .
    (\(x,_,_) -> x)
    .
    (\stat -> runRWS (privatizeStmt stat)
                    (FunctionProcessingEnvironment module_data_accessor_name local_static_variable_index_map)
                    (global_variables,local_static_variables)
    )
    .
    parseStatement
    $
    original_code
-- @-node:gcross.20090709200011.41:makePrivatizeStmtTest
-- @-node:gcross.20090709200011.36:Test makers
-- @+node:gcross.20090709200011.12:Tests
tests =
    -- @    @+others
    -- @+node:gcross.20090709200011.13:test
    [testCase "test" $ makeTestFromSource . unlines $
        ["#include <stdio.h>"
        ,"int main() { printf(\"Nothing to see here;  move along.\"); return 0; }"
        ]
    -- @-node:gcross.20090709200011.13:test
    -- @+node:gcross.20090709200011.15:makeAccessor
    ,testCase "makeAccessor" $
        let accessor_source = render . pretty $
                let size_const = CConst (CIntConst (cInteger.toInteger $ 6) internalNode)
                    indirections = [CArrDeclr [] (CArrSize False size_const) internalNode]
                in makeAccessor "getGlobals" False "global_variable" (makeTypedefSpecifier "global_variable_type") indirections 6
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
    -- @+node:gcross.20090709200011.24:makeInitializer
    ,testGroup "makeInitializer"
        -- @    @+others
        -- @+node:gcross.20090709200011.28:with initializer
        [testCase "with initializer" $
            let initializer_source = render . pretty $
                    let init = CInitList [([],CInitExpr expr internalNode) | expr <-
                                    [   makeIntegerExpr 42
                                    ,   makeFloatExpr 3.14
                                    ,   makeStringExpr "right"
                                    ] ] internalNode
                    in makeInitializer "global_variable" (makeTypedefSpecifier "global_variable_type") [] (Just init)
                source = unlines
                    [""
                    ,"#include <stdio.h>"
                    ,"#include <stdlib.h>"
                    ,"#include <string.h>"
                    ,""
                    ,"typedef struct {"
                    ,"  int a;"
                    ,"  float b;"
                    ,"  char *c;"
                    ,"} global_variable_type;"
                    ,""
                    ,"global_variable_type global_variable = {0,0,\"wrong\"};"
                    ,"global_variable_type* __access__global_variable() { return &global_variable; }"
                    ,""
                    ]
                    ++ initializer_source ++ unlines
                    [""
                    ,"int main() {"
                    ,"  __initialize__global_variable();"
                    ,"  printf(\"Expected {42,3.14,'right'} but got {%i,%f,'%s',%i}\","
                    ,"         global_variable.a,global_variable.b,global_variable.c);"
                    ,"  if(     (global_variable.a != 42)"
                    ,"      ||  (global_variable.b != 3.14f)"
                    ,"      ||  (strcmp(global_variable.c,\"right\") != 0)"
                    ,"    )"
                    ,"      return -1;"
                    ,"  else"
                    ,"      return 0;"
                    ,"}"
                    ]
            in makeTestFromSource source
        -- @-node:gcross.20090709200011.28:with initializer
        -- @+node:gcross.20090709200011.29:without initializer
        ,testCase "without initializer" $
            let initializer_source = render . pretty $ makeInitializer "global_variable" undefined [] Nothing
                source = unlines
                    [""
                    ,"#include <stdio.h>"
                    ,""
                    ,"typedef struct {"
                    ,"  int a;"
                    ,"  float b;"
                    ,"  char *c;"
                    ,"} global_variable_type;"
                    ,""
                    ]
                    ++ initializer_source ++ unlines
                    [""
                    ,"int main() {"
                    ,"  __initialize__global_variable();"
                    ,"  return 0;"
                    ,"}"
                    ]
            in makeTestFromSource source
        -- @-node:gcross.20090709200011.29:without initializer
        -- @-others
        ]
    -- @nonl
    -- @-node:gcross.20090709200011.24:makeInitializer
    -- @+node:gcross.20090709200011.33:privatizeExpr
    ,testGroup "privatizeExpr"
        -- @    @+others
        -- @+node:gcross.20090709200011.34:global variable
        [testCase "global variable" $
            makePrivatizeExprTest (Set.singleton "var") Set.empty "1 + var * 2" "1 + *__access__var() * 2"
        -- @-node:gcross.20090709200011.34:global variable
        -- @+node:gcross.20090709200011.37:local static variable
        ,testCase "local static variable" $
            makePrivatizeExprTest Set.empty (Set.singleton "var") "var * 2 == 5 ? i : j" "*var * 2 == 5 ? i : j"
        -- @-node:gcross.20090709200011.37:local static variable
        -- @+node:gcross.20090709200011.38:both
        ,testCase "both" $
            makePrivatizeExprTest (Set.singleton "var") (Set.singleton "var") "(int) sizeof(var++)" "(int) sizeof((*var)++)"
        -- @-node:gcross.20090709200011.38:both
        -- @+node:gcross.20090709200011.39:neither
        ,testCase "neither" $
            makePrivatizeExprTest (Set.singleton "var") (Set.singleton "var") "++nonvar" "++nonvar"
        -- @-node:gcross.20090709200011.39:neither
        -- @-others
        ]
    -- @-node:gcross.20090709200011.33:privatizeExpr
    -- @+node:gcross.20090709200011.42:privatizeStmt
    ,testGroup "privatizeStmt"
        -- @    @+others
        -- @+node:gcross.20090709200011.43:global variable
        [testCase "global variable" $
            makePrivatizeStmtTest undefined undefined (Set.singleton "var") Set.empty
                "if (var == i) { return var; } else { return j; }"
                "if (*__access__var() == i) { return *__access__var(); } else { return j; }"
        -- @-node:gcross.20090709200011.43:global variable
        -- @+node:gcross.20090709200011.44:local static variable
        ,testCase "local static variable" $
            makePrivatizeStmtTest undefined undefined Set.empty (Set.singleton "var")
            "switch (var) { case 1: var = var + 1; case 2: ++var; }"
            "switch (*var) { case 1: *var = *var + 1; case 2: ++ (*var); }"
        -- @-node:gcross.20090709200011.44:local static variable
        -- @+node:gcross.20090709200011.45:both
        ,testCase "both" $
            makePrivatizeStmtTest undefined undefined (Set.singleton "var") (Set.singleton "var")
            "for (var = 0; var < 5; var++) { return var; }"
            "for (*var = 0; *var < 5; (*var)++) { return *var; }"
        -- @-node:gcross.20090709200011.45:both
        -- @+node:gcross.20090709200011.46:neither
        ,testCase "neither" $
            makePrivatizeStmtTest undefined undefined (Set.singleton "var") (Set.singleton "var")
            "if (nonvar) return 4;"
            "if (nonvar) return 4;"
        -- @-node:gcross.20090709200011.46:neither
        -- @-others
        ]
    -- @-node:gcross.20090709200011.42:privatizeStmt
    -- @+node:gcross.20090710174219.5:privatizeBlockItem
    ,testGroup "privatizeBlockItem"
        -- @    @+others
        -- @+node:gcross.20090710174219.6:shadowing
        [testCase "shadowing" $
            makePrivatizeStmtTest undefined undefined (Set.singleton "var") (Set.singleton "var")
            "{ var++; int var; var++; }"
            "{ (*var)++; int var; var++; }"
        -- @-node:gcross.20090710174219.6:shadowing
        -- @+node:gcross.20090710174219.7:static declaration
        ,testCase "static declaration" $
            makePrivatizeStmtTest "getPtr" (Map.fromList [("var1",42),("var2",12)]) Set.empty Set.empty
            "{ ++var; static int var1, *var2; ++var1; var2 = &var1; }"
            "{ ++var; typedef int __type_of__var1; __type_of__var1 *var1 = (__type_of__var1*) (getPtr() + 42), **var2 = (__type_of__var1 **) (getPtr() + 12); ++ (*var1); *var2 = &(*var1); }"
        -- @-node:gcross.20090710174219.7:static declaration
        -- @+node:gcross.20090710174219.8:external global variable
        ,testCase "external global variable" $
            makePrivatizeStmtTest undefined undefined (Set.singleton "var") Set.empty
            "{ var; extern int var; var; }"
            "{ *__access__var(); extern int *__access__var(); *__access__var(); }"
        -- @-node:gcross.20090710174219.8:external global variable
        -- @+node:gcross.20090710174219.9:emerging from the shadow
        ,testCase "emerging from the shadow" $
            makePrivatizeStmtTest undefined undefined Set.empty (Set.singleton "var")
            "{ if(1) { int var; var++; }; var++; }"
            "{ if(1) { int var; var++; }; (*var)++; }"
        -- @-node:gcross.20090710174219.9:emerging from the shadow
        -- @-others
        ]
    -- @-node:gcross.20090710174219.5:privatizeBlockItem
    -- @-others
    ]
-- @-node:gcross.20090709200011.12:Tests
-- @-others

main = defaultMain tests
-- @-node:gcross.20090709200011.8:@thin PrivatizationTests.hs
-- @-leo
