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
import Control.Monad.Reader
import Control.Monad.RWS

import qualified Data.ByteString as S
import Data.Either.Unwrap
import Data.Map (Map,keysSet)
import qualified Data.Map as Map
import Data.Maybe
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

import Algorithm.GlobalVariablePrivatization.Common
import Algorithm.GlobalVariablePrivatization.Privatization

import CommonTestUtils
-- @-node:gcross.20090709200011.10:<< Imports >>
-- @nl

-- @+others
-- @+node:gcross.20090715105401.29:Helpers
-- @+node:gcross.20090715105401.27:makeFunctionFromMap
makeFunctionFromMap map name = fromMaybe (error $ "unable to find global variable with name '" ++ name ++ "'") (Map.lookup name map)
-- @-node:gcross.20090715105401.27:makeFunctionFromMap
-- @+node:gcross.20090715105401.28:makeFunctionFromLocalStaticMap
makeFunctionFromLocalStaticMap map = fmap makeFunctionFromMap . flip Map.lookup map
-- @-node:gcross.20090715105401.28:makeFunctionFromLocalStaticMap
-- @-node:gcross.20090715105401.29:Helpers
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
-- @+node:gcross.20090711085032.33:makeTestFromPrivatizedSource
makeTestFromPrivatizedSource :: String -> Map String Allocation -> Map String (Map String Allocation) -> Bool -> String -> String -> Assertion
makeTestFromPrivatizedSource
    module_data_accessor_name
    global_variable_allocation_map
    local_static_variable_allocation_map
    print_source
    prelude
    =
    makeTestFromSource
    .
    (if print_source then echo id else id)
    .
    (prelude ++)
    .
    render
    .
    pretty
    .
    processTranslUnit
        module_data_accessor_name
        (keysSet global_variable_allocation_map)
        (makeFunctionFromMap global_variable_allocation_map)
        (makeFunctionFromLocalStaticMap local_static_variable_allocation_map) 
    .
    parseTranslUnit
-- @nonl
-- @-node:gcross.20090711085032.33:makeTestFromPrivatizedSource
-- @+node:gcross.20090709200011.35:makePrivatizeExprTest
makePrivatizeExprTest :: Map String Allocation -> Map String Allocation  -> String -> String -> Assertion
makePrivatizeExprTest global_variable_allocation_map local_static_variable_allocation_map original_code privatized_code =
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
    (\expr -> runRWS (privatizeExpr expr)
                    (FunctionProcessingEnvironment
                        {   globalModuleDataAccessorName = undefined
                        ,   globalVariableAllocationMap = (makeFunctionFromMap global_variable_allocation_map)
                        ,   localStaticVariableAllocationMap = (makeFunctionFromMap local_static_variable_allocation_map)
                        }
                    )
                    (keysSet global_variable_allocation_map, keysSet local_static_variable_allocation_map))
    .
    parseExpression
    $
    original_code
-- @-node:gcross.20090709200011.35:makePrivatizeExprTest
-- @+node:gcross.20090709200011.41:makePrivatizeStmtTest
makePrivatizeStmtTest :: String -> Map String Allocation -> Set String -> Map String Allocation -> String -> String -> Assertion
makePrivatizeStmtTest
    module_data_accessor_name
    global_variable_allocation_map
    local_static_variables_in_scope
    local_static_variable_allocation_map
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
                    (FunctionProcessingEnvironment
                        {   globalModuleDataAccessorName = module_data_accessor_name
                        ,   globalVariableAllocationMap = (makeFunctionFromMap global_variable_allocation_map)
                        ,   localStaticVariableAllocationMap = (makeFunctionFromMap local_static_variable_allocation_map)
                        }
                    )
                    (keysSet global_variable_allocation_map, local_static_variables_in_scope)
    )
    .
    parseStatement
    $
    original_code
-- @-node:gcross.20090709200011.41:makePrivatizeStmtTest
-- @+node:gcross.20090710174219.12:makePrivatizeFunctionTest
makePrivatizeFunctionTest :: String -> Map String Allocation -> Map String Allocation -> String -> String -> Assertion
makePrivatizeFunctionTest
    module_data_accessor_name
    global_variable_allocation_map
    local_static_variable_allocation_map
    original_code privatized_code
    =
    assertEqual "is the privatized code correct?" (unwords . words . render . pretty . parseDeclaration $ privatized_code)
    .
    unwords
    .
    words
    .
    render
    .
    pretty
    .
    privatizeFunction
        module_data_accessor_name
        (keysSet global_variable_allocation_map)
        (makeFunctionFromMap global_variable_allocation_map)
        (makeFunctionFromMap local_static_variable_allocation_map)
    .
    (\(CFDefExt x) -> x)
    .
    parseDeclaration
    $
    original_code
-- @-node:gcross.20090710174219.12:makePrivatizeFunctionTest
-- @+node:gcross.20090711085032.9:makeProcessStmtTest
makeProcessStmtTest :: String -> Map String Allocation -> Map String Allocation -> String -> String -> Assertion
makeProcessStmtTest
    module_data_accessor_name
    global_variable_allocation_map
    local_static_variable_allocation_map
    original_code processed_code
    =
    assertEqual "is the processed code correct?" (unwords . words . render . pretty . parseStatement $ processed_code)
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
    (\stat -> runRWS (processStmt stat)
                    (FunctionProcessingEnvironment
                        {   globalModuleDataAccessorName = module_data_accessor_name
                        ,   globalVariableAllocationMap = (makeFunctionFromMap global_variable_allocation_map)
                        ,   localStaticVariableAllocationMap = (makeFunctionFromMap local_static_variable_allocation_map)
                        }
                    )
                    (keysSet global_variable_allocation_map, keysSet local_static_variable_allocation_map)
    )
    .
    parseStatement
    $
    original_code
-- @-node:gcross.20090711085032.9:makeProcessStmtTest
-- @+node:gcross.20090711085032.22:makeProcessFunctionTest
makeProcessFunctionTest :: String -> Map String Allocation -> Map String Allocation -> String -> String -> Assertion
makeProcessFunctionTest
    module_data_accessor_name
    global_variable_allocation_map
    local_static_variable_allocation_map
    original_code processed_code
    =
    assertEqual "is the processed code correct?" (unwords . words . render . pretty . parseDeclaration $ processed_code)
    .
    unwords
    .
    words
    .
    render
    .
    pretty
    .
    CFDefExt
    .
    processFunction
        module_data_accessor_name
        (keysSet global_variable_allocation_map)
        (makeFunctionFromMap global_variable_allocation_map)
        (makeFunctionFromMap local_static_variable_allocation_map)
    .
    (\(CFDefExt x) -> x)
    .
    parseDeclaration
    $
    original_code
-- @-node:gcross.20090711085032.22:makeProcessFunctionTest
-- @+node:gcross.20090711085032.28:makeProcessToplevelDeclarationTest
makeProcessToplevelDeclarationTest :: String -> Map String Allocation -> Map String (Map String Allocation) -> String -> String -> Assertion
makeProcessToplevelDeclarationTest
    module_data_accessor_name
    global_variable_allocation_map
    local_static_variable_allocation_map
    original_code processed_code
    =
    assertEqual "is the processed code correct?" (unwords . words . render . pretty . parseTranslUnit $ processed_code)
    .
    unwords
    .
    words
    .
    render
    .
    pretty
    .
    (flip CTranslUnit internalNode)
    .
    processToplevelDeclaration
        module_data_accessor_name
        (keysSet global_variable_allocation_map)
        (makeFunctionFromMap global_variable_allocation_map)
        (makeFunctionFromLocalStaticMap local_static_variable_allocation_map) 
    .
    parseDeclaration
    $
    original_code
-- @-node:gcross.20090711085032.28:makeProcessToplevelDeclarationTest
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
    -- @+node:gcross.20090711085032.7:Privatization
    ,testGroup "Privatization"
        -- @    @+others
        -- @+node:gcross.20090709200011.33:privatizeExpr
        [testGroup "privatizeExpr"
            -- @    @+others
            -- @+node:gcross.20090709200011.34:global variable
            [testCase "global variable" $
                makePrivatizeExprTest (Map.singleton "var" undefined) Map.empty "1 + var * 2" "1 + *__access__var() * 2"
            -- @-node:gcross.20090709200011.34:global variable
            -- @+node:gcross.20090711085032.37:nested global variable
            ,testCase "nested global variable" $
                makePrivatizeExprTest (Map.singleton "c" undefined) Map.empty "strcpy(c, d)" "strcpy(*__access__c(), d)"
            -- @-node:gcross.20090711085032.37:nested global variable
            -- @+node:gcross.20090709200011.37:local static variable
            ,testCase "local static variable" $
                makePrivatizeExprTest Map.empty (Map.singleton "var" undefined) "var * 2 == 5 ? i : j" "*var * 2 == 5 ? i : j"
            -- @-node:gcross.20090709200011.37:local static variable
            -- @+node:gcross.20090709200011.38:both
            ,testCase "both" $
                makePrivatizeExprTest (Map.singleton "var" undefined) (Map.singleton "var" undefined) "(int) f(var++)" "(int) f((*var)++)"
            -- @-node:gcross.20090709200011.38:both
            -- @+node:gcross.20090709200011.39:neither
            ,testCase "neither" $
                makePrivatizeExprTest (Map.singleton "var" undefined) (Map.singleton "var" undefined) "++nonvar" "++nonvar"
            -- @-node:gcross.20090709200011.39:neither
            -- @+node:gcross.20090928181847.1541:sizeof global variable
            ,testCase "sizeof global variable" $
                makePrivatizeExprTest (Map.singleton "var" (Allocation {allocationSize = 42, allocationOffset = undefined})) Map.empty "sizeof(var)" "42"
            -- @-node:gcross.20090928181847.1541:sizeof global variable
            -- @+node:gcross.20090928181847.1543:sizeof local static variable
            ,testCase "sizeof local static variable" $
                makePrivatizeExprTest Map.empty (Map.singleton "var" (Allocation {allocationSize = 42, allocationOffset = undefined})) "sizeof(var)" "42"
            -- @-node:gcross.20090928181847.1543:sizeof local static variable
            -- @+node:gcross.20090928181847.1545:sizeof both
            ,testCase "sizeof both" $
                makePrivatizeExprTest
                    (Map.singleton "var1" (Allocation {allocationSize = 24, allocationOffset = undefined}))
                    (Map.singleton "var2" (Allocation {allocationSize = 42, allocationOffset = undefined}))
                    "sizeof(var1)+sizeof(var2)"
                    "24 + 42"
            -- @-node:gcross.20090928181847.1545:sizeof both
            -- @-others
            ]
        -- @-node:gcross.20090709200011.33:privatizeExpr
        -- @+node:gcross.20090709200011.42:privatizeStmt
        ,testGroup "privatizeStmt"
            -- @    @+others
            -- @+node:gcross.20090709200011.43:global variable
            [testCase "global variable" $
                makePrivatizeStmtTest undefined (Map.singleton "var" undefined) Set.empty Map.empty
                    "if (var == i) { return var; } else { return j; }"
                    "if (*__access__var() == i) { return *__access__var(); } else { return j; }"
            -- @-node:gcross.20090709200011.43:global variable
            -- @+node:gcross.20090709200011.44:local static variable
            ,testCase "local static variable" $
                makePrivatizeStmtTest undefined Map.empty (Set.singleton "var") (Map.singleton "var" undefined)
                "switch (var) { case 1: var = var + 1; case 2: ++var; }"
                "switch (*var) { case 1: *var = *var + 1; case 2: ++ (*var); }"
            -- @-node:gcross.20090709200011.44:local static variable
            -- @+node:gcross.20090709200011.45:both
            ,testCase "both" $
                makePrivatizeStmtTest undefined (Map.singleton "var" undefined) (Set.singleton "var") (Map.singleton "var" undefined)
                "for (var = 0; var < 5; var++) { return var; }"
                "for (*var = 0; *var < 5; (*var)++) { return *var; }"
            -- @-node:gcross.20090709200011.45:both
            -- @+node:gcross.20090709200011.46:neither
            ,testCase "neither" $
                makePrivatizeStmtTest undefined (Map.singleton "var" undefined) Set.empty (Map.singleton "var" undefined)
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
                makePrivatizeStmtTest undefined (Map.singleton "var" undefined) (Set.singleton "var") (Map.singleton "var" undefined)
                "{ var++; int var; var++; }"
                "{ (*var)++; int var; var++; }"
            -- @-node:gcross.20090710174219.6:shadowing
            -- @+node:gcross.20090718130736.27:initializer handling
            ,testCase "shadowing" $
                makePrivatizeStmtTest undefined (Map.singleton "other" undefined) Set.empty Map.empty
                "{ int* var = &other; }"
                "{ int* var = & (*__access__other());}"
            -- @-node:gcross.20090718130736.27:initializer handling
            -- @+node:gcross.20090710174219.7:static declaration
            ,testCase "static declaration" $
                makePrivatizeStmtTest "getPtr" Map.empty Set.empty (Map.fromList [("var1",Allocation (error "Should not need to know size of var1!") 42),("var2",Allocation (error "Should not need to know size of var2!") 12)])
                "{ ++var; static int var1, *var2; ++var1; var2 = &var1; }"
                "{ ++var; typedef int __type_of__var1; __type_of__var1 *var1 = (__type_of__var1*) (getPtr() + 42), **var2 = (__type_of__var1 **) (getPtr() + 12); ++ (*var1); *var2 = &(*var1); }"
            -- @-node:gcross.20090710174219.7:static declaration
            -- @+node:gcross.20090710174219.8:external global variable
            ,testCase "external global variable" $
                makePrivatizeStmtTest undefined (Map.singleton "var" undefined) Set.empty Map.empty
                "{ var; extern int var; var; }"
                "{ *__access__var(); extern int *__access__var(); *__access__var(); }"
            -- @-node:gcross.20090710174219.8:external global variable
            -- @+node:gcross.20090710174219.9:emerging from the shadow
            ,testCase "emerging from the shadow" $
                makePrivatizeStmtTest undefined Map.empty (Set.singleton "var") (Map.singleton "var" undefined)
                "{ if(1) { int var; var++; }; var++; }"
                "{ if(1) { int var; var++; }; (*var)++; }"
            -- @-node:gcross.20090710174219.9:emerging from the shadow
            -- @-others
            ]
        -- @-node:gcross.20090710174219.5:privatizeBlockItem
        -- @+node:gcross.20090710174219.13:privatizeFunction
        ,testGroup "privatizeFunction"
            -- @    @+others
            -- @+node:gcross.20090711085032.36:simple
            [testCase "simple" $
                makePrivatizeFunctionTest undefined (Map.singleton "c" undefined) Map.empty
                    "static char* f() { strcpy(c,d); }"
                    "static char* f() { strcpy(*__access__c(),d); }"
            -- @-node:gcross.20090711085032.36:simple
            -- @+node:gcross.20090710174219.14:shadowing
            ,testCase "shadowing" $
                makePrivatizeFunctionTest "getPtr" (Map.fromList [("gvar",undefined),("var",undefined)]) (Map.fromList [("svar",Allocation undefined 1)])
                "void f(int var) { static int svar = 0; ++svar; return var + gvar + svar; }"
                "void f(int var) { typedef int __type_of__svar; __type_of__svar *svar = (__type_of__svar*)(getPtr()+1); ++(*svar); return var + *__access__gvar() + *svar; }"
            -- @-node:gcross.20090710174219.14:shadowing
            -- @+node:gcross.20090928192713.1543:with sizeof
            ,testCase "with sizeof" $
                makePrivatizeFunctionTest undefined (Map.singleton "c" (Allocation 42 undefined)) Map.empty
                    "static int f() { return sizeof(c); }"
                    "static int f() { return 42; }"
            -- @-node:gcross.20090928192713.1543:with sizeof
            -- @-others
            ]
        -- @-node:gcross.20090710174219.13:privatizeFunction
        -- @-others
        ]
    -- @-node:gcross.20090711085032.7:Privatization
    -- @+node:gcross.20090711085032.8:Initializer Construction
    ,testGroup "Initializer Construction"
        -- @    @+others
        -- @+node:gcross.20090709200011.24:makeInitializer
        [testGroup "makeInitializer"
            -- @    @+others
            -- @+node:gcross.20090709200011.28:with initializer
            [testCase "with initializer" $
                let initializer_source = render . pretty $
                        let init = CInitList [([],CInitExpr expr internalNode) | expr <-
                                        [   makeIntegerExpr 42
                                        ,   makeFloatExpr 3.14
                                        ,   makeStringExpr "right"
                                        ] ] internalNode
                        in makeInitializer undefined undefined "global_variable" (makeTypedefSpecifier "global_variable_type") [] (Just init)
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
                let initializer_source = render . pretty $ makeInitializer undefined undefined "global_variable" undefined [] Nothing
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
        -- @-node:gcross.20090709200011.24:makeInitializer
        -- @+node:gcross.20090711085032.10:processStmt
        ,testGroup "processStmt"
            -- @    @+others
            -- @+node:gcross.20090711085032.13:single statement
            [testCase "single statement" $
                makeProcessStmtTest undefined undefined undefined
                    "return i;"
                    "{ }"
            -- @-node:gcross.20090711085032.13:single statement
            -- @+node:gcross.20090711085032.16:multiple statements
            ,testCase "multiple statements" $
                makeProcessStmtTest undefined undefined undefined
                    "{ i = 1;  ++i; }"
                    "{ }"
            -- @-node:gcross.20090711085032.16:multiple statements
            -- @+node:gcross.20090711085032.17:non-static declarations
            ,testCase "non-static declarations" $
                makeProcessStmtTest undefined undefined undefined
                    "{ int i = 1;  ++i;  int j = i; }"
                    "{ int i; int j; }"
            -- @-node:gcross.20090711085032.17:non-static declarations
            -- @+node:gcross.20090711085032.18:static declarations
            ,testCase "static declarations" $
                makeProcessStmtTest "getPtr" undefined (Map.singleton "svar" (Allocation undefined 13))
                    "{ static int svar = 1;  ++svar;  int j = svar; }"
                    "{ int svar = 1; memcpy(getPtr()+13,&svar,sizeof(svar)); int j; }"
            -- @-node:gcross.20090711085032.18:static declarations
            -- @+node:gcross.20090711085032.19:nesting
            ,testCase "nesting" $
                makeProcessStmtTest "getPtr" undefined (Map.singleton "svar" (Allocation undefined 13))
                    "{ if (i == 1) {static int svar = 1;  ++svar;} {for (i = 0; i < 2; ++i) { return 42; }} }"
                    "{ { int svar = 1; memcpy(getPtr()+13,&svar,sizeof(svar)); } }"
            -- @-node:gcross.20090711085032.19:nesting
            -- @+node:gcross.20090718130736.31:aliased variable
            ,testCase "aliased variable" $
                makeProcessStmtTest "getPtr" (Map.singleton "other" undefined) (Map.singleton "svar" (Allocation undefined 13))
                    "{ static int svar = &other;  ++svar; }"
                    "{ int svar = & (*__access__other()); memcpy(getPtr()+13,&svar,sizeof(svar)); }"
            -- @-node:gcross.20090718130736.31:aliased variable
            -- @-others
            ]
        -- @-node:gcross.20090711085032.10:processStmt
        -- @+node:gcross.20090711085032.23:processFunction
        ,testGroup "processFunction"
            -- @    @+others
            -- @+node:gcross.20090711085032.24:single statement
            [testCase "single statement" $
                makeProcessFunctionTest undefined undefined undefined
                    "const int f() { return i; }"
                    "static void __initialize_statics_in__f() { }"
            -- @-node:gcross.20090711085032.24:single statement
            -- @+node:gcross.20090711085032.25:arguments
            ,testCase "arguments" $
                makeProcessFunctionTest undefined undefined undefined
                    "const int f(int a, restrict char* b, const short* c) { if (a == b) { return c; } else { ++(*b); }; }"
                    "static void __initialize_statics_in__f() { int a; restrict char* b; const short* c; }"
            -- @-node:gcross.20090711085032.25:arguments
            -- @+node:gcross.20090711085032.26:arguments and statics
            ,testCase "arguments and statics" $
                makeProcessFunctionTest "getPtr" undefined (Map.singleton "meaning_of_life" (Allocation undefined 24))
                    "static char* f(void ** data) { static int meaning_of_life = 42; }"
                    "static void __initialize_statics_in__f() { void ** data; int meaning_of_life = 42; memcpy(getPtr()+24,&meaning_of_life,sizeof(meaning_of_life)); }"
            -- @-node:gcross.20090711085032.26:arguments and statics
            -- @+node:gcross.20090928192713.1549:embedded sizeof
            ,testCase "embedded sizeof" $
                makeProcessFunctionTest "getPtr" (Map.singleton "global" (Allocation 42 undefined)) (Map.singleton "meaning_of_life" (Allocation undefined 24))
                    "static char* f(void ** data) { static int meaning_of_life = sizeof(global); }"
                    "static void __initialize_statics_in__f() { void ** data; int meaning_of_life = 42; memcpy(getPtr()+24,&meaning_of_life,sizeof(meaning_of_life)); }"
            -- @-node:gcross.20090928192713.1549:embedded sizeof
            -- @-others
            ]
        -- @-node:gcross.20090711085032.23:processFunction
        -- @+node:gcross.20090711085032.29:processToplevelDeclaration
        ,testGroup "processToplevelDeclaration"
            -- @    @+others
            -- @+node:gcross.20090711085032.30:external declaration
            [testCase "external declaration" $
                makeProcessToplevelDeclarationTest undefined (Map.singleton "ext_var" undefined) undefined
                    "extern char var, ext_var;"
                    "extern char var, *__access__ext_var();"
            -- @-node:gcross.20090711085032.30:external declaration
            -- @+node:gcross.20090711085032.35:function
            ,testCase "function" $
                makeProcessToplevelDeclarationTest undefined (Map.singleton "c" undefined) Map.empty
                    "static char* f() { strcpy(c,d); }"
                    "static char* f() { strcpy(*__access__c(),d); }"
            -- @-node:gcross.20090711085032.35:function
            -- @+node:gcross.20090718130736.24:static forward function
            ,testCase "static forward function" $
                makeProcessToplevelDeclarationTest undefined (Map.singleton "c" undefined) Map.empty
                    "static char* f();"
                    "static char* f();"
            -- @-node:gcross.20090718130736.24:static forward function
            -- @+node:gcross.20090711085032.34:global variable
            ,testCase "global variable" $
                makeTestFromPrivatizedSource "getPtr" (Map.singleton "c" (Allocation undefined 3)) (Map.singleton "main" Map.empty) False
                (unlines
                    ["#include <stdlib.h>"
                    ,"#include <stdio.h>"
                    ,"#include <string.h>"
                    ,""
                    ,"char global_array[20];"
                    ,"void* getPtr() { return global_array; }"
                    ])
                (unlines
                    ["char c[7] = \"hello!\";"
                    ,""
                    ,"int main() {"
                    ,"  memset(global_array,0,sizeof(global_array));"
                    ,"  __initialize__c();"
                    ,"  if(strcmp(c,\"hello!\") != 0) {"
                    ,"    printf(\"expected c='hello!', but saw '%s'\",c);"
                    ,"    return -1;"
                    ,"  }"
                    ,"  if(strcmp(global_array+3,\"hello!\") != 0) {"
                    ,"    printf(\"expected global_array+3='hello!', but saw '%s'\",c);"
                    ,"    return -1;"
                    ,"  }"
                    ,"}"
                    ])
            -- @-node:gcross.20090711085032.34:global variable
            -- @+node:gcross.20090928192713.1547:global variable dependent on sizeof
            ,testCase "global variable dependent on sizeof" $
                makeTestFromPrivatizedSource "getPtr" (Map.fromList [("c",Allocation 42 3),("d",Allocation undefined 20)]) (Map.singleton "main" Map.empty) False
                (unlines
                    ["#include <stdlib.h>"
                    ,"#include <stdio.h>"
                    ,"#include <string.h>"
                    ,""
                    ,"char global_array[100];"
                    ,"void* getPtr() { return global_array; }"
                    ])
                (unlines
                    ["char c[7] = \"hello!\";"
                    ,"int d = sizeof(c);"
                    ,""
                    ,"int main() {"
                    ,"  memset(global_array,0,sizeof(global_array));"
                    ,"  __initialize__c();"
                    ,"  __initialize__d();"
                    ,"  if(strcmp(global_array+3,\"hello!\") != 0) {"
                    ,"    printf(\"expected global_array+3='hello!', but saw '%s'\",c);"
                    ,"    return -1;"
                    ,"  }"
                    ,"  if(*(int*)(global_array+20) != 42) {"
                    ,"    printf(\"expected global_array+20=42, but saw '%i'\",d);"
                    ,"    return -1;"
                    ,"  }"
                    ,"}"
                    ])
            -- @-node:gcross.20090928192713.1547:global variable dependent on sizeof
            -- @-others
            ]
        -- @-node:gcross.20090711085032.29:processToplevelDeclaration
        -- @-others
        ]
    -- @-node:gcross.20090711085032.8:Initializer Construction
    -- @-others
    ]
-- @-node:gcross.20090709200011.12:Tests
-- @-others

main = defaultMain tests
-- @-node:gcross.20090709200011.8:@thin PrivatizationTests.hs
-- @-leo
