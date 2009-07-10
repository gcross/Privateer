-- @+leo-ver=4-thin
-- @+node:gcross.20090520220305.3:@thin SizeAnalysisTests.hs
-- @@language haskell

module SizeAnalysisTests(tests) where

-- @<< Imports >>
-- @+node:gcross.20090520220305.4:<< Imports >>
import Language.C
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import Text.PrettyPrint

import Algorithm.GlobalVariablePrivatization.SizeAnalysis

import CommonTestUtils
-- @-node:gcross.20090520220305.4:<< Imports >>
-- @nl

-- @+others
-- @+node:gcross.20090520220305.5:Tests
tests =
    -- @    @+others
    -- @+node:gcross.20090520220305.6:hasStaticInsideXXX
    [testGroup "hasStaticInsideXXX"
        -- @    @+others
        -- @+node:gcross.20090520220305.21:plain statements
        [testGroup "plain statements"
            -- @    @+others
            -- @+node:gcross.20090520220305.7:1
            [testCase "1" $
                let result = hasStaticInsideStatement . parseStatement $ "i = 0;"
                in assertEqual "does it correctly report whether there is a static?" False result
            -- @-node:gcross.20090520220305.7:1
            -- @+node:gcross.20090520220305.8:2
            ,testCase "2" $
                let result = hasStaticInsideStatement . parseStatement $ "continue;"
                in assertEqual "does it correctly report whether there is a static?" False result
            -- @-node:gcross.20090520220305.8:2
            -- @+node:gcross.20090520220305.17:3
            ,testCase "3" $
                let result = hasStaticInsideStatement . parseStatement $ "foobar: i+= 1;"
                in assertEqual "does it correctly report whether there is a static?" False result
            -- @-node:gcross.20090520220305.17:3
            -- @+node:gcross.20090520220305.18:4
            ,testCase "4" $
                let result = hasStaticInsideStatement . parseStatement $ "foobar: {static int i = 1;}"
                in assertEqual "does it correctly report whether there is a static?" True result
            -- @-node:gcross.20090520220305.18:4
            -- @-others
            ]
        -- @-node:gcross.20090520220305.21:plain statements
        -- @+node:gcross.20090520220305.22:if statements
        ,testGroup "if statements"
            -- @    @+others
            -- @+node:gcross.20090520220305.9:1
            [testCase "1" $
                let result = hasStaticInsideStatement . parseStatement $ "if(i==0) { register int i; } else i+=1;"
                in assertEqual "does it correctly report whether there is a static?" False result
            -- @-node:gcross.20090520220305.9:1
            -- @+node:gcross.20090520220305.10:2
            ,testCase "2" $
                let result = hasStaticInsideStatement . parseStatement $ "if(i==0) { static int i; } else i+=1;"
                in assertEqual "does it correctly report whether there is a static?" True result
            -- @-node:gcross.20090520220305.10:2
            -- @+node:gcross.20090520220305.11:3
            ,testCase "3" $
                let result = hasStaticInsideStatement . parseStatement $ "if(i==0) { int i; } else { i+=1; static int b; }"
                in assertEqual "does it correctly report whether there is a static?" True result
            -- @-node:gcross.20090520220305.11:3
            -- @-others
            ]
        -- @-node:gcross.20090520220305.22:if statements
        -- @+node:gcross.20090520220305.23:switch statements
        ,testGroup "switch statements"
            -- @    @+others
            -- @+node:gcross.20090520220305.12:1
            [testCase "1" $
                let result = hasStaticInsideStatement . parseStatement $
                        "switch(b) { case 1: {int i = 0;  ++i;  break;} case 2: {++b; ++i; int s;} default: {x=b+i; int j; j=x;} }"
                in assertEqual "does it correctly report whether there is a static?" False result
            -- @-node:gcross.20090520220305.12:1
            -- @+node:gcross.20090520220305.13:2
            ,testCase "2" $
                let result = hasStaticInsideStatement . parseStatement $
                        "switch(b) { case 1: {static int i = 0;  ++i;  break;} case 2: {++b; ++i; int s;} default: {x=b+i; int j; j=x;} }"
                in assertEqual "does it correctly report whether there is a static?" True result
            -- @-node:gcross.20090520220305.13:2
            -- @+node:gcross.20090520220305.14:3
            ,testCase "3" $
                let result = hasStaticInsideStatement . parseStatement $
                        "switch(b) { case 1: {int i = 0;  ++i;  break;} case 2: {++b; ++i; static int s;} default: {x=b+i; int j; j=x;} }"
                in assertEqual "does it correctly report whether there is a static?" True result
            -- @-node:gcross.20090520220305.14:3
            -- @+node:gcross.20090520220305.15:4
            ,testCase "4" $
                let result = hasStaticInsideStatement . parseStatement $
                        "switch(b) { case 1: {int i = 0;  ++i;  break;} case 2: {++b; ++i; int s;} default: {x=b+i; static int j; j=x;} }"
                in assertEqual "does it correctly report whether there is a static?" True result
            -- @-node:gcross.20090520220305.15:4
            -- @+node:gcross.20090520220305.16:5
            ,testCase "5" $
                let result = hasStaticInsideStatement . parseStatement $
                        "switch(b) { case 1: {static int i = 0;  ++i;  break;} case 2: {++b; ++i; static int s;} default: {x=b+i; static int j; j=x;} }"
                in assertEqual "does it correctly report whether there is a static?" True result
            -- @-node:gcross.20090520220305.16:5
            -- @-others
            ]
        -- @-node:gcross.20090520220305.23:switch statements
        -- @+node:gcross.20090520220305.24:loops
        ,testGroup "loops"
            -- @    @+others
            -- @+node:gcross.20090520220305.25:for
            [testGroup "for"
                -- @    @+others
                -- @+node:gcross.20090520220305.19:1
                [testCase "1" $
                    let result = hasStaticInsideStatement . parseStatement $
                            "for(i=0;i<1;++i) return i;"
                    in assertEqual "does it correctly report whether there is a static?" False result
                -- @-node:gcross.20090520220305.19:1
                -- @+node:gcross.20090520220305.20:2
                ,testCase "2" $
                    let result = hasStaticInsideStatement . parseStatement $
                            "for(i=0;i<1;++i) { register int i = 0; }"
                    in assertEqual "does it correctly report whether there is a static?" False result
                -- @-node:gcross.20090520220305.20:2
                -- @+node:gcross.20090520220305.26:3
                ,testCase "3" $
                    let result = hasStaticInsideStatement . parseStatement $
                            "for(i=0;i<1;++i) { static int i = 0; }"
                    in assertEqual "does it correctly report whether there is a static?" True result
                -- @-node:gcross.20090520220305.26:3
                -- @-others
                ]
            -- @-node:gcross.20090520220305.25:for
            -- @+node:gcross.20090520220305.27:while
            ,testGroup "while"
                -- @    @+others
                -- @+node:gcross.20090520220305.28:1
                [testCase "1" $
                    let result = hasStaticInsideStatement . parseStatement $
                            "while(i) return i;"
                    in assertEqual "does it correctly report whether there is a static?" False result
                -- @-node:gcross.20090520220305.28:1
                -- @+node:gcross.20090520220305.29:2
                ,testCase "2" $
                    let result = hasStaticInsideStatement . parseStatement $
                            "while(i==0) { register int i = 0; }"
                    in assertEqual "does it correctly report whether there is a static?" False result
                -- @-node:gcross.20090520220305.29:2
                -- @+node:gcross.20090520220305.30:3
                ,testCase "3" $
                    let result = hasStaticInsideStatement . parseStatement $
                            "while(i==1) { static int i = 0; }"
                    in assertEqual "does it correctly report whether there is a static?" True result
                -- @-node:gcross.20090520220305.30:3
                -- @-others
                ]
            -- @-node:gcross.20090520220305.27:while
            -- @-others
            ]
        -- @-node:gcross.20090520220305.24:loops
        -- @-others
        ]
    -- @nonl
    -- @-node:gcross.20090520220305.6:hasStaticInsideXXX
    -- @+node:gcross.20090520220305.33:classifyDeclaration
    ,testGroup "classifyDeclaration"
        -- @    @+others
        -- @+node:gcross.20090520220305.34:other
        [testGroup "other"
            -- @    @+others
            -- @+node:gcross.20090520220305.35:typedef
            [testCase "typedef" $
                let (CDeclExt decl) = parseDeclaration "typedef struct { int A; } A_struct;"
                    result = classifyDeclaration decl
                in assertDataEqual "was the declaration correctly classified?" (OtherDeclaration decl) result
            -- @-node:gcross.20090520220305.35:typedef
            -- @+node:gcross.20090520220305.38:register
            ,testCase "register" $
                let (CDeclExt decl) = parseDeclaration "register int i;"
                    result = classifyDeclaration decl
                in assertDataEqual "was the declaration correctly classified?" (OtherDeclaration decl) result
            -- @-node:gcross.20090520220305.38:register
            -- @-others
            ]
        -- @-node:gcross.20090520220305.34:other
        -- @+node:gcross.20090520220305.39:extern
        ,testCase "extern" $
            let (CDeclExt decl) = parseDeclaration "extern int i;"
                result = classifyDeclaration decl
            in assertDataEqual "was the declaration correctly classified?" DeclarationImportingExternalObject result
        -- @-node:gcross.20090520220305.39:extern
        -- @-others
        ]
    -- @-node:gcross.20090520220305.33:classifyDeclaration
    -- @+node:gcross.20090523222635.5:produceDocumentFromBlockItemClassification
    ,testGroup "produceDocumentFromBlockItemClassification"
        -- @    @+others
        -- @+node:gcross.20090523222635.6:statements
        [testGroup "statements"
            -- @    @+others
            -- @+node:gcross.20090523222635.3:1
            [testCase "1" $
                assertDataEqual "was the statement correctly classified?" ""
                .
                render
                .
                produceDocumentFromBlockItemClassification
                .
                classifyBlockItem
                .
                CBlockStmt
                .
                parseStatement
                $
                "i = 1;"
            -- @-node:gcross.20090523222635.3:1
            -- @+node:gcross.20090523222635.4:2
            ,testCase "2" $
                assertDataEqual "was the statement correctly classified?" ""
                .
                render
                .
                produceDocumentFromBlockItemClassification
                .
                classifyBlockItem
                .
                CBlockStmt
                .
                parseStatement
                $
                "if(i == 1) i = 2; else i = 3;"
            -- @-node:gcross.20090523222635.4:2
            -- @+node:gcross.20090523222635.7:3
            ,testCase "3" $
                assertDataEqual "was the statement correctly classified?" "{}"
                .
                render
                .
                produceDocumentFromBlockItemClassification
                .
                classifyBlockItem
                .
                CBlockStmt
                .
                parseStatement
                $
                "switch(c) { case 'a': i = 3; }"
            -- @-node:gcross.20090523222635.7:3
            -- @-others
            ]
        -- @-node:gcross.20090523222635.6:statements
        -- @+node:gcross.20090523222635.8:declarations
        ,testGroup "declarations"
            -- @    @+others
            -- @+node:gcross.20090523222635.9:1
            [testCase "1" $
                assertDataEqual "was the declaration correctly classified?" "{int i;}"
                .
                render
                .
                produceDocumentFromBlockItemClassification
                .
                classifyBlockItem
                .
                CBlockStmt
                .
                parseStatement
                $
                "{ int i = 1; }"
            -- @-node:gcross.20090523222635.9:1
            -- @-others
            ]
        -- @-node:gcross.20090523222635.8:declarations
        -- @-others
        ]
    -- @-node:gcross.20090523222635.5:produceDocumentFromBlockItemClassification
    -- @-others
    ]
-- @-node:gcross.20090520220305.5:Tests
-- @-others

main = defaultMain tests
-- @-node:gcross.20090520220305.3:@thin SizeAnalysisTests.hs
-- @-leo
