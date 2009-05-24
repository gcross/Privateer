-- @+leo-ver=4-thin
-- @+node:gcross.20090520220305.3:@thin SizeAnalysisTests.hs
-- @@language haskell

module SizeAnalysisTests where

-- @<< Imports >>
-- @+node:gcross.20090520220305.4:<< Imports >>
import Language.C
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import Text.PrettyPrint

import CommonTestUtils
import SizeAnalysis
-- @nonl
-- @-node:gcross.20090520220305.4:<< Imports >>
-- @nl

-- @+others
-- @+node:gcross.20090520220305.5:Tests
tests = [   testGroup "Queries"
            [   testGroup "hasStaticInsideXXX"
                [   testGroup "plain statements"
                    [   testCase "1" test_hasStaticInsideXXX_statement_1
                    ,   testCase "2" test_hasStaticInsideXXX_statement_2
                    ,   testCase "3" test_hasStaticInsideXXX_statement_3
                    ,   testCase "4" test_hasStaticInsideXXX_statement_4
                    ]
                ,   testGroup "if"
                    [   testCase "1" test_hasStaticInsideXXX_if_1
                    ,   testCase "2" test_hasStaticInsideXXX_if_2
                    ,   testCase "3" test_hasStaticInsideXXX_if_3
                    ]
                ,   testGroup "switch"
                    [   testCase "1" test_hasStaticInsideXXX_switch_1
                    ,   testCase "2" test_hasStaticInsideXXX_switch_2
                    ,   testCase "3" test_hasStaticInsideXXX_switch_3
                    ,   testCase "4" test_hasStaticInsideXXX_switch_4
                    ,   testCase "5" test_hasStaticInsideXXX_switch_5
                    ]
                ,   testGroup "loops"
                    [   testGroup "for"
                        [   testCase "1" test_hasStaticInsideXXX_loops_for_1
                        ,   testCase "2" test_hasStaticInsideXXX_loops_for_2
                        ,   testCase "3" test_hasStaticInsideXXX_loops_for_3
                        ]
                    ,   testGroup "while"
                        [   testCase "1" test_hasStaticInsideXXX_loops_while_1
                        ,   testCase "2" test_hasStaticInsideXXX_loops_while_2
                        ,   testCase "3" test_hasStaticInsideXXX_loops_while_3
                        ]
                    ]
                ]
            ,   testGroup "Classification"
                [   testGroup "classifyDeclaration"
                    [   testGroup "other"
                        [   testCase "typedef" test_classification_classifyDeclaration_other_typedef
                        ,   testCase "register" test_classification_classifyDeclaration_other_register
                        ]
                    ,   testCase "extern" test_classification_classifyDeclaration_extern
                    ]
                ]
            ,   testGroup "Processing"
                [   testGroup "produceDocumentFromBlockItemClassification"
                    [   testGroup "statements"
                        [   testCase "1" test_processing_produceDocumentFromBlockItemClassification_statement_1
                        ,   testCase "2" test_processing_produceDocumentFromBlockItemClassification_statement_2
                        ,   testCase "3" test_processing_produceDocumentFromBlockItemClassification_statement_3
                        ]
                    ,   testGroup "declarations"
                        [   testCase "1" test_processing_produceDocumentFromBlockItemClassification_declaration_1
                        ,   testCase "2" test_processing_produceDocumentFromBlockItemClassification_declaration_2
                        ]
                    ]
                ]
            ]
        ]
-- @+node:gcross.20090520220305.31:Queries
-- @+node:gcross.20090520220305.6:hasStaticInsideXXX
-- @+node:gcross.20090520220305.21:plain statements
-- @+node:gcross.20090520220305.7:1
test_hasStaticInsideXXX_statement_1 =
    let result = hasStaticInsideStatement . parseStatement $ "i = 0;"
    in assertEqual "does it correctly report whether there is a static?" False result
-- @-node:gcross.20090520220305.7:1
-- @+node:gcross.20090520220305.8:2
test_hasStaticInsideXXX_statement_2 =
    let result = hasStaticInsideStatement . parseStatement $ "continue;"
    in assertEqual "does it correctly report whether there is a static?" False result
-- @-node:gcross.20090520220305.8:2
-- @+node:gcross.20090520220305.17:3
test_hasStaticInsideXXX_statement_3 =
    let result = hasStaticInsideStatement . parseStatement $ "foobar: i+= 1;"
    in assertEqual "does it correctly report whether there is a static?" False result
-- @-node:gcross.20090520220305.17:3
-- @+node:gcross.20090520220305.18:4
test_hasStaticInsideXXX_statement_4 =
    let result = hasStaticInsideStatement . parseStatement $ "foobar: {static int i = 1;}"
    in assertEqual "does it correctly report whether there is a static?" True result
-- @-node:gcross.20090520220305.18:4
-- @-node:gcross.20090520220305.21:plain statements
-- @+node:gcross.20090520220305.22:if statements
-- @+node:gcross.20090520220305.9:1
test_hasStaticInsideXXX_if_1 =
    let result = hasStaticInsideStatement . parseStatement $ "if(i==0) { register int i; } else i+=1;"
    in assertEqual "does it correctly report whether there is a static?" False result
-- @-node:gcross.20090520220305.9:1
-- @+node:gcross.20090520220305.10:2
test_hasStaticInsideXXX_if_2 =
    let result = hasStaticInsideStatement . parseStatement $ "if(i==0) { static int i; } else i+=1;"
    in assertEqual "does it correctly report whether there is a static?" True result
-- @-node:gcross.20090520220305.10:2
-- @+node:gcross.20090520220305.11:3
test_hasStaticInsideXXX_if_3 =
    let result = hasStaticInsideStatement . parseStatement $ "if(i==0) { int i; } else { i+=1; static int b; }"
    in assertEqual "does it correctly report whether there is a static?" True result
-- @-node:gcross.20090520220305.11:3
-- @-node:gcross.20090520220305.22:if statements
-- @+node:gcross.20090520220305.23:switch statements
-- @+node:gcross.20090520220305.12:1
test_hasStaticInsideXXX_switch_1 =
    let result = hasStaticInsideStatement . parseStatement $
            "switch(b) { case 1: {int i = 0;  ++i;  break;} case 2: {++b; ++i; int s;} default: {x=b+i; int j; j=x;} }"
    in assertEqual "does it correctly report whether there is a static?" False result
-- @-node:gcross.20090520220305.12:1
-- @+node:gcross.20090520220305.13:2
test_hasStaticInsideXXX_switch_2 =
    let result = hasStaticInsideStatement . parseStatement $
            "switch(b) { case 1: {static int i = 0;  ++i;  break;} case 2: {++b; ++i; int s;} default: {x=b+i; int j; j=x;} }"
    in assertEqual "does it correctly report whether there is a static?" True result
-- @-node:gcross.20090520220305.13:2
-- @+node:gcross.20090520220305.14:3
test_hasStaticInsideXXX_switch_3 =
    let result = hasStaticInsideStatement . parseStatement $
            "switch(b) { case 1: {int i = 0;  ++i;  break;} case 2: {++b; ++i; static int s;} default: {x=b+i; int j; j=x;} }"
    in assertEqual "does it correctly report whether there is a static?" True result
-- @-node:gcross.20090520220305.14:3
-- @+node:gcross.20090520220305.15:4
test_hasStaticInsideXXX_switch_4 =
    let result = hasStaticInsideStatement . parseStatement $
            "switch(b) { case 1: {int i = 0;  ++i;  break;} case 2: {++b; ++i; int s;} default: {x=b+i; static int j; j=x;} }"
    in assertEqual "does it correctly report whether there is a static?" True result
-- @-node:gcross.20090520220305.15:4
-- @+node:gcross.20090520220305.16:5
test_hasStaticInsideXXX_switch_5 =
    let result = hasStaticInsideStatement . parseStatement $
            "switch(b) { case 1: {static int i = 0;  ++i;  break;} case 2: {++b; ++i; static int s;} default: {x=b+i; static int j; j=x;} }"
    in assertEqual "does it correctly report whether there is a static?" True result
-- @-node:gcross.20090520220305.16:5
-- @-node:gcross.20090520220305.23:switch statements
-- @+node:gcross.20090520220305.24:loops
-- @+node:gcross.20090520220305.25:for
-- @+node:gcross.20090520220305.19:1
test_hasStaticInsideXXX_loops_for_1 =
    let result = hasStaticInsideStatement . parseStatement $
            "for(i=0;i<1;++i) return i;"
    in assertEqual "does it correctly report whether there is a static?" False result
-- @-node:gcross.20090520220305.19:1
-- @+node:gcross.20090520220305.20:2
test_hasStaticInsideXXX_loops_for_2 =
    let result = hasStaticInsideStatement . parseStatement $
            "for(i=0;i<1;++i) { register int i = 0; }"
    in assertEqual "does it correctly report whether there is a static?" False result
-- @-node:gcross.20090520220305.20:2
-- @+node:gcross.20090520220305.26:3
test_hasStaticInsideXXX_loops_for_3 =
    let result = hasStaticInsideStatement . parseStatement $
            "for(i=0;i<1;++i) { static int i = 0; }"
    in assertEqual "does it correctly report whether there is a static?" True result
-- @-node:gcross.20090520220305.26:3
-- @-node:gcross.20090520220305.25:for
-- @+node:gcross.20090520220305.27:while
-- @+node:gcross.20090520220305.28:1
test_hasStaticInsideXXX_loops_while_1 =
    let result = hasStaticInsideStatement . parseStatement $
            "while(i) return i;"
    in assertEqual "does it correctly report whether there is a static?" False result
-- @-node:gcross.20090520220305.28:1
-- @+node:gcross.20090520220305.29:2
test_hasStaticInsideXXX_loops_while_2 =
    let result = hasStaticInsideStatement . parseStatement $
            "while(i==0) { register int i = 0; }"
    in assertEqual "does it correctly report whether there is a static?" False result
-- @-node:gcross.20090520220305.29:2
-- @+node:gcross.20090520220305.30:3
test_hasStaticInsideXXX_loops_while_3 =
    let result = hasStaticInsideStatement . parseStatement $
            "while(i==1) { static int i = 0; }"
    in assertEqual "does it correctly report whether there is a static?" True result
-- @-node:gcross.20090520220305.30:3
-- @-node:gcross.20090520220305.27:while
-- @-node:gcross.20090520220305.24:loops
-- @-node:gcross.20090520220305.6:hasStaticInsideXXX
-- @-node:gcross.20090520220305.31:Queries
-- @+node:gcross.20090520220305.32:Classification
-- @+node:gcross.20090520220305.33:classifyDeclaration
-- @+node:gcross.20090520220305.34:other
-- @+node:gcross.20090520220305.35:typedef
test_classification_classifyDeclaration_other_typedef =
    let (CDeclExt decl) = parseDeclaration "typedef struct { int A; } A_struct;"
        result = classifyDeclaration decl
    in assertDataEqual "was the declaration correctly classified?" (OtherDeclaration decl) result
-- @-node:gcross.20090520220305.35:typedef
-- @+node:gcross.20090520220305.38:register
test_classification_classifyDeclaration_other_register =
    let (CDeclExt decl) = parseDeclaration "register int i;"
        result = classifyDeclaration decl
    in assertDataEqual "was the declaration correctly classified?" (OtherDeclaration decl) result
-- @-node:gcross.20090520220305.38:register
-- @-node:gcross.20090520220305.34:other
-- @+node:gcross.20090520220305.39:extern
test_classification_classifyDeclaration_extern =
    let (CDeclExt decl) = parseDeclaration "extern int i;"
        result = classifyDeclaration decl
    in assertDataEqual "was the declaration correctly classified?" DeclarationImportingExternalObject result
-- @-node:gcross.20090520220305.39:extern
-- @-node:gcross.20090520220305.33:classifyDeclaration
-- @-node:gcross.20090520220305.32:Classification
-- @+node:gcross.20090523222635.2:Processing
-- @+node:gcross.20090523222635.5:produceDocumentFromBlockItemClassification
-- @+node:gcross.20090523222635.6:statements
-- @+node:gcross.20090523222635.3:1
test_processing_produceDocumentFromBlockItemClassification_statement_1 =
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
test_processing_produceDocumentFromBlockItemClassification_statement_2 =
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
test_processing_produceDocumentFromBlockItemClassification_statement_3 =
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
-- @-node:gcross.20090523222635.6:statements
-- @+node:gcross.20090523222635.8:declarations
-- @+node:gcross.20090523222635.9:1
test_processing_produceDocumentFromBlockItemClassification_declaration_1 =
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
-- @+node:gcross.20090523222635.10:2
test_processing_produceDocumentFromBlockItemClassification_declaration_2 =
    assertDataEqual "was the declaration correctly classified?" "{static int i;\n printf(\"\t\t<static-variable name=\"i\" size=\"%i\"/>\", sizeof(i));}"
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
    "{ static int i = 1; }"
-- @-node:gcross.20090523222635.10:2
-- @-node:gcross.20090523222635.8:declarations
-- @-node:gcross.20090523222635.5:produceDocumentFromBlockItemClassification
-- @-node:gcross.20090523222635.2:Processing
-- @-node:gcross.20090520220305.5:Tests
-- @-others

main = defaultMain tests
-- @-node:gcross.20090520220305.3:@thin SizeAnalysisTests.hs
-- @-leo
