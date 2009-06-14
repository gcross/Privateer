-- @+leo-ver=4-thin
-- @+node:gcross.20090502101608.4:@thin SizeAnalysis.hs
-- @@language Haskell

-- @<< Language Extensions >>
-- @+node:gcross.20090502101608.14:<< Language Extensions >>
{-# LANGUAGE RelaxedPolyRec #-}
{-# LANGUAGE DeriveDataTypeable #-}
-- @-node:gcross.20090502101608.14:<< Language Extensions >>
-- @nl

module SizeAnalysis where

-- @<< Imports >>
-- @+node:gcross.20090502101608.6:<< Imports >>
import Control.Exception

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B8
import Data.Data
import qualified Data.List.Stream as List
import Data.Trie (Trie)
import qualified Data.Trie as Trie
import Data.Maybe

import Language.C

import Text.PrettyPrint
import Text.Printf
import Text.XML.Expat.Tree

import Common
-- @-node:gcross.20090502101608.6:<< Imports >>
-- @nl
-- @<< Types >>
-- @+node:gcross.20090502101608.7:<< Types >>
data StorageClassification = Automatic | Static
 deriving (Eq,Show,Typeable,Data)

data DeclarationClassification =
    DeclarationImportingExternalObject
 |  DeclarationWithVariables StorageClassification CDecl [String]
 |  OtherDeclaration CDecl
 deriving (Typeable,Data)

data ToplevelDeclarationClassification =
    ToplevelDeclaration DeclarationClassification
 |  FunctionDefinitionContainingStatics String [BlockItemClassification]
 |  OtherFunctionDefinition
 |  Other
 deriving (Typeable,Data)

data BlockItemClassification =
    IrreleventStatement
 |  NestedBlocks [[BlockItemClassification]]
 |  BlockItemDeclaration DeclarationClassification
 deriving (Typeable,Data)

data AnalyzedModule = AnalyzedModule
    {    exportedVariables :: !(Trie Int)
    ,    hiddenVariables :: !(Trie Int)
    ,    functionsWithStatics :: !(Trie (Trie Int))
    }
-- @-node:gcross.20090502101608.7:<< Types >>
-- @nl

-- @+others
-- @+node:gcross.20090506115644.12:Queries
-- @+node:gcross.20090502101608.13:hasStaticInsideXXX
hasStaticInsideBlockItem :: CBlockItem -> Bool
hasStaticInsideBlockItem (CBlockStmt stat) = hasStaticInsideStatement stat
hasStaticInsideBlockItem (CNestedFunDef (CFunDef _ _ _ stat _)) = hasStaticInsideStatement stat
hasStaticInsideBlockItem (CBlockDecl (CDecl decl_specs _ _)) = any isStaticSpec decl_specs
    where
        isStaticSpec (CStorageSpec (CStatic _)) = True
        isStaticSpec _ = False

hasStaticInsideStatement :: CStat -> Bool
hasStaticInsideStatement = List.any (List.any hasStaticInsideBlockItem) . extractNestedBlocksFromStatement

hasStaticInsideFunction :: CFunDef -> Bool
hasStaticInsideFunction (CFunDef _ _ _ stat _) = hasStaticInsideStatement stat
-- @-node:gcross.20090502101608.13:hasStaticInsideXXX
-- @+node:gcross.20090506115644.17:globalStorageRequiredBy
globalStorageRequiredBy :: CExtDecl -> Bool
globalStorageRequiredBy (CAsmExt _) = False
globalStorageRequiredBy (CFDefExt fundef) = hasStaticInsideFunction fundef
globalStorageRequiredBy (CDeclExt (CDecl decl_specs _ _)) =
    case extractStorage decl_specs of
        Nothing -> True
        Just (CAuto _) -> True
        Just (CStatic _) -> True
        _ -> False
-- @-node:gcross.20090506115644.17:globalStorageRequiredBy
-- @+node:gcross.20090506115644.18:needsFurtherProcessing
needsFurtherProcessing :: CTranslUnit -> Bool
needsFurtherProcessing (CTranslUnit ext_decls _) = List.any globalStorageRequiredBy ext_decls
-- @-node:gcross.20090506115644.18:needsFurtherProcessing
-- @-node:gcross.20090506115644.12:Queries
-- @+node:gcross.20090517181648.10:Classification
-- @+node:gcross.20090517181648.7:classifyToplevelDeclaration
classifyToplevelDeclaration :: CExtDecl -> ToplevelDeclarationClassification
classifyToplevelDeclaration extdecl =
    case extdecl of
        CAsmExt _ -> Other
        CDeclExt decl -> ToplevelDeclaration . classifyDeclaration $ decl
        CFDefExt (CFunDef _ (CDeclr (Just ident) _ _ _ _) _ stat _) ->
            if hasStaticInsideStatement stat
            then
                FunctionDefinitionContainingStatics (identToString ident)
                .
                List.map classifyBlockItem
                .
                fromJust
                .
                extractBlockFromStatement
                $
                stat
            else OtherFunctionDefinition
-- @-node:gcross.20090517181648.7:classifyToplevelDeclaration
-- @+node:gcross.20090517181648.8:classifyDeclaration
classifyDeclaration :: CDecl -> DeclarationClassification
classifyDeclaration decl =
    let (CDecl decl_specs declarators node_info) = decl
        (variable_names,stripped_declarators) =
            List.mapAccumL 
                (\variable_names (Just declarator,_,_) ->
                    let CDeclr (Just ident) indirections _ _ _ = declarator
                        name = identToString ident
                        stripped_triplet = (Just declarator,Nothing,Nothing)
                    in case indirections of
                        CFunDeclr _ _ _:_ -> (variable_names,stripped_triplet)
                        _ -> (name:variable_names,stripped_triplet)
                )
                []
                declarators
        stripped_declaration = (CDecl decl_specs stripped_declarators node_info)
        storage = extractStorage decl_specs
    in case storage of
        Just (CExtern _) -> DeclarationImportingExternalObject
        Just (CTypedef _) -> OtherDeclaration decl
        Just (CRegister _) -> OtherDeclaration decl
        Just (CStatic _) -> DeclarationWithVariables Static stripped_declaration variable_names
        Just (CAuto _) -> DeclarationWithVariables Automatic stripped_declaration variable_names
        Nothing -> DeclarationWithVariables Automatic stripped_declaration variable_names
        _ -> error $ "unable to handle storage type " ++ show storage
-- @-node:gcross.20090517181648.8:classifyDeclaration
-- @+node:gcross.20090517181648.11:classifyBlockItem
classifyBlockItem :: CBlockItem -> BlockItemClassification
classifyBlockItem item =
    case item of
        CNestedFunDef _ -> error "nested functions not yet supported"
        CBlockDecl decl -> BlockItemDeclaration . classifyDeclaration $ decl
        CBlockStmt stat ->
            NestedBlocks
            .
            (List.map $ List.map classifyBlockItem)
            .
            extractNestedBlocksFromStatement
            $
            stat
-- @-node:gcross.20090517181648.11:classifyBlockItem
-- @-node:gcross.20090517181648.10:Classification
-- @+node:gcross.20090506115644.14:Document Production
-- @+node:gcross.20090517181648.13:produceDocumentFromBlockItemClassification
produceDocumentFromBlockItemClassification :: BlockItemClassification -> Doc
produceDocumentFromBlockItemClassification classification =
    case classification of
        IrreleventStatement -> empty
        NestedBlocks blocks ->
            vcat
            .
            map (
                braces
                .
                nest 4
                .
                vcat
                .
                map produceDocumentFromBlockItemClassification
                )
            $
            blocks
        BlockItemDeclaration declaration_classification ->
            case declaration_classification of
                DeclarationImportingExternalObject -> empty
                OtherDeclaration decl -> pretty decl <> semi
                DeclarationWithVariables Automatic decl _ -> pretty decl <> semi
                DeclarationWithVariables Static decl variable_names -> 
                    pretty decl <> semi $$ (sep . map makePrintfDoc $ variable_names)
  where
    makePrintfDoc :: String -> Doc
    makePrintfDoc variable_name =
        text $ printf "printf(\"\t\t<static-variable name=\\\"%s\\\" size=\\\"%%i\\\"/>\\n\", sizeof(%s));"
                                                      variable_name                variable_name
-- @-node:gcross.20090517181648.13:produceDocumentFromBlockItemClassification
-- @+node:gcross.20090517181648.5:produceDocumentFromToplevelClassification
produceDocumentFromToplevelClassification :: ToplevelDeclarationClassification -> Doc
produceDocumentFromToplevelClassification classification =
    case classification of
        ToplevelDeclaration declaration_classification ->
            case declaration_classification of
                DeclarationImportingExternalObject -> empty
                OtherDeclaration decl -> pretty decl <> semi
                DeclarationWithVariables storage_classification decl variable_names ->
                    pretty decl <> semi $$ (sep . map (makeGlobalPrintfDoc storage_classification) $ variable_names)
        FunctionDefinitionContainingStatics name items ->
                makeBeginFunctionPrintfDoc name
            $+$ (braces . nest 4 . vcat . map produceDocumentFromBlockItemClassification $ items)
            $+$ makeEndFunctionPrintfDoc
        OtherFunctionDefinition -> empty
        Other -> empty
  where
    makeGlobalPrintfDoc :: StorageClassification -> String -> Doc
    makeGlobalPrintfDoc storage_classification variable_name =
        let visibility = case storage_classification of
                Automatic -> "exported"
                Static -> "hidden"
        in text $ printf "printf(\"\t<%s-variable name=\\\"%s\\\" size=\\\"%%i\\\"/>\\n\", sizeof(%s));"
                                  visibility       variable_name                variable_name
    makeBeginFunctionPrintfDoc :: String -> Doc
    makeBeginFunctionPrintfDoc function_name = text $ printf "printf(\"\t<function name=\\\"%s\\\">\\n\");"
                                                                                    function_name
    makeEndFunctionPrintfDoc :: Doc
    makeEndFunctionPrintfDoc = text $ printf "printf(\"\t</function>\\n\");"
-- @-node:gcross.20090517181648.5:produceDocumentFromToplevelClassification
-- @-node:gcross.20090506115644.14:Document Production
-- @+node:gcross.20090523222635.14:Processing
-- @+node:gcross.20090523222635.15:processStream
processStream :: InputStream -> Doc
processStream input =
    let block = 
            case execParser_ translUnitP input nopos of
                Left err -> throw (ParseException err)
                Right (CTranslUnit decls _) ->
                    vcat
                    .
                    map (
                            produceDocumentFromToplevelClassification
                            .
                            classifyToplevelDeclaration
                        )
                    $
                    decls
    in  text "extern int printf(const char *, ...);" $+$
        text "int main(int argc, char** argv)" $+$
       (
        braces
        .
        nest 4
        $
        vcat
        [   text "printf(\"<analysis>\\n\");"
        ,   block
        ,   text "printf(\"</analysis>\\n\");"
        ,   text ""
        ]
       )
-- @-node:gcross.20090523222635.15:processStream
-- @+node:gcross.20090523222635.18:processFile
processFile :: String -> String -> IO ()
processFile input_filename output_filename = do
    input <- readInputStream input_filename
    let output = processStream input
    (writeFile output_filename . render) output
-- @-node:gcross.20090523222635.18:processFile
-- @-node:gcross.20090523222635.14:Processing
-- @+node:gcross.20090524230548.5:Analysis
-- @+node:gcross.20090524230548.6:xmlToAnalyzedModule
analysis_B = B8.pack "analysis"
exported_B = B8.pack "exported-variable"
hidden_B = B8.pack "hidden-variable"
static_B = B8.pack "static-variable"
name_B = B8.pack "name"
size_B = B8.pack "size"
function_B = B8.pack "function"

xmlToAnalyzedModule :: Node ByteString ByteString -> AnalyzedModule
xmlToAnalyzedModule (Element name attributes children) = assert (name == analysis_B) $
    AnalyzedModule
        {   exportedVariables = Trie.fromList . readVariables exported_B $ children
        ,   hiddenVariables = Trie.fromList . readVariables hidden_B $ children
        ,   functionsWithStatics =
                Trie.fromList
                .
                List.map (\(Element _ attributes children) ->
                    (   fromJust . List.lookup name_B $ attributes
                    ,   Trie.fromList . readVariables static_B $ children
                    )
                  )
                .
                List.filter isFunctionElement
                $
                children
        }
  where
    readVariables vartyp = List.map readVariable . List.filter (isVariableOfType vartyp)

    readVariable (Element _ attributes _ ) =
        let name = fromJust . List.lookup name_B $ attributes
            size = read . B8.unpack . fromJust . List.lookup size_B $ attributes
        in (name,size)

    isFunctionElement (Element name _ _) = (name == function_B)
    isFunctionElement _ = False

    isVariableOfType vartyp (Element name _ _) = vartyp == name
    isVariableOfType _ _ = False
-- @-node:gcross.20090524230548.6:xmlToAnalyzedModule
-- @-node:gcross.20090524230548.5:Analysis
-- @-others
-- @-node:gcross.20090502101608.4:@thin SizeAnalysis.hs
-- @-leo
