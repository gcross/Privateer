-- @+leo-ver=4-thin
-- @+node:gcross.20090502101608.4:@thin SizeAnalysis.hs
-- @@language Haskell

-- @<< Language Extensions >>
-- @+node:gcross.20090502101608.14:<< Language Extensions >>
{-# LANGUAGE RelaxedPolyRec #-}
{-# LANGUAGE DeriveDataTypeable #-}
-- @-node:gcross.20090502101608.14:<< Language Extensions >>
-- @nl

module Algorithm.GlobalVariablePrivatization.SizeAnalysis where

-- @<< Imports >>
-- @+node:gcross.20090502101608.6:<< Imports >>
import Prelude hiding ((++),unzip,map,any,null,filter)

import Control.Arrow
import Control.Exception

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B8
import Data.Data
import qualified Data.List.Stream as List
import Data.List.Stream
import Data.Maybe
import Data.Trie (Trie)
import qualified Data.Trie as Trie

import Language.C

import Text.PrettyPrint
import Text.Printf
import Text.XML.Expat.Tree

import Algorithm.GlobalVariablePrivatization.Common
import Algorithm.GlobalVariablePrivatization.VariableLayout
-- @-node:gcross.20090502101608.6:<< Imports >>
-- @nl

-- @+others
-- @+node:gcross.20090502101608.7:Types
-- @+node:gcross.20090715105401.18:Classifications
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
-- @-node:gcross.20090715105401.18:Classifications
-- @+node:gcross.20090715105401.19:Analysis
data AnalyzedModule = AnalyzedModule
    {    exportedVariables :: !(Trie Offset)
    ,    hiddenVariables :: !(Trie Offset)
    ,    functionsWithStaticVariables :: !(Trie (Trie Offset))
    } deriving (Show)

data VariableKey =
    ExportedVariableKey ByteString
  | HiddenVariableKey ByteString
  | FunctionStaticVariableKey ByteString ByteString
    deriving (Ord,Eq,Show)

instance (Show a) => Show (Trie a) where
    show = show . Trie.toList
-- @-node:gcross.20090715105401.19:Analysis
-- @-node:gcross.20090502101608.7:Types
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
-- @+node:gcross.20090718130736.19:maximumDesignatorIndexIn
maximumIndexDesignatorIn :: [CDesignator] -> Integer
maximumIndexDesignatorIn designators =
    let (CArrDesig (CConst (CIntConst int _) ) _) = maximumBy compareDesignators designators
    in getCInteger int
    where
        reportBadDesignator ad =
            let s_ad = (render.pretty) ad
            in error $ "array designators with indices other than an explicit numeric constant are not supported when used to initialize an array with unspecified size; problem designator is '" ++ s_ad ++ "'"
        compareDesignators (CArrDesig (CConst (CIntConst int1 _) ) _) (CArrDesig (CConst (CIntConst int2 _) ) _) = (getCInteger int1) `compare` (getCInteger int2)
        compareDesignators (CArrDesig (CConst (CIntConst _ _) ) _) ad = reportBadDesignator ad
        compareDesignators ad _ = reportBadDesignator ad
-- @-node:gcross.20090718130736.19:maximumDesignatorIndexIn
-- @+node:gcross.20090718130736.18:lastArrayDesignatorIndexIn
lastArrayDesignatorIndexIn :: [[CDesignator]] -> Integer
lastArrayDesignatorIndexIn = lastDesignator 0
    where
        lastDesignator :: Integer -> [[CDesignator]] -> Integer
        lastDesignator next [] = next
        lastDesignator next (designators:rest) =
            case designators of
                [] -> lastDesignator (next+1) rest
                _ -> lastDesignator ((maximumIndexDesignatorIn designators)+1) rest
-- @-node:gcross.20090718130736.18:lastArrayDesignatorIndexIn
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
        (variable_names,stripped_declarators) = unzip . catMaybes . map retrieveVariable $ declarators
        stripped_declaration = (CDecl (filter (not . isStorage) decl_specs) stripped_declarators node_info)
        storage = extractStorage decl_specs
    in case storage of
        Just (CExtern _) -> DeclarationImportingExternalObject
        Just (CTypedef _) -> OtherDeclaration decl
        Just (CRegister _) -> OtherDeclaration decl
        Just (CStatic _) -> DeclarationWithVariables Static stripped_declaration variable_names
        Just (CAuto _) -> DeclarationWithVariables Automatic stripped_declaration variable_names
        Nothing -> DeclarationWithVariables Automatic stripped_declaration variable_names
        _ -> error $ "unable to handle storage type " ++ show storage
  where
    retrieveVariable (Just declarator,maybe_init,_) =
        let CDeclr (Just ident) indirections maybe_cstrlit attributes _ = declarator
            name = identToString ident in
        case indirections of
            (CFunDeclr _ _ _:_) -> Nothing
            (CArrDeclr qualifiers (CNoArrSize unknown) _:rest) ->
                let new_size_expr = if unknown
                        then error "cannot handle global variable of array type with unknown size"
                        else case maybe_init of
                            Nothing -> error "global arrays with unspecified size must have an initializer to specify the size"
                            Just (CInitExpr expr _) -> (CSizeofExpr expr internalNode)
                            Just (CInitList list _) ->
                                let last_designator_index = lastArrayDesignatorIndexIn (map fst list)
                                in (CConst (CIntConst (cInteger last_designator_index) internalNode))
                    new_indirections = CArrDeclr qualifiers (CArrSize False new_size_expr) internalNode : rest
                    new_declarator = CDeclr (Just ident) new_indirections maybe_cstrlit attributes internalNode
                in Just (name,(Just new_declarator,Nothing,Nothing))
            _ -> Just (name,(Just declarator,Nothing,Nothing))

    isStorage (CStorageSpec _) = True
    isStorage _ = False
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
                    if not . null $ variable_names
                        then
                            pretty decl <> semi
                            $$ (
                                sep
                                .
                                map (makeGlobalPrintfDoc storage_classification)
                                $
                                variable_names
                            )
                        else
                            let (CDecl specifiers _ _) = decl
                            in if any isNamedType specifiers then pretty decl <> semi else empty
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

    isNamedType (CTypeSpec (CSUType (CStruct _ (Just _) _ _ _) _)) = True
    isNamedType (CTypeSpec (CEnumType (CEnum (Just _) _ _ _) _)) = True
    isNamedType _ = False
-- @-node:gcross.20090517181648.5:produceDocumentFromToplevelClassification
-- @-node:gcross.20090506115644.14:Document Production
-- @+node:gcross.20090523222635.14:Processing
-- @+node:gcross.20090523222635.15:processStream
processStream :: InputStream -> Doc
processStream input =
    case execParser_ translUnitP input nopos of
        Left err -> throw (ParseException err)
        Right transl_unit -> processTranslUnit transl_unit
-- @-node:gcross.20090523222635.15:processStream
-- @+node:gcross.20090715105401.30:processTranslUnit
processTranslUnit :: CTranslUnit -> Doc
processTranslUnit (CTranslUnit decls _) =
    let block = 
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
-- @-node:gcross.20090715105401.30:processTranslUnit
-- @+node:gcross.20090523222635.18:processFile
processFile :: String -> String -> IO ()
processFile input_filename output_filename = do
    input <- readInputStream input_filename
    let output = processStream input
    (writeFile output_filename . render) output
-- @-node:gcross.20090523222635.18:processFile
-- @-node:gcross.20090523222635.14:Processing
-- @+node:gcross.20090524230548.5:Analysis
-- @+node:gcross.20090715105401.23:xmlToRequestList
analysis_B = B8.pack "analysis"
exported_B = B8.pack "exported-variable"
hidden_B = B8.pack "hidden-variable"
static_B = B8.pack "static-variable"
name_B = B8.pack "name"
size_B = B8.pack "size"
function_B = B8.pack "function"

xmlToRequestList :: Node ByteString ByteString -> NamedRequestList VariableKey
xmlToRequestList (Element name _ children) =
    assert (name == analysis_B) $ go children
  where
    go :: [Node ByteString ByteString] -> NamedRequestList VariableKey
    go [] = []
    go (Text _:rest) = go rest
    go (Element tag attributes children:rest)
        | tag == exported_B
            = extractVariableKeyPair ExportedVariableKey attributes : go rest
        | tag == hidden_B
            = extractVariableKeyPair HiddenVariableKey attributes : go rest
        | tag == function_B
            = go2 (extractNameFrom attributes) children rest

    go2 :: ByteString -> [Node ByteString ByteString] -> [Node ByteString ByteString] -> NamedRequestList VariableKey
    go2 _ [] rest = go rest
    go2 name (Text _:rest2) rest = go2 name rest2 rest
    go2 name (Element tag attributes _:rest2) rest
        = assert (tag == static_B)
            $ extractVariableKeyPair (FunctionStaticVariableKey name) attributes : go2 name rest2 rest

    extractNameFrom :: [(ByteString,ByteString)] -> ByteString
    extractNameFrom = fromJust . List.lookup name_B

    extractSizeFrom :: [(ByteString,ByteString)] -> Size
    extractSizeFrom = read . B8.unpack . fromJust . List.lookup size_B

    extractVariableKeyPair
        :: (ByteString -> VariableKey)
        -> [(ByteString,ByteString)]
        -> NamedRequest VariableKey
    extractVariableKeyPair wrapper = (wrapper . extractNameFrom) &&& ((minimumAlignment &&& id) . extractSizeFrom)
-- @-node:gcross.20090715105401.23:xmlToRequestList
-- @+node:gcross.20090715105401.25:moduleWithoutVariables
moduleWithoutVariables = AnalyzedModule Trie.empty Trie.empty Trie.empty
-- @-node:gcross.20090715105401.25:moduleWithoutVariables
-- @+node:gcross.20090715105401.24:allocationListToAnalyzedModule
allocationListToAnalyzedModule :: [(VariableKey,Offset)] -> AnalyzedModule
allocationListToAnalyzedModule = List.foldl' addVariable moduleWithoutVariables --'
  where
    addVariable :: AnalyzedModule -> (VariableKey,Offset) -> AnalyzedModule
    addVariable analyzed_module (key,offset) =
        case key of
            ExportedVariableKey name ->
                analyzed_module {
                    exportedVariables = Trie.insert name offset (exportedVariables analyzed_module)
                }
            HiddenVariableKey name ->
                analyzed_module {
                    hiddenVariables = Trie.insert name offset (hiddenVariables analyzed_module)
                }
            FunctionStaticVariableKey function_name variable_name ->
                let old_statics = functionsWithStaticVariables analyzed_module
                in analyzed_module {
                    functionsWithStaticVariables = Trie.insert function_name (
                        maybe
                            (Trie.singleton variable_name offset)
                            (Trie.insert variable_name offset)
                            (Trie.lookup function_name old_statics)
                    ) old_statics
                }
-- @nonl
-- @-node:gcross.20090715105401.24:allocationListToAnalyzedModule
-- @-node:gcross.20090524230548.5:Analysis
-- @-others
-- @-node:gcross.20090502101608.4:@thin SizeAnalysis.hs
-- @-leo
