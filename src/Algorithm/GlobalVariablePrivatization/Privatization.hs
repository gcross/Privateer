-- @+leo-ver=4-thin
-- @+node:gcross.20090708193517.2:@thin Privatization.hs
-- @@language Haskell

-- @<< Language Extensions >>
-- @+node:gcross.20090710174219.2:<< Language Extensions >>
-- @-node:gcross.20090710174219.2:<< Language Extensions >>
-- @nl

module Algorithm.GlobalVariablePrivatization.Privatization where

-- @<< Imports >>
-- @+node:gcross.20090709200011.2:<< Imports >>
import Prelude hiding (map,(++),null,concat,filter,head)

import Control.Arrow
import Control.Monad
import Control.Monad.Reader
import Control.Monad.RWS

import Data.DList (DList)
import qualified Data.DList as DList
import Data.List.Stream
import Data.Generics
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set

import Language.C

import Text.PrettyPrint

import Algorithm.GlobalVariablePrivatization.Common

import Debug.Trace
-- @-node:gcross.20090709200011.2:<< Imports >>
-- @nl

-- @+others
-- @+node:gcross.20090709200011.22:Constants
-- @+node:gcross.20090709200011.49:Specifiers
-- @+node:gcross.20090709200011.18:static_specifier
static_specifier = CStorageSpec (CStatic internalNode)
-- @-node:gcross.20090709200011.18:static_specifier
-- @+node:gcross.20090709200011.23:void_specifier
void_specifier = CTypeSpec (CVoidType internalNode)
-- @-node:gcross.20090709200011.23:void_specifier
-- @-node:gcross.20090709200011.49:Specifiers
-- @+node:gcross.20090709200011.50:Indirections
-- @+node:gcross.20090709200011.51:pointer_indirection
pointer_indirection :: CDerivedDeclr
pointer_indirection = CPtrDeclr [] internalNode
-- @-node:gcross.20090709200011.51:pointer_indirection
-- @+node:gcross.20090709200011.52:function_without_arguments_indirection
function_without_arguments_indirection = CFunDeclr (Left []) [] internalNode
-- @nonl
-- @-node:gcross.20090709200011.52:function_without_arguments_indirection
-- @-node:gcross.20090709200011.50:Indirections
-- @-node:gcross.20090709200011.22:Constants
-- @+node:gcross.20090709200011.17:Utilities
-- @+node:gcross.20090709200011.19:makeTypedefSpecifier
makeTypedefSpecifier variable_type_name = CTypeSpec (CTypeDef (internalIdent variable_type_name) internalNode)
-- @-node:gcross.20090709200011.19:makeTypedefSpecifier
-- @+node:gcross.20090709200011.21:makeVariableExpr
makeVariableExpr variable_name = CVar (internalIdent variable_name) internalNode
-- @-node:gcross.20090709200011.21:makeVariableExpr
-- @+node:gcross.20090709200011.25:makeIntegerExpr
makeIntegerExpr value = CConst (CIntConst (cInteger.toInteger $ value) internalNode)
-- @-node:gcross.20090709200011.25:makeIntegerExpr
-- @+node:gcross.20090709200011.26:makeFloatExpr
makeFloatExpr value = CConst (CFloatConst (cFloat value) internalNode)
-- @-node:gcross.20090709200011.26:makeFloatExpr
-- @+node:gcross.20090709200011.27:makeStringExpr
makeStringExpr value = CConst (CStrConst (cString value) internalNode)
-- @-node:gcross.20090709200011.27:makeStringExpr
-- @+node:gcross.20090711085032.5:makePointerToGlobalVariable
makePointerToGlobalVariable :: String -> Int -> CExpr
makePointerToGlobalVariable module_data_access_function_name module_data_offset =
    CBinary CAddOp
        (CCall (makeVariableExpr module_data_access_function_name) [] internalNode)
        (makeIntegerExpr module_data_offset)
        internalNode
-- @-node:gcross.20090711085032.5:makePointerToGlobalVariable
-- @+node:gcross.20090709200011.54:makeCastedPointerToGlobalVariable
makeCastedPointerToGlobalVariable :: String -> CDeclSpec -> [CDerivedDeclr] -> Int -> CExpr
makeCastedPointerToGlobalVariable module_data_access_function_name typedef_spec variable_indirections module_data_offset =
    let cast_declr = CDeclr Nothing (pointer_indirection:variable_indirections) Nothing [] internalNode
        cast_decl = CDecl
                        [typedef_spec]
                        [(Just cast_declr,Nothing,Nothing)]
                        internalNode
        pointer_expr = makePointerToGlobalVariable module_data_access_function_name module_data_offset
    in CCast cast_decl pointer_expr internalNode
-- @-node:gcross.20090709200011.54:makeCastedPointerToGlobalVariable
-- @+node:gcross.20090709200011.53:addAccessorFunctionIndirections
addAccessorFunctionIndirectionsTo :: [CDerivedDeclr] -> [CDerivedDeclr]
addAccessorFunctionIndirectionsTo indirections = function_without_arguments_indirection : pointer_indirection : indirections
-- @-node:gcross.20090709200011.53:addAccessorFunctionIndirections
-- @+node:gcross.20090709200011.65:isStorageSpec
isStorageSpec :: CDeclSpec -> Bool
isStorageSpec (CStorageSpec _) = True
isStorageSpec _ = False
-- @nonl
-- @-node:gcross.20090709200011.65:isStorageSpec
-- @+node:gcross.20090709200011.64:makeTypedefDeclFrom
makeTypedefDeclFrom :: CDecl -> (CDeclSpec,CDecl)
makeTypedefDeclFrom (CDecl specification declarators _) =
    let (Just (CDeclr (Just ident) _ _ _ _),_,_) = head declarators
        typedef_name = ("__type_of__" ++ ) . identToString $ ident
        typedef_spec = makeTypedefSpecifier typedef_name

        new_specification = (CStorageSpec (CTypedef internalNode)) : filter (not . isStorageSpec) specification
        new_declarators = [(Just (CDeclr (Just . internalIdent $ typedef_name) [] Nothing [] internalNode),Nothing,Nothing)]
        typedef_decl = CDecl new_specification new_declarators internalNode
    in (typedef_spec,typedef_decl)
-- @-node:gcross.20090709200011.64:makeTypedefDeclFrom
-- @+node:gcross.20090711085032.4:makeMemcpyStmt
makeMemcpyStmt :: String -> CExpr -> CStat
makeMemcpyStmt source_variable_name destination_expr = flip CExpr internalNode . Just $
    CCall
        (makeVariableExpr "memcpy")
        [   destination_expr
        ,   CUnary CAdrOp (makeVariableExpr source_variable_name) internalNode
        ,   CSizeofExpr (makeVariableExpr source_variable_name) internalNode
        ]
        internalNode
-- @-node:gcross.20090711085032.4:makeMemcpyStmt
-- @+node:gcross.20090711085032.15:makeCompoundStmt
makeCompoundStmt :: [CBlockItem] -> CStat
makeCompoundStmt items = CCompound [] items internalNode
-- @-node:gcross.20090711085032.15:makeCompoundStmt
-- @+node:gcross.20090709200011.66:echo
echo :: (a -> String) -> a -> a
echo show value = trace (show value) $ value
-- @-node:gcross.20090709200011.66:echo
-- @-node:gcross.20090709200011.17:Utilities
-- @+node:gcross.20090709200011.20:Processing
-- @+node:gcross.20090708193517.3:makeAccessor
makeAccessor :: String -> Bool -> String -> CDeclSpec -> [CDerivedDeclr] -> Int -> CFunDef
makeAccessor module_data_access_function_name is_static variable_name typedef_spec variable_indirections module_data_offset =
    let specification =   typedef_spec
                        : (CTypeQual (CInlineQual internalNode))
                        : if is_static then static_specifier:[] else []
        declarator =
            let ident = internalIdent ("__access__" ++ variable_name)
                indirections =  addAccessorFunctionIndirectionsTo variable_indirections
            in CDeclr (Just ident) indirections Nothing [] internalNode
        statement =
            let cast_expr = makeCastedPointerToGlobalVariable
                                module_data_access_function_name
                                typedef_spec
                                variable_indirections
                                module_data_offset
                return_stmt = CReturn (Just cast_expr) internalNode
            in CCompound [] [CBlockStmt return_stmt] internalNode
    in CFunDef specification declarator [] statement internalNode
-- @-node:gcross.20090708193517.3:makeAccessor
-- @+node:gcross.20090709200011.16:makeInitializer
makeInitializer :: String -> CDeclSpec -> [CDerivedDeclr] -> Maybe CInit -> CFunDef
makeInitializer variable_name typedef_spec variable_indirections maybe_init =
    let specifiers = [static_specifier,void_specifier]
        declarator =
            let maybe_ident = (Just . internalIdent . ("__initialize__" ++) $ variable_name)
                indirections = [function_without_arguments_indirection]
            in CDeclr maybe_ident indirections Nothing [] internalNode
        block_items =
            case maybe_init of
                Nothing -> []
                Just init ->
                    let declare_default_value = 
                            let specifiers = [static_specifier, typedef_spec]
                                declarator = CDeclr (Just . internalIdent $ "__initial__value__") variable_indirections Nothing [] internalNode
                            in CDecl specifiers [(Just declarator ,maybe_init,Nothing)] internalNode
                        copy_into_variable = makeMemcpyStmt
                                                "__initial__value__"
                                                (CCall (makeVariableExpr $ "__access__" ++ variable_name) [] internalNode)
                    in  [CBlockDecl declare_default_value
                        ,CBlockStmt copy_into_variable
                        ]
        statement = CCompound [] block_items internalNode
    in CFunDef specifiers declarator [] statement internalNode
-- @-node:gcross.20090709200011.16:makeInitializer
-- @+node:gcross.20090709200011.48:processExtern
processExtern :: Set String -> CDecl -> CDecl
processExtern global_variables (CDecl specification declarators _)
    = CDecl specification (map processDeclarator declarators) internalNode
  where
    processDeclarator declr@(Just (CDeclr (Just ident) indirections maybe_cstrlit attributes _), Nothing, expr)
        = if (flip Set.member global_variables) . identToString $ ident
            then (Just (CDeclr (Just new_ident) new_indirections maybe_cstrlit attributes internalNode), Nothing, expr)
            else declr
      where
        new_ident = internalIdent . ("__access__"++) . identToString $ ident
        new_indirections = addAccessorFunctionIndirectionsTo indirections
-- @-node:gcross.20090709200011.48:processExtern
-- @+node:gcross.20090711085032.20:processToplevelDeclaration
processToplevelDeclaration :: String -> Set String -> Map String Int -> Map String (Map String Int) -> CExtDecl -> [CExtDecl]
processToplevelDeclaration _ _ _ _ decl@(CAsmExt _) = [decl]
processToplevelDeclaration module_data_accessor_name global_variables _ function_static_variables (CFDefExt fundef) =
    let CFunDef _ (CDeclr (Just ident) _ _ _ _) _ _ _ = fundef
        local_static_variables = fromMaybe (Map.empty) (Map.lookup (identToString ident) function_static_variables)
        privatized_function = CFDefExt $ privatizeFunction module_data_accessor_name global_variables local_static_variables fundef
    in privatized_function :
        if Map.null local_static_variables
            then []
            else (CFDefExt $ processFunction module_data_accessor_name local_static_variables fundef) : []
processToplevelDeclaration module_data_accessor_name global_variables global_variable_index_map _ ext_decl@(CDeclExt decl@(CDecl specification declarators _)) =
    case extractStorage specification of
        Just (CTypedef _) -> [ext_decl]
        Just (CExtern _) -> [CDeclExt . processExtern global_variables $ decl]
        Just (CStatic _) -> processGlobalsDeclaration True
        Just (CAuto _) -> processGlobalsDeclaration False
        Nothing -> processGlobalsDeclaration False
  where
    processGlobalsDeclaration is_static =
        let function_declarators = filter isFunctionDeclarator declarators
            functions_declaration = CDeclExt $ CDecl [typedef_spec] function_declarators internalNode
            prepend_functions_declaration = if null function_declarators then id else (functions_declaration:)
            variable_declarations =
                concat
                .
                map processVariable
                .
                filter isVariableDeclarator
                $
                declarators
        in (CDeclExt typedef_declaration) : (prepend_functions_declaration variable_declarations)
      where
        (typedef_spec,typedef_declaration) = makeTypedefDeclFrom decl

        isFunctionDeclarator :: (Maybe CDeclr, Maybe CInit, Maybe CExpr) -> Bool
        isFunctionDeclarator (Just (CDeclr _ indirections _ _ _),_,_) =
            case indirections of 
                ((CFunDeclr _ _ _):_) -> True
                _ -> False

        isVariableDeclarator = not . isFunctionDeclarator

        processVariable :: (Maybe CDeclr, Maybe CInit, Maybe CExpr) -> [CExtDecl]
        processVariable (Just (CDeclr (Just ident) variable_indirections _ _ _), maybe_init, _) =
            let variable_name = identToString ident 
                variable_offset = fromJust (Map.lookup variable_name global_variable_index_map)
                variable_accessor = makeAccessor module_data_accessor_name is_static variable_name typedef_spec variable_indirections variable_offset
                variable_initializer = makeInitializer variable_name typedef_spec variable_indirections maybe_init
            in map CFDefExt [variable_accessor, variable_initializer]
-- @-node:gcross.20090711085032.20:processToplevelDeclaration
-- @+node:gcross.20090711085032.32:processTranslUnit
processTranslUnit :: String -> Set String -> Map String Int -> Map String (Map String Int) -> CTranslUnit -> CTranslUnit
processTranslUnit a b c d (CTranslUnit decls _) = (flip CTranslUnit internalNode) . concat . map (processToplevelDeclaration a b c d) $ decls
-- @-node:gcross.20090711085032.32:processTranslUnit
-- @-node:gcross.20090709200011.20:Processing
-- @+node:gcross.20090709200011.55:Function Processing
-- @+node:gcross.20090709200011.56:Types
data FunctionProcessingEnvironment = FunctionProcessingEnvironment
    {    globalModuleDataAccessorName :: String
    ,    localStaticVariableIndexMap :: Map String Int
    } deriving (Show)

type CurrentVariablesState = (Set String,Set String)

type FunctionProcessingMonad = RWS FunctionProcessingEnvironment () CurrentVariablesState
-- @-node:gcross.20090709200011.56:Types
-- @+node:gcross.20090709200011.57:Convenience Functions
-- @+node:gcross.20090709200011.58:isXXXVariable
isGlobalVariable :: String -> FunctionProcessingMonad Bool
isGlobalVariable name = gets fst >>= return . Set.member name

isLocalStaticVariable :: String -> FunctionProcessingMonad Bool
isLocalStaticVariable name = gets snd >>= return . Set.member name
-- @-node:gcross.20090709200011.58:isXXXVariable
-- @+node:gcross.20090709200011.60:addLocalVariableNames
addLocalStaticVariableNames :: [String] -> FunctionProcessingMonad ()
addLocalStaticVariableNames names = modify (second addNames)
  where
    names_as_set = Set.fromList names
    addNames :: Set String -> Set String
    addNames old_names =
        if Set.null (old_names `Set.intersection` names_as_set)
            then (old_names `Set.union` names_as_set)
            else error "We have detected that there is a place where a local static variable within a function was shadowed by another local static variable with the same name.  Code has not been written to properly handle this case, since we currently assume that there is only one local static variable with a given name inside of a function, so we are aborting the transformation now."
-- @-node:gcross.20090709200011.60:addLocalVariableNames
-- @+node:gcross.20090709200011.62:shadowNames
shadowNames names = modify (removeNames *** removeNames)
  where
    removeNames = (`Set.difference` (Set.fromList names))
-- @-node:gcross.20090709200011.62:shadowNames
-- @-node:gcross.20090709200011.57:Convenience Functions
-- @+node:gcross.20090709200011.59:Privatization Functions
-- @+node:gcross.20090709200011.31:privatizeExpr
privatizeExpr :: CExpr -> FunctionProcessingMonad CExpr
privatizeExpr var@(CVar ident _) =
  do
    is_local_static_variable <- isLocalStaticVariable name
    is_global_variable <- isGlobalVariable name
    case (is_local_static_variable,is_global_variable) of
        (True,_) -> return $ CUnary CIndOp var internalNode
        (False,True) -> return $ CUnary CIndOp (CCall (makeVariableExpr $ "__access__" ++ name) [] internalNode) internalNode
        (False,False) -> return var
  where
    name = identToString ident

privatizeExpr expr = privatizeSubtermsOf expr
  where
    privatizeSubtermsOf :: GenericM FunctionProcessingMonad
    privatizeSubtermsOf = gmapM (privatizeSubtermsOf `extM` privatizeExpr)
-- @-node:gcross.20090709200011.31:privatizeExpr
-- @+node:gcross.20090709200011.40:privatizeStmt
privatizeStmt :: CStat -> FunctionProcessingMonad CStat
privatizeStmt stmt =
    case stmt of
        CCompound labels block_items _ -> do
            old_state <- get
            processed_block_items <- foldM privatizeBlockItem DList.empty block_items >>= return . DList.toList
            put old_state
            return (CCompound labels processed_block_items internalNode)
        _ -> privatizeSubtermsOf stmt
  where
    privatizeSubtermsOf :: GenericM FunctionProcessingMonad
    privatizeSubtermsOf = gmapM (privatizeSubtermsOf `extM` privatizeExpr `extM` privatizeStmt)
-- @-node:gcross.20090709200011.40:privatizeStmt
-- @+node:gcross.20090709200011.47:privatizeBlockItem
privatizeBlockItem :: DList CBlockItem -> CBlockItem -> FunctionProcessingMonad (DList CBlockItem)
privatizeBlockItem privatized_block_items item
    = case item of
        CBlockStmt stmt ->
            privatizeStmt stmt >>= return . (privatized_block_items `DList.snoc`) . CBlockStmt
        CBlockDecl decl@(CDecl specification declarators _) ->
            let passThru :: FunctionProcessingMonad (DList CBlockItem)
                passThru = return $ privatized_block_items `DList.snoc` item

                shadow :: FunctionProcessingMonad (DList CBlockItem)
                shadow = do
                    shadowNames . extractNamesFromDeclarators $ declarators
                    passThru
            in case extractStorage specification of
                Nothing -> shadow
                Just (CAuto _) -> shadow
                Just (CRegister _) -> shadow
                Just (CTypedef _) -> passThru
                Just (CExtern _) -> do
                    global_variable_names <- gets fst
                    return
                        .
                        (privatized_block_items `DList.snoc`)
                        .
                        CBlockDecl
                        .
                        processExtern global_variable_names
                        $
                        decl
                Just (CStatic _) -> do
                    module_data_access_function_name <- asks globalModuleDataAccessorName
                    local_static_index_map <- asks localStaticVariableIndexMap
                    let (typedef_spec,typedef_decl) = makeTypedefDeclFrom decl
                        processDeclarator :: [String] -> (Maybe CDeclr, Maybe CInit, Maybe CExpr) -> ([String],(Maybe CDeclr, Maybe CInit, Maybe CExpr))
                        processDeclarator names declarator =
                            let (Just (CDeclr (Just ident) indirections maybe_cstrlit attrs _),_,maybe_expr) = declarator
                                name = identToString ident
                                new_declarator = CDeclr (Just ident) (pointer_indirection:indirections) maybe_cstrlit attrs internalNode
                                new_initializer = flip CInitExpr internalNode $
                                    makeCastedPointerToGlobalVariable
                                        module_data_access_function_name
                                        typedef_spec
                                        indirections
                                        (fromJust $ Map.lookup name local_static_index_map)
                            in ((name:names),(Just new_declarator,Just new_initializer,maybe_expr))
                        (variable_names,new_declarators) = mapAccumL processDeclarator [] declarators
                        variable_decl = CDecl [typedef_spec] new_declarators internalNode
                    addLocalStaticVariableNames variable_names
                    return $ (privatized_block_items `DList.snoc` (CBlockDecl typedef_decl)) `DList.snoc` (CBlockDecl variable_decl)
-- @-node:gcross.20090709200011.47:privatizeBlockItem
-- @+node:gcross.20090710174219.10:privatizeFunction
privatizeFunction :: String -> Set String -> Map String Int -> CFunDef -> CFunDef
privatizeFunction
    module_data_accessor_name
    global_variables
    local_static_variable_index_map
    (CFunDef specification declarator declarations statement _)
    =
    let CDeclr (Just ident) ((CFunDeclr args _ _):_) _ _ _ = declarator
        variables_to_shadow = Set.fromList $
            case args of
                Left idents -> map identToString idents
                Right (declarations,_) -> extractNamesFromDeclarations declarations
        (new_statement,_,_) = runRWS (privatizeStmt statement)
                                (FunctionProcessingEnvironment module_data_accessor_name local_static_variable_index_map)
                                (global_variables `Set.difference` variables_to_shadow,Set.empty)
    in CFunDef specification declarator declarations new_statement internalNode
-- @-node:gcross.20090710174219.10:privatizeFunction
-- @-node:gcross.20090709200011.59:Privatization Functions
-- @+node:gcross.20090711085032.2:Initializer Functions
-- @+node:gcross.20090711085032.14:makeInitializerStmt
makeInitializerStmt :: String -> Reader FunctionProcessingEnvironment CStat
makeInitializerStmt name =
    liftM2 makePointerToGlobalVariable (asks globalModuleDataAccessorName) (asks localStaticVariableIndexMap >>= return . fromJust . Map.lookup name)
    >>=
    return . makeMemcpyStmt name
-- @-node:gcross.20090711085032.14:makeInitializerStmt
-- @+node:gcross.20090711085032.3:processBlockItem
processBlockItem :: CBlockItem -> Reader FunctionProcessingEnvironment [CBlockItem]
processBlockItem item =
    case item of
        CBlockStmt stat ->
            forM (extractNestedBlocksFromStatement stat) (
                mapM processBlockItem
                >=>
                return
                    .
                    (\list -> if null list then Nothing else Just (CBlockStmt . makeCompoundStmt $ list))
                    .
                    concat
            )
            >>=
            return . catMaybes
        CBlockDecl decl@(CDecl specification declarators _) ->
            case extractStorage specification of
                Just (CStatic _) ->
                    mapM makeInitializerStmt (extractNamesFromDeclarators declarators)
                    >>=
                    (return
                        .
                        (item:)
                        .
                        map CBlockStmt
                    )
                _ -> return $ [item]
-- @-node:gcross.20090711085032.3:processBlockItem
-- @+node:gcross.20090711085032.12:processStmt
processStmt :: CStat -> Reader FunctionProcessingEnvironment CStat
processStmt stat =
    processBlockItem (CBlockStmt stat)
    >>=
    \new_items_dlist -> return $
        case new_items_dlist of
            [CBlockStmt stmt] -> stmt
            new_items -> CCompound [] new_items internalNode
-- @-node:gcross.20090711085032.12:processStmt
-- @+node:gcross.20090711085032.21:processFunction
processFunction :: String -> Map String Int -> CFunDef -> CFunDef
processFunction
    module_data_accessor_name
    local_static_variable_index_map
    (CFunDef _ declarator declarations statement _)
    =
    let CDeclr (Just ident) ((CFunDeclr args _ _):_) _ _ _ = declarator
        new_declarator = CDeclr (Just ident) [CFunDeclr (Left []) [] internalNode] Nothing [] internalNode
        argument_declarations =
            case args of
                Left _ -> declarations
                Right (declarations,_) -> declarations
        processed_statement = runReader (processStmt statement)
                                (FunctionProcessingEnvironment module_data_accessor_name local_static_variable_index_map)
        new_statement = if null argument_declarations
                            then processed_statement
                            else CCompound [] ((map CBlockDecl argument_declarations)++(
                                    case processed_statement of
                                        CCompound labels items _ -> items
                                        other -> [CBlockStmt other]
                                )) internalNode
        new_specification = [CStorageSpec (CStatic internalNode), CTypeQual (CInlineQual internalNode), CTypeSpec (CVoidType internalNode)]
    in CFunDef new_specification new_declarator [] new_statement internalNode
-- @-node:gcross.20090711085032.21:processFunction
-- @-node:gcross.20090711085032.2:Initializer Functions
-- @-node:gcross.20090709200011.55:Function Processing
-- @-others
-- @-node:gcross.20090708193517.2:@thin Privatization.hs
-- @-leo
