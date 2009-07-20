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
import Data.Either.Unwrap
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
-- @+node:gcross.20090718130736.5:Prefixes
-- @+node:gcross.20090718130736.6:initializers
prefixVariableInitializer = ("__initialize__" ++)
prefixFunctionStaticsInitializer = ("__initialize_statics_in__" ++)
-- @-node:gcross.20090718130736.6:initializers
-- @-node:gcross.20090718130736.5:Prefixes
-- @+node:gcross.20090709200011.17:Utilities
-- @+node:gcross.20090709200011.19:makeTypedefSpecifier
makeTypedefSpecifier variable_type_name = CTypeSpec (CTypeDef (internalIdent variable_type_name) internalNode)
-- @-node:gcross.20090709200011.19:makeTypedefSpecifier
-- @+node:gcross.20090709200011.21:makeVariableExpr
makeVariableExpr variable_name = CVar (internalIdent variable_name) internalNode
-- @-node:gcross.20090709200011.21:makeVariableExpr
-- @+node:gcross.20090709200011.25:makeIntegerExpr
makeIntegerExpr :: Integer -> CExpr
makeIntegerExpr = CConst . flip CIntConst internalNode . cInteger
-- @-node:gcross.20090709200011.25:makeIntegerExpr
-- @+node:gcross.20090709200011.26:makeFloatExpr
makeFloatExpr = CConst . flip CFloatConst internalNode . cFloat
-- @-node:gcross.20090709200011.26:makeFloatExpr
-- @+node:gcross.20090709200011.27:makeStringExpr
makeStringExpr = CConst . flip CStrConst internalNode . cString
-- @-node:gcross.20090709200011.27:makeStringExpr
-- @+node:gcross.20090711085032.5:makePointerToGlobalVariable
makePointerToGlobalVariable :: String -> Integer -> CExpr
makePointerToGlobalVariable module_data_access_function_name module_data_offset =
    CBinary CAddOp
        (CCall (makeVariableExpr module_data_access_function_name) [] internalNode)
        (makeIntegerExpr module_data_offset)
        internalNode
-- @-node:gcross.20090711085032.5:makePointerToGlobalVariable
-- @+node:gcross.20090709200011.54:makeCastedPointerToGlobalVariable
makeCastedPointerToGlobalVariable :: String -> CDeclSpec -> [CDerivedDeclr] -> Integer -> CExpr
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
-- @+node:gcross.20090709200011.66:echo
echo :: (a -> String) -> a -> a
echo show value = trace (show value) $ value
-- @-node:gcross.20090709200011.66:echo
-- @+node:gcross.20090718130736.7:makeArgumentlessProcedure
makeArgumentlessProcedure :: Bool -> String -> CStat -> CFunDef
makeArgumentlessProcedure is_static name statement =
    let specifiers = (if is_static then (static_specifier:) else id) [void_specifier]
        declarator =
            let maybe_ident = (Just . internalIdent $ name)
                indirections = [function_without_arguments_indirection]
            in CDeclr maybe_ident indirections Nothing [] internalNode
    in CFunDef specifiers declarator [] statement internalNode
-- @-node:gcross.20090718130736.7:makeArgumentlessProcedure
-- @-node:gcross.20090709200011.17:Utilities
-- @+node:gcross.20090718130736.9:Std C Function Imports
-- @+node:gcross.20090718130736.10:import_memcpy
import_memcpy = fromRight . flip (execParser_ extDeclP) nopos . inputStreamFromString $
    "extern void *memcpy(void *restrict s1, const void *restrict s2, int n);"
-- @-node:gcross.20090718130736.10:import_memcpy
-- @-node:gcross.20090718130736.9:Std C Function Imports
-- @+node:gcross.20090718130736.8:Function generators
-- @+node:gcross.20090708193517.3:makeAccessor
makeAccessor :: String -> Bool -> String -> CDeclSpec -> [CDerivedDeclr] -> Integer -> CFunDef
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
makeInitializer :: Set String -> String -> CDeclSpec -> [CDerivedDeclr] -> Maybe CInit -> CFunDef
makeInitializer global_variables variable_name typedef_spec variable_indirections maybe_init =
    let name = prefixVariableInitializer variable_name
        statement = makeCompoundStmt $
            case maybe_init of
                Nothing -> []
                Just init ->
                    let declare_default_value = 
                            let specifiers = [typedef_spec]
                                declarator = CDeclr (Just . internalIdent $ "__initial__value__") variable_indirections Nothing [] internalNode

                                (new_init,_,_) = runRWS (everywhereM (mkM privatizeExpr) init)
                                    (FunctionProcessingEnvironment undefined undefined)
                                    (global_variables,Set.empty)
                            in CDecl specifiers [(Just declarator,Just new_init,Nothing)] internalNode
                        copy_into_variable = makeMemcpyStmt
                                                "__initial__value__"
                                                (CCall (makeVariableExpr $ "__access__" ++ variable_name) [] internalNode)
                    in  [CBlockDecl declare_default_value
                        ,CBlockStmt copy_into_variable
                        ]
    in makeArgumentlessProcedure True name statement
-- @-node:gcross.20090709200011.16:makeInitializer
-- @+node:gcross.20090718130736.3:makeInitializerForwardDeclaration
makeInitializerForwardDeclaration :: [String] -> [String] -> CDecl
makeInitializerForwardDeclaration global_variable_names functions_with_statics_names =
    let specification = [static_specifier,void_specifier]
        declarators =
            map (makeDeclarator . prefixVariableInitializer) global_variable_names
         ++ map (makeDeclarator . prefixFunctionStaticsInitializer) functions_with_statics_names
    in CDecl specification declarators internalNode
  where
    makeDeclarator name = (Just (CDeclr (Just . internalIdent $ name) [function_without_arguments_indirection] Nothing [] internalNode),Nothing,Nothing)
-- @-node:gcross.20090718130736.3:makeInitializerForwardDeclaration
-- @+node:gcross.20090718130736.4:makeModuleInitializer
makeModuleInitializer :: String -> [String] -> [String] -> CFunDef
makeModuleInitializer name global_variable_names functions_with_statics_names =
    let statement = makeCompoundStmt $
            map (makeCall . prefixVariableInitializer) global_variable_names
         ++ map (makeCall . prefixFunctionStaticsInitializer) functions_with_statics_names
    in makeArgumentlessProcedure False name statement
  where
    makeCall name = CBlockStmt . (flip CExpr internalNode) . Just $ CCall (makeVariableExpr name) [] internalNode
-- @-node:gcross.20090718130736.4:makeModuleInitializer
-- @-node:gcross.20090718130736.8:Function generators
-- @+node:gcross.20090709200011.20:Processing
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
processToplevelDeclaration :: String -> Set String -> (String -> Integer) -> (String -> Maybe (String -> Integer)) -> CExtDecl -> [CExtDecl]
processToplevelDeclaration _ _ _ _ decl@(CAsmExt _) = [decl]
processToplevelDeclaration module_data_accessor_name global_variables _ getFunctionStaticVariableOffset (CFDefExt fundef) =
    let CFunDef _ (CDeclr (Just ident) _ _ _ _) _ _ _ = fundef
        function_name = identToString ident
        maybe_getStaticVariableOffset = getFunctionStaticVariableOffset function_name
        getStaticVariableOffset = fromMaybe
            (\variable_name -> error $ "Error:  First pass of '" ++ function_name ++ "' didn't see any static variables, but in the second pass we saw a variable named '" ++ variable_name ++ "'!")
            maybe_getStaticVariableOffset
        privatized_function = CFDefExt $ privatizeFunction module_data_accessor_name global_variables getStaticVariableOffset fundef
    in privatized_function :
        if isNothing maybe_getStaticVariableOffset
            then []
            else (CFDefExt $ processFunction module_data_accessor_name global_variables getStaticVariableOffset fundef) : []
processToplevelDeclaration module_data_accessor_name global_variables getGlobalVariableOffset _ ext_decl@(CDeclExt decl@(CDecl specification declarators _)) =
    case extractStorage specification of
        Just (CTypedef _) -> [ext_decl]
        Just (CExtern _) -> [CDeclExt . processExtern global_variables $ decl]
        Just (CStatic _) -> processGlobalsDeclaration True
        Just (CAuto _) -> processGlobalsDeclaration False
        Nothing -> processGlobalsDeclaration False
  where
    processGlobalsDeclaration is_static =
        if null declarators then [ext_decl] else
        let function_specifiers = typedef_spec :
                case extractStorage specification of
                    Nothing -> []
                    Just storage -> CStorageSpec storage : []
            function_declarators = filter isFunctionDeclarator declarators
            functions_declaration = CDeclExt $ CDecl function_specifiers function_declarators internalNode
            prepend_functions_declaration = if null function_declarators then id else (functions_declaration:)
            variable_declarations =
                concat
                .
                map processVariable
                .
                filter isVariableDeclarator
                $
                declarators
        in if null variable_declarations
            then [ext_decl]
            else (CDeclExt typedef_declaration) : (prepend_functions_declaration variable_declarations)
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
                variable_offset = getGlobalVariableOffset variable_name
                variable_accessor = makeAccessor module_data_accessor_name is_static variable_name typedef_spec variable_indirections variable_offset
                variable_initializer = makeInitializer global_variables variable_name typedef_spec variable_indirections maybe_init
            in map CFDefExt [variable_accessor, variable_initializer]
-- @-node:gcross.20090711085032.20:processToplevelDeclaration
-- @+node:gcross.20090711085032.32:processTranslUnit
processTranslUnit :: String -> Set String -> (String -> Integer) -> (String -> Maybe (String -> Integer)) -> CTranslUnit -> CTranslUnit
processTranslUnit a b c d (CTranslUnit decls _) = (flip CTranslUnit internalNode) . concat . map (processToplevelDeclaration a b c d) $ decls
-- @-node:gcross.20090711085032.32:processTranslUnit
-- @-node:gcross.20090709200011.20:Processing
-- @+node:gcross.20090709200011.55:Function Processing
-- @+node:gcross.20090709200011.56:Types
data FunctionProcessingEnvironment = FunctionProcessingEnvironment
    {    globalModuleDataAccessorName :: String
    ,    localStaticVariableOffsetMap :: String -> Integer
    }

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
                passThru = return . (privatized_block_items `DList.snoc`) $ item

                privatizeAndShadow :: FunctionProcessingMonad (DList CBlockItem)
                privatizeAndShadow = do
                        shadowNames . extractNamesFromDeclarators $ declarators
                        privatizeInitializersInDeclaration decl
                            >>= return . (privatized_block_items `DList.snoc`) . CBlockDecl
            in case extractStorage specification of
                Nothing -> privatizeAndShadow
                Just (CAuto _) -> privatizeAndShadow
                Just (CRegister _) -> privatizeAndShadow
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
                    getLocalStaticVariableOffset <- asks localStaticVariableOffsetMap
                    let (typedef_spec,typedef_decl) = makeTypedefDeclFrom decl
                        processDeclarator :: [String] -> (Maybe CDeclr, Maybe CInit, Maybe CExpr) -> ([String],(Maybe CDeclr, Maybe CInit, Maybe CExpr))
                        processDeclarator names declarator =
                            let (Just (CDeclr (Just ident) indirections maybe_cstrlit attrs _),_,maybe_expr) = declarator
                                name = identToString ident
                                new_declarator = CDeclr (Just ident) (pointer_indirection:indirections) maybe_cstrlit attrs internalNode
                                new_initializer =
                                    flip CInitExpr internalNode
                                    .
                                    makeCastedPointerToGlobalVariable
                                        module_data_access_function_name
                                        typedef_spec
                                        indirections
                                    .
                                    getLocalStaticVariableOffset
                                    $
                                    name
                            in ((name:names),(Just new_declarator,Just new_initializer,maybe_expr))
                        (variable_names,new_declarators) = mapAccumL processDeclarator [] declarators
                        variable_decl = CDecl [typedef_spec] new_declarators internalNode
                    addLocalStaticVariableNames variable_names
                    return $ (privatized_block_items `DList.snoc` (CBlockDecl typedef_decl)) `DList.snoc` (CBlockDecl variable_decl)
-- @-node:gcross.20090709200011.47:privatizeBlockItem
-- @+node:gcross.20090710174219.10:privatizeFunction
privatizeFunction :: String -> Set String -> (String -> Integer) -> CFunDef -> CFunDef
privatizeFunction
    module_data_accessor_name
    global_variables
    getLocalStaticVariableOffset
    (CFunDef specification declarator declarations statement _)
    =
    let CDeclr (Just ident) ((CFunDeclr args _ _):_) _ _ _ = declarator
        variables_to_shadow = Set.fromList $
            case args of
                Left idents -> map identToString idents
                Right (declarations,_) -> extractNamesFromDeclarations declarations
        (new_statement,_,_) = runRWS (privatizeStmt statement)
                                (FunctionProcessingEnvironment module_data_accessor_name getLocalStaticVariableOffset)
                                (global_variables `Set.difference` variables_to_shadow,Set.empty)
    in CFunDef specification declarator declarations new_statement internalNode
-- @-node:gcross.20090710174219.10:privatizeFunction
-- @+node:gcross.20090718130736.26:privatizeInitializer
privatizeInitializer :: (Maybe CDeclr, Maybe CInit, Maybe CExpr) -> FunctionProcessingMonad (Maybe CDeclr, Maybe CInit, Maybe CExpr)
privatizeInitializer declarator@(maybe_declr, maybe_init, maybe_expr) =
    case maybe_init of 
        Nothing -> return declarator
        Just init -> everywhereM (mkM privatizeExpr) init >>= (\init -> return (maybe_declr,Just init,maybe_expr))
-- @-node:gcross.20090718130736.26:privatizeInitializer
-- @+node:gcross.20090718130736.28:privatizeInitializersInDeclaration
privatizeInitializersInDeclaration :: CDecl -> FunctionProcessingMonad CDecl
privatizeInitializersInDeclaration (CDecl specifiers declarators _) =
    let new_specifiers = removeStorageSpecifiersFrom specifiers
    in mapM privatizeInitializer declarators >>= return . flip (CDecl new_specifiers) internalNode
-- @-node:gcross.20090718130736.28:privatizeInitializersInDeclaration
-- @-node:gcross.20090709200011.59:Privatization Functions
-- @+node:gcross.20090711085032.2:Initializer Functions
-- @+node:gcross.20090711085032.14:makeInitializerStmt
makeInitializerStmt :: String -> FunctionProcessingMonad CStat
makeInitializerStmt name =
    liftM2 makePointerToGlobalVariable (asks globalModuleDataAccessorName) (asks localStaticVariableOffsetMap >>= return . ($ name))
    >>=
    return . makeMemcpyStmt name
-- @-node:gcross.20090711085032.14:makeInitializerStmt
-- @+node:gcross.20090711085032.3:processBlockItem
processBlockItem :: CBlockItem -> FunctionProcessingMonad [CBlockItem]
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
        CBlockDecl decl@(CDecl specifiers declarators _) ->
            case extractStorage specifiers of
                Just (CStatic _) -> do
                    declaration <- privatizeInitializersInDeclaration decl >>= return . CBlockDecl
                    memcpy_stmts <- mapM (makeInitializerStmt >=> return . CBlockStmt) (extractNamesFromDeclarators declarators)
                    return (declaration:memcpy_stmts)
                _ -> return [CBlockDecl . flip (CDecl specifiers) internalNode $ [(a,Nothing,b) | (a,_,b) <- declarators]]
-- @-node:gcross.20090711085032.3:processBlockItem
-- @+node:gcross.20090711085032.12:processStmt
processStmt :: CStat -> FunctionProcessingMonad CStat
processStmt stat =
    processBlockItem (CBlockStmt stat)
    >>=
    \new_items_dlist -> return $
        case new_items_dlist of
            [CBlockStmt stmt] -> stmt
            new_items -> CCompound [] new_items internalNode
-- @-node:gcross.20090711085032.12:processStmt
-- @+node:gcross.20090711085032.21:processFunction
processFunction :: String -> Set String -> (String -> Integer) -> CFunDef -> CFunDef
processFunction
    module_data_accessor_name
    global_variables
    getLocalStaticVariableOffset
    (CFunDef _ declarator declarations statement _)
    =
    let CDeclr (Just ident) ((CFunDeclr args _ _):_) _ _ _ = declarator
        name = prefixFunctionStaticsInitializer . identToString $ ident
        argument_declarations =
            case args of
                Left _ -> declarations
                Right (declarations,_) -> declarations
        (processed_statement,_,_) = runRWS (processStmt statement)
                                (FunctionProcessingEnvironment module_data_accessor_name getLocalStaticVariableOffset)
                                (global_variables,Set.empty)
        new_statement = if null argument_declarations
                            then processed_statement
                            else
                                let new_argument_declarations =
                                        [CBlockDecl decl |
                                            decl@(CDecl _ declarators _) <- argument_declarations,
                                            (not . null) declarators
                                        ]
                                in CCompound [] (new_argument_declarations ++(
                                    case processed_statement of
                                        CCompound labels items _ -> items
                                        other -> [CBlockStmt other]
                                )) internalNode
    in makeArgumentlessProcedure True name new_statement
-- @-node:gcross.20090711085032.21:processFunction
-- @-node:gcross.20090711085032.2:Initializer Functions
-- @-node:gcross.20090709200011.55:Function Processing
-- @-others
-- @-node:gcross.20090708193517.2:@thin Privatization.hs
-- @-leo
