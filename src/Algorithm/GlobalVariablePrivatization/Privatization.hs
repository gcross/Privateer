-- @+leo-ver=4-thin
-- @+node:gcross.20090708193517.2:@thin Privatization.hs
-- @@language Haskell

-- @<< Language Extensions >>
-- @+node:gcross.20090710174219.2:<< Language Extensions >>
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RelaxedPolyRec #-}
-- @-node:gcross.20090710174219.2:<< Language Extensions >>
-- @nl

module Algorithm.GlobalVariablePrivatization.Privatization where

-- @<< Imports >>
-- @+node:gcross.20090709200011.2:<< Imports >>
import Control.Arrow
import Control.Monad
import Control.Monad.RWS

import Data.DList (DList)
import qualified Data.DList as DList
import Data.Generics
import Data.List
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
-- @+node:gcross.20090709200011.54:makeCastedPointerToGlobalVariable
makeCastedPointerToGlobalVariable :: String -> CDeclSpec -> [CDerivedDeclr] -> Int -> CExpr
makeCastedPointerToGlobalVariable module_data_access_function_name typedef_spec variable_indirections module_data_offset =
    let cast_declr = CDeclr Nothing (pointer_indirection:variable_indirections) Nothing [] internalNode
        cast_decl = CDecl
                        [typedef_spec]
                        [(Just cast_declr,Nothing,Nothing)]
                        internalNode
        pointer_expr = CBinary CAddOp
                            (CCall (makeVariableExpr module_data_access_function_name) [] internalNode)
                            (makeIntegerExpr module_data_offset)
                            internalNode
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
                        copy_into_variable = flip CExpr internalNode . Just $
                            CCall
                                (makeVariableExpr "memcpy")
                                [   CCall (makeVariableExpr $ "__access__" ++ variable_name) [] internalNode
                                ,   CUnary CAdrOp (makeVariableExpr "__initial__value__") internalNode
                                ,   CSizeofExpr (makeVariableExpr "__initial__value__") internalNode
                                ]
                                internalNode
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

privatizeExpr expr = gmapM (mkM privatizeExpr) expr
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
privatizeFunction :: String -> Map String Int -> Set String -> CFunDef -> CFunDef
privatizeFunction
    module_data_accessor_name
    local_static_variable_index_map
    global_variables
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
-- @-node:gcross.20090709200011.55:Function Processing
-- @-others
-- @-node:gcross.20090708193517.2:@thin Privatization.hs
-- @-leo
