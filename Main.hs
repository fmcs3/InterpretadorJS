import qualified Language.ECMAScript3.Parser as Parser
import Language.ECMAScript3.Syntax
import Control.Monad hiding (empty)
import Control.Applicative hiding (empty)
import Data.Map as Map
import Debug.Trace
import Value

--
-- Evaluate functions
--

evalExpr :: StateT -> Expression -> StateTransformer Value
evalExpr env (VarRef (Id id)) = stateLookup env id
evalExpr env (IntLit int) = return $ Int int
evalExpr env (StringLit string) = return $ String string
evalExpr env (InfixExpr op expr1 expr2) = do
    v1 <- evalExpr env expr1
    v2 <- evalExpr env expr2
    infixOp env op v1 v2
evalExpr env (AssignExpr OpAssign (LVar var) expr) = do
    v <- stateLookup env var
    case v of
        -- Variable not defined :(
        (Error _) -> return $ Error $ (show var) ++ " not defined"
        -- Variable defined, let's set its value
        _ -> do
            e <- evalExpr env expr
            setVar var e

----------------------------LISTA------------------------
evalExpr env (ArrayLit []) = return $ (List [])
evalExpr env (ArrayLit b) = do
    a <- mapM (evalExpr env) b
    return $ (List a)

evalExpr env (UnaryAssignExpr a (LVar var)) = do
    v <- stateLookup env var
    case v of
        -- Variable not defined :(
        (Error _) -> return $ Error $ (show var) ++ " not defined"
        -- Variable defined, let's set its value
        _ -> do
            e <-  postfixOp env a (v)
            setVar var e

evalExpr env (CallExpr funcRef funcArgs) = ST $ \s ->
    let (ST a) = return Nil
        (t, newS) = a s
        (ST g) = do
            value  <- evalExpr env funcRef
            case value of
                (Function nome args body) -> do
                    eval <- evalFuncArgs env args funcArgs
                    case eval of
                        (Error a) -> return (Error a)
                        _ -> evalStmt env (BlockStmt body)
                _ -> return (Error "Function not defined")
        (resp,ign) = g newS
        fEnv = intersection ign s
        in (resp,fEnv) 

--Inicializa os Argumentos das Funções
evalFuncArgs :: StateT -> [Id] -> [Expression] -> StateTransformer Value
evalFuncArgs env (x:xs) [] = return (Error "Número de argumentos errados")
evalFuncArgs env [] (x:xs) = return (Error "Número de argumentos errados")
evalFuncArgs env [] [] = return Nil
evalFuncArgs env (x:xs) (y:ys) = do
    val <- evalExpr env y
    setVar (unId x) val
    evalFuncArgs env xs ys   

evalStmt :: StateT -> Statement -> StateTransformer Value
evalStmt env (EmptyStmt) = return Nil
evalStmt env (VarDeclStmt []) = return Nil
evalStmt env (VarDeclStmt (decl:ds)) =
    varDecl env decl >> evalStmt env (VarDeclStmt ds)
evalStmt env (ExprStmt expr) = evalExpr env expr
 


-----------------BlockStmt (x:xs)--------------------------
evalStmt env (BlockStmt []) = return Nil
evalStmt env (BlockStmt (x:xs)) = do
    case x of
        (BreakStmt Nothing) -> return Break
        (ReturnStmt (Just expr)) -> evalExpr env expr
        (ReturnStmt Nothing) -> return Nil
        _ -> do
            evalStmt env x
            evalStmt env (BlockStmt xs)

---------------------BLOCO IF------------------------------ 
evalStmt env (IfSingleStmt expr ifBlock) = do
    ret <- evalExpr env expr
    case ret of 
        err@(Error s) -> return err
        Bool b -> if b == True then evalStmt env ifBlock else evalStmt env EmptyStmt

--------------------BLOCO IF/ELSE------------------------
evalStmt env (IfStmt expr ifBlock elseBlock) = do
	ret <- evalExpr env expr
	case ret of
		(Bool b) -> if b then evalStmt env ifBlock else evalStmt env elseBlock
		(Error _) -> return $ Error $  " not defined"
		
-----------------------------------------------------------

----------------------FUNCTION---------------------------
evalStmt env (FunctionStmt (Id funcId) funcArgs funcBody) = do
    setVar funcId (Function (Id funcId) funcArgs funcBody)

evalStmt env (ExprStmt expr) = evalExpr env expr

-----------------------BLOCO FOR-----------------------------
-----------------------FOR-----------------------------------		
evalStmt env (ForStmt inicio expressao incremento comando) = do
            evalInit env inicio
            case expressao of
                (Just a) -> do
                    ret <- evalExpr env a
                    case ret of
                        (Bool b) -> do 			-- Checando se o for ainda deve ser executado
                            if b then do
                                eval <- evalStmt env comando
                                case eval of    -- Vamos checar se o break foi chamado
                                    Break -> return Nil
                                    _ -> do
                                        case incremento of
                                            (Just expr) -> evalExpr env expr
                                            (Nothing) -> return Nil
                                        evalStmt env (ForStmt NoInit expressao incremento comando)
                            else return Nil  
                (Nothing) -> do
                            eval <- evalStmt env comando
                            case eval of    -- Vamos checar se o break foi chamado
                                Break -> return Nil
                                _ -> do
                                    case incremento of
                                        (Just expr) -> evalExpr env expr
                                        (Nothing) -> return Nil
                                    evalStmt env (ForStmt NoInit expressao incremento comando)

------------------------FORINIT----------------------------
evalInit env (NoInit) = return Nil
evalInit env (VarInit a) = (evalStmt env (VarDeclStmt a))
evalInit env (ExprInit b) = (evalExpr env b)


-----------------------------------------------------       

-- Do not touch this one :)
evaluate :: StateT -> [Statement] -> StateTransformer Value
evaluate env [] = return Nil
evaluate env [stmt] = evalStmt env stmt
evaluate env (s:ss) = evalStmt env s >> evaluate env ss

--
-- Operators
--

infixOp :: StateT -> InfixOp -> Value -> Value -> StateTransformer Value
infixOp env OpAdd  (Int  v1) (Int  v2) = return $ Int  $ v1 + v2
infixOp env OpSub  (Int  v1) (Int  v2) = return $ Int  $ v1 - v2
infixOp env OpMul  (Int  v1) (Int  v2) = return $ Int  $ v1 * v2
infixOp env OpDiv  (Int  v1) (Int  v2) = return $ Int  $ div v1 v2
infixOp env OpMod  (Int  v1) (Int  v2) = return $ Int  $ mod v1 v2
infixOp env OpLT   (Int  v1) (Int  v2) = return $ Bool $ v1 < v2
infixOp env OpLEq  (Int  v1) (Int  v2) = return $ Bool $ v1 <= v2
infixOp env OpGT   (Int  v1) (Int  v2) = return $ Bool $ v1 > v2
infixOp env OpGEq  (Int  v1) (Int  v2) = return $ Bool $ v1 >= v2
infixOp env OpEq   (Int  v1) (Int  v2) = return $ Bool $ v1 == v2
infixOp env OpNEq  (Bool v1) (Bool v2) = return $ Bool $ v1 /= v2
infixOp env OpLAnd (Bool v1) (Bool v2) = return $ Bool $ v1 && v2
infixOp env OpLOr  (Bool v1) (Bool v2) = return $ Bool $ v1 || v2

infixOp env op (Var x) v2 = do
    var <- stateLookup env x
    case var of
        error@(Error _) -> return error
        val -> infixOp env op val v2

infixOp env op v1 (Var x) = do
    var <- stateLookup env x
    case var of
        error@(Error _) -> return error
        val -> infixOp env op v1 val


postfixOp env PostfixInc  (Int a) = return $ Int $ a + 1

--
-- Environment and auxiliary functions
--

environment :: Map String Value
environment = empty

stateLookup :: StateT -> String -> StateTransformer Value
stateLookup env var = ST $ \s ->
    (maybe
        (Error $ "Variable " ++ show var ++ " not defined")
        id
        (Map.lookup var (union s env)),
    s)

varDecl :: StateT -> VarDecl -> StateTransformer Value
varDecl env (VarDecl (Id id) maybeExpr) = do
    case maybeExpr of
        Nothing -> setVar id Nil
        (Just expr) -> do
            val <- evalExpr env expr
            setVar id val

setVar :: String -> Value -> StateTransformer Value
setVar var val = ST $ \s -> (val, insert var val s)

--
-- Types and boilerplate
--

type StateT = Map String Value
data StateTransformer t = ST (StateT -> (t, StateT))

instance Monad StateTransformer where
    return x = ST $ \s -> (x, s)
    (>>=) (ST m) f = ST $ \s ->
        let (v, newS) = m s
            (ST resF) = f v
        in resF newS

instance Functor StateTransformer where
    fmap = liftM

instance Applicative StateTransformer where
    pure = return
    (<*>) = ap

--
-- Main and results functions
--

showResult :: (Value, StateT) -> String
showResult (val, defs) = show val ++ "\n" ++ show (toList defs) ++ "\n"

getResult :: StateTransformer Value -> (Value, StateT)
getResult (ST f) = f empty

main :: IO ()
main = do
    js <- Parser.parseFromFile "Main.js"
    let statements = unJavaScript js
    putStrLn $ "AST: " ++ (show $ statements) ++ "\n"
    putStr $ showResult $ getResult $ evaluate environment statements
