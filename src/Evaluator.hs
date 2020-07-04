{-# LANGUAGE ExistentialQuantification #-}

module Evaluator where


import           Control.Monad                 (liftM)
import           Control.Monad.Except
import           Data.IORef
import           Parser
import           System.IO
import           Text.ParserCombinators.Parsec (ParseError, parse)


-- Types

data LispError = NumArgs Integer [LispVal]
    | TypeMismatch String LispVal
    | Parser ParseError
    | BadSpecialForm String LispVal
    | NotFunction String String
    | UnboundVar String String
    | Default String


type ThrowsError = Either LispError


data Unpacker = forall a . Eq a => AnyUnpacker (LispVal -> ThrowsError a)

-- Error Handling Functions


trapError action = catchError action (return . show)


extractValue :: ThrowsError a -> a
extractValue (Right val) = val


-- Evaluator Primitives

eval :: LispVal -> ThrowsError LispVal
eval val@(String _)             = return val
eval val@(Number _)             = return val
eval val@(Bool _)               = return val
eval val@(Character _)          = return val
eval val@(Float _)              = return val
eval val@(Ratio _)              = return val
eval val@(Complex _)            = return val
eval (List [Atom "quote", val]) = return val
eval (List [Atom "if", pred, conseq, alt]) = -- if statement
    do
        result <- eval pred
        case result of
            Bool True  -> eval conseq
            Bool False -> eval alt
            notBool    -> throwError $ TypeMismatch "boolean" notBool
eval (List ((Atom "cond") : alts)) = cond alts
eval form@(List (Atom "case" : key : clauses)) = -- case statement
    if null clauses
        then throwError $ BadSpecialForm "no true clause in case expression: " form
        else case head clauses of
            List (Atom "else" : exprs) -> mapM eval exprs >>= return . last
            List ((List datums) : exprs) -> do
                result <- eval key
                equality <- mapM (\x -> eqv [result, x]) datums
                if Bool True `elem` equality
                    then mapM eval exprs >>= return . last
                    else eval $ List (Atom "case" : key : tail clauses)
            _ -> throwError $ BadSpecialForm "ill-formed case expression: " form
eval (List (Atom func : args))  = mapM eval args >>= apply func -- function application
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm


apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function args" func) ($ args) $ lookup func primitives


primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives =
    [ ("+", numericBinop (+))
    , ("-", numericBinop (-))
    , ("*", numericBinop (*))
    , ("/", numericBinop div)
    , ("mod", numericBinop mod)
    , ("quotient", numericBinop quot)
    , ("remainder", numericBinop rem)
    , ("symbol?", unaryOp symbolp)
    , ("string?", unaryOp stringp)
    , ("number?", unaryOp numberp)
    , ("bool?", unaryOp boolp)
    , ("list?", unaryOp listp)
    , ("symbol->string", unaryOp symbol2string)
    , ("string->symbol", unaryOp string2symbol)
    , ("=", numBoolBinop (==))
    , ("<", numBoolBinop (<))
    , (">", numBoolBinop (>))
    , ("/=", numBoolBinop (/=))
    , (">=", numBoolBinop (>=))
    , ("<=", numBoolBinop (<=))
    , ("&&", boolBoolBinop (&&))
    , ("||", boolBoolBinop (||))
    , ("string=?", strBoolBinop (==))
    , ("string<?", strBoolBinop (<))
    , ("string>?", strBoolBinop (>))
    , ("string<=?", strBoolBinop (<=))
    , ("string>=?", strBoolBinop (>=))
    , ("car", car)
    , ("cdr", cdr)
    , ("cons", cons)
    , ("eq?", eqv)
    , ("eqv?", eqv)
    , ("equal?", equal)
    , ("string-length", stringLen)
    , ("string-ref", stringRef)
    ]


numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
-- numericBinop op params = Number $ foldl1 op $ map unpackNum params
numericBinop op [] = throwError $ NumArgs 2 []
numericBinop op single@[_] = throwError $ NumArgs 2 single
numericBinop op params = mapM unpackNum params >>= return . Number . foldl1 op


unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) =
    let parsed = reads n
    in
        if null parsed
            then throwError $ TypeMismatch "number" $ String n
            else return $ fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum notNum    = throwError $ TypeMismatch "number" notNum


unaryOp :: (LispVal -> ThrowsError LispVal) -> [LispVal] -> ThrowsError LispVal
unaryOp f [v] = f v -- error to be given more than 2 args


symbolp, stringp, numberp, boolp, listp :: LispVal -> ThrowsError LispVal
symbolp (Atom _) = return $ Bool True
symbolp _        = return $ Bool False
stringp (String _) = return $ Bool True
stringp _          = return $ Bool False
numberp (Number _) = return $ Bool True
numberp _          = return $ Bool False
boolp (Bool _) = return $ Bool True
boolp _        = return $ Bool False
listp (List _) = return $ Bool True
listp _        = return $ Bool False


symbol2string, string2symbol :: LispVal -> ThrowsError LispVal
symbol2string (Atom s) = return $ String s
symbol2string _        = return $ String ""
string2symbol (String s) = return $ Atom s
string2symbol _          = return $ Atom ""


numBoolBinop = boolBinop unpackNum
strBoolBinop = boolBinop unpackStr
boolBoolBinop = boolBinop unpackBool


boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args =
    if length args /= 2
        then throwError $ NumArgs 2 args
        else do
                left <- unpacker $ args !! 0
                right <- unpacker $ args !! 1
                return $ Bool $ left `op` right


unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool s)   = return $ show s
unpackStr notString  = throwError $ TypeMismatch "string" notString


unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool  = throwError $ TypeMismatch "boolean" notBool


-- List Primitives

car :: [LispVal] -> ThrowsError LispVal
car [List (x:xs)]         = return x
car [DottedList (x:xs) _] = return x
car [badArg]              = throwError $ TypeMismatch "pair" badArg
car badArgList            = throwError $ NumArgs 1 badArgList


cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (x:xs)]           = return $ List xs
cdr [DottedList [_] x]      = return x
cdr [DottedList (_ : xs) x] = return $ DottedList xs x
cdr [badArg]                = throwError $ TypeMismatch "pair" badArg
cdr badArgList              = throwError $ NumArgs 1 badArgList


cons :: [LispVal] -> ThrowsError LispVal
cons [x1, List []]            = return $ List [x1]
cons [x, List xs]             = return $ List $ x : xs
cons [x, DottedList xs xlast] = return $ DottedList (x : xs) xlast
cons [x1, x2]                 = return $ DottedList [x1] x2
cons badArgList               = throwError $ NumArgs 2 badArgList


-- Equivalence Predicates

eqv :: [LispVal] -> ThrowsError LispVal
eqv [(Bool arg1), (Bool arg2)] = return $ Bool $ arg1 == arg2
eqv [(Number arg1), (Number arg2)] = return $ Bool $ arg1 == arg2
eqv [(String arg1), (String arg2)] = return $ Bool $ arg1 == arg2
eqv [(Atom arg1), (Atom arg2)] = return $ Bool $ arg1 == arg2
eqv [(DottedList xs x), (DottedList ys y)] = eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv listPair@[(List _), (List _)] = eqvList eqv listPair
eqv [_, _] = return $ Bool False
eqv badArgList = throwError $ NumArgs 2 badArgList


equal :: [LispVal] -> ThrowsError LispVal
equal listPair@[(List _), (List _)] = eqvList equal listPair
equal [(DottedList xs x), (DottedList ys y)] = equal [List $ xs ++ [x], List $ ys ++ [y]]
equal [arg1, arg2] = do
        primitiveEquals <- liftM or $ mapM (unpackEquals arg1 arg2) [AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool]
        eqvEquals <- eqv [arg1, arg2]
        return $ Bool $ (primitiveEquals || let (Bool x) = eqvEquals in x)
equal badArgList = throwError $ NumArgs 2 badArgList


unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) =
    do
        unpacked1 <- unpacker arg1
        unpacked2 <- unpacker  arg2
        return $ unpacked1 == unpacked2
    `catchError` (const $ return False)


eqvList :: ([LispVal] -> ThrowsError LispVal) -> [LispVal] -> ThrowsError LispVal
eqvList eqvFunc [(List arg1), (List arg2)] = return $ Bool $ (length arg1 == length arg2) && (all eqvPair $ zip arg1 arg2)
    where
        eqvPair (x1, x2) = case eqvFunc [x1, x2] of
            Left err         -> False
            Right (Bool val) -> val


-- Conditional Function

cond :: [LispVal] -> ThrowsError LispVal
cond ((List (Atom "else" : value : [])) : [])  = eval value
cond ((List (condition : value : [])) : alts) = do
        result <- eval condition
        boolResult <- unpackBool result
        if boolResult
            then eval value
            else cond alts
cond ((List a) : _) = throwError $ NumArgs 2 a
cond (a : _) = throwError $ NumArgs 2 [a]
cond _ = throwError $ Default "Not viable alternative in cond"


-- String Functions

stringLen :: [LispVal] -> ThrowsError LispVal
stringLen [(String s)] = Right $ Number $ fromIntegral $ length s
stringLen [notString]  = throwError $ TypeMismatch "string" notString
stringLen badArgList   = throwError $ NumArgs 1 badArgList


stringRef :: [LispVal] -> ThrowsError LispVal
stringRef [(String s), (Number k)]
    | length s < k' + 1 = throwError $ Default "Out of bound error"
    | otherwise = Right $ String $ [s !! k']
    where k' = fromIntegral k
stringRef [(String s), notNum] = throwError $ TypeMismatch "number" notNum
stringRef [notString, _] = throwError $ TypeMismatch "string" notString
stringRef badArgList = throwError $ NumArgs 2 badArgList


-- Scheme Execution

runRepl :: IO ()
runRepl = until_ (== "quit") (readPrompt "uS> ") evalAndPrint


readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
    Left err  -> throwError $ Parser err
    Right val -> return val


-- IO Tasks

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout


readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine


evalString :: String -> IO String
evalString expr = return $ extractValue $ trapError (liftM show $ readExpr expr >>= eval)


evalAndPrint :: String -> IO ()
evalAndPrint expr = evalString expr >>= putStrLn


until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
        result <- prompt
        if pred result
            then return ()
            else action result >> until_ pred prompt action


-- Instances

instance Show LispVal where
    show (String contents) = "\"" ++ contents ++ "\""
    show (Atom name) = name
    show (Number contents) = show contents
    show (Character char) = [char]
    show (Float double) = show double
    show (Ratio rational) = show rational
    show (Complex comp) = show comp
    show (Bool True) = "#t"
    show (Bool False) = "#f"
    show (List contents) = "(" ++ (unwords . map show) contents ++ ")"
    show (DottedList head tail) = "(" ++ (unwords . map show) head ++ " . " ++ show tail ++ ")"
    show (Vector vec) = show vec


instance Show LispError where
    show (UnboundVar message varname)  = message ++ ": " ++ varname
    show (BadSpecialForm message form) = message ++ ": " ++ show form
    show (NotFunction message func)    = message ++ ": " ++ show func
    show (NumArgs expected found)     = "Expected " ++ show expected ++ " args; found values " ++ (unwords . map show) found
    show (TypeMismatch expected found) = "Invalid type: expected " ++ expected ++ ", found " ++ show found
    show (Parser parseErr) = "Parse error at " ++ show parseErr
