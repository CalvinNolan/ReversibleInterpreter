{-# Language MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-} 

module Interpreter (runFileProgram, run) where

import Prelude hiding (lookup, print)

import qualified Data.Map as Map
import Data.Maybe

import qualified System.IO as System

import Control.Monad.Identity
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Text.Read hiding (get)

{-------------------------------------------------------------------}
{- The pure expression language                                    -}
{-------------------------------------------------------------------}

data Val = I Int | B Bool
           deriving (Eq, Show, Read)

data Expr = Const Val
     | Add Expr Expr | Sub Expr Expr  | Mul Expr Expr | Div Expr Expr
     | And Expr Expr | Or Expr Expr   | Not Expr 
     | Eq Expr Expr  | Gt Expr Expr   | Lt Expr Expr
     | Var String
     deriving (Eq, Show, Read)

type Name = String 
type Env = Map.Map Name [Val]

lookup k t = case Map.lookup k t of
               Just (x:_) -> return x
               Nothing -> fail ("Unknown variable "++k)

{-- Monadic style expression evaluator, 
 -- with error handling and Reader monad instance to carry dictionary
 --}

type Eval a = ReaderT Env (ExceptT String Identity) a 
runEval env ex = runIdentity ( runExceptT ( runReaderT ex env) )

evali op e0 e1 = do e0' <- eval e0
                    e1' <- eval e1
                    case (e0', e1') of
                         (I i0, I i1) -> return $ I (i0 `op` i1)
                         _            -> fail "type error in arithmetic expression"

evalb op e0 e1 = do e0' <- eval e0
                    e1' <- eval e1
                    case (e0', e1') of
                         (B i0, B i1) -> return $ B (i0 `op` i1)
                         _            -> fail "type error in boolean expression"

evalib op e0 e1 = do e0' <- eval e0
                     e1' <- eval e1
                     case (e0', e1') of
                          (I i0, I i1) -> return $ B (i0 `op` i1)
                          _            -> fail "type error in arithmetic expression"

eval :: Expr -> Eval Val
eval (Const v) = return v
eval (Add e0 e1) = do evali (+) e0 e1
eval (Sub e0 e1) = do evali (-) e0 e1
eval (Mul e0 e1) = do evali (*) e0 e1
eval (Div e0 e1) = do evali div e0 e1

eval (And e0 e1) = do evalb (&&) e0 e1
eval (Or e0 e1) = do evalb (||) e0 e1

eval (Not e0  ) = do evalb (const not) e0 (Const (B True)) 
                       where not2 a _ = not a -- hack, hack

eval (Eq e0 e1) = do evalib (==) e0 e1
eval (Gt e0 e1) = do evalib (>) e0 e1
eval (Lt e0 e1) = do evalib (<) e0 e1
                        
eval (Var s) = do env <- ask
                  lookup s env


{-------------------------------------------------------------------}
{- The statement language                                          -}


data Statement = Assign String Expr
               | If Expr Statement Statement
               | While Expr Statement
               | Print Expr
               | Seq Statement Statement
               | Try Statement Statement
               | Pass                    
      deriving (Eq, Show, Read)

type Run a = StateT Env (ExceptT String IO) a 
runRun p =  runExceptT ( runStateT p Map.empty) 

set :: (Name, Val) -> Run ()
set (s,i) =  state $ (\table -> let oldValue = case Map.lookup s table of
                                                Just x -> x
                                                Nothing -> []
                                in ((), Map.insert s (i:oldValue) table))

{- Removes the oldest appended value to a variable in the state. -}
reverseSet :: Name -> Run ()
reverseSet s = state $ (\table -> let oldValue = case Map.lookup s table of
                                                              Just (x:xs) -> xs
                                                              Just [] -> []
                                                              Nothing -> []
                                  in ((), Map.insert s oldValue table))

{- Changes the flow of the statements to the previously executed statement -}
reverseStatements :: Statement -> [Statement] -> Run()
reverseStatements s [] = handleExec s []
reverseStatements s (x:xs) = do liftIO (System.putStrLn "") >> liftIO (System.print ("xs: " ++ show (s) ++ " > " ++ show(xs) ++ " > " ++ show(x))) >> liftIO (System.putStrLn "")
                                case s of
                                  (Print _) -> case x of
                                                (Seq (Assign d v) s2) -> reverseSet d >> handleExec (Seq (Assign d v) s2) xs
                                                _ -> handleExec x xs
                                  _ -> case xs of
                                        ((Seq (Assign s v) s2):xss) -> reverseSet s >> handleExec (Seq (Assign s v) s2) xss
                                        (xss:xsss) -> handleExec xss xsss
                                        [] -> handleExec s (x:xs)
                

{- Here we handle any user promtps. -}
userPrompt :: Statement -> [Statement] -> Run()
userPrompt s prevStatements = do x <- liftIO(System.getChar)
                                 st <- get
                                 case x of
                                  '.' -> liftIO (System.putStrLn "") >> exec s prevStatements
                                  ',' -> liftIO (System.putStrLn "") >> liftIO (System.print ("Variable Inspection: " ++ (show st))) >> userPrompt s prevStatements
                                  '/' -> liftIO (System.putStrLn "") >> liftIO (System.print ("<< Reversing <<")) >> reverseStatements s prevStatements
                                  _ -> userPrompt s prevStatements

{- handleExec will print the next line and wait for a user prompt before executing it -}
handleExec :: Statement -> [Statement] -> Run()
handleExec (While cond s) prevStatements = do liftIO (System.print ("Next Line: While " ++ (show cond)))
                                              userPrompt (While cond s) prevStatements

handleExec (Print e) prevStatements = do liftIO (System.print ("Next Line: Print " ++ (show e)))
                                         userPrompt (Print e) prevStatements

handleExec (Assign s v) prevStatements = do liftIO (System.print ("Next Line: Assign " ++ (show s) ++ " " ++ (show v)))
                                            userPrompt (Assign s v) prevStatements

handleExec (If cond s0 s1) prevStatements = do liftIO (System.print ("Next Line: If " ++ (show cond)))
                                               userPrompt (If cond s0 s1) prevStatements

handleExec (Try s0 s1) prevStatements = do liftIO (System.print ("Next Line: Try " ++ (show s0)))
                                           userPrompt (Try s0 s1) prevStatements

handleExec s prevStatements = exec s prevStatements

exec :: Statement -> [Statement] -> Run ()
exec (Assign s v) _ = do st <- get
                         Right val <- return $ runEval st (eval v)
                         set (s,val)

exec (Seq s0 s1) prevStatements = do handleExec s0 ((Seq s0 s1):prevStatements) >> handleExec s1 ((Seq s0 s1):prevStatements)

exec (Print e) _ = do st <- get
                      Right val <- return $ runEval st (eval e) 
                      liftIO $ System.print val
                      return () 

exec (If cond s0 s1) prevStatements = do st <- get
                                         Right (B val) <- return $ runEval st (eval cond)
                                         if val then do exec s0 ((If cond s0 s1):prevStatements) else do exec s1 ((If cond s0 s1):prevStatements)

exec (While cond s) prevStatements = do st <- get
                                        Right (B val) <- return $ runEval st (eval cond)
                                        if val then do exec s ((While cond s):prevStatements) >> exec (While cond s) ((While cond s):prevStatements) else return ()

exec (Try s0 s1) prevStatements = do catchError (exec s0 ((Try s0 s1):prevStatements)) (\e -> exec s1 ((Try s0 s1):prevStatements))
                        
exec Pass _ = return ()

{- Pour some sugar over this -}
{- This next section deals exclusively with defining convenience functions -}
{- which we will use when writing the actual programs in the DSL. -}

int = Const . I
bool = Const . B
var = Var

class SmartAssignment a where
  assign :: String -> a -> Statement

instance SmartAssignment Int where
  assign v i = Assign v (Const (I i))

instance SmartAssignment Bool where
  assign v b = Assign v (Const (B b))

instance SmartAssignment Expr where
  assign v e = Assign v e

class PrettyExpr a b where
  (.*) :: a -> b -> Expr
  (.-) :: a -> b -> Expr


instance PrettyExpr String String where
  x .* y = (Var x) `Mul` (Var y)
  x .- y = (Var x) `Sub` (Var y)

instance PrettyExpr String Int where
  x .* y = (Var x) `Mul` (Const (I y))
  x .- y = (Var x) `Sub` (Const (I y))

type Program = Writer Statement ()

instance Monoid Statement where
  mempty = Pass
  mappend a b = a `Seq` b

compile :: Program -> Statement
compile p = snd . runIdentity $ runWriterT p

run :: Program -> IO ()
run program = do result <- runExceptT $ (runStateT $ (exec (snd $ runIdentity $ (runWriterT program)) [])) Map.empty
                 case result of
                      Right ( (), env ) -> return ()                     
                      Left exn -> System.print ("Uncaught exception: "++exn)

infixl 1 .=
(.=) :: String -> Expr -> Program 
var .= val = tell $ assign var val

iif :: Expr -> Program -> Program -> Program
iif cond tthen eelse = tell $ If cond (compile tthen) (compile eelse)

while :: Expr -> Program -> Program
while cond body = tell $ While cond (compile body)

print :: Expr -> Program
print e = tell $ Print e

try :: Program -> Program -> Program
try block recover = tell $ Try (compile block) (compile recover)

runFileProgram :: String -> IO ()
runFileProgram path = do
                  x <- liftIO(System.readFile path)
                  case readMaybe x of
                    Just y -> run y
                    Nothing -> liftIO(System.print "Error parsing from file")
