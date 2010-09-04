{-# LANGUAGE DeriveDataTypeable #-}

module MyInterpret
        ( simpleInterpret
	, simpleTypeOf
        , catchInterpreterErrors
        )
         where 

import Language.Haskell.Interpreter
import Language.Haskell.Interpreter.Unsafe

import Prelude hiding (catch)
import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.Error
import System.Posix.Signals
import Data.Typeable (Typeable)
import Data.List
import Data.Either


-- Scoped modules
defaultModules = [    "Prelude",
               "Data.List"
        ]

data MyException = MyException String deriving (Show,Typeable)
instance Exception MyException

timeoutIO action = bracket
	(do mainId <- myThreadId
	    installHandler sigXCPU (CatchOnce $ throwTo mainId $ MyException "Time limit exceeded.") Nothing
	    forkIO $ do
		    threadDelay (5 * 1000 * 1000)
		    -- Time's up. It's a good day to die.
		    throwTo mainId (MyException "Time limit exceeded")

		    {- why do we need that here?
 - 		    yield -- give the other thread a chance
		    putStrLn "Killing main thread..."
		    killThread mainId -- Die now, srsly.
		    error "Time expired"
		    -}
	)

	(killThread)
	
	(\_ -> do
	        mainId <- myThreadId
        	mvar <- newEmptyMVar
		forkIO $ (action >>= putMVar mvar) `catch`
			 (\e -> throwTo mainId (e::SomeException))
		ret <- takeMVar mvar
		evaluate (length ret) -- make sure exceptions are handled here
		return ret
	)

myInterpreter :: [String] -> [String] -> (String -> InterpreterT IO [a]) -> String -> IO [a]
myInterpreter mods imports todo exp = timeoutIO $ do
        when (unsafe exp) $ throw (MyException "Indicators for unsafe computations found in exp")

	eResult <- runInterpreter $ do
                set [languageExtensions := []]
		-- Not available in hint-3.2?
                -- setOptimizations All

                reset

                if null mods
                  then do -- no need for temporary files I hope. Used by bff
                       set [installedModulesInScope := True ]
                       setImports defaultModules
                  else do -- Only these modules in scope. No Prelude either!
                       loadModules mods
                       set [installedModulesInScope := False ]
                       setImports imports
	
		unsafeSetGhcOption "-fno-monomorphism-restriction"
                unsafeSetGhcOption "-fno-warn-warnings-deprecations"
                
		liftIO $ putStrLn exp
                ret <- todo exp

                -- Hopefully removes temporary files
                reset
                
                return ret
	
	case eResult of
		Left exception -> throw exception
		Right result -> return result
        
formatInterpreterError :: InterpreterError -> [Char]
formatInterpreterError (UnknownError s) = "Unknown Interpreter Error:\n" ++ s
formatInterpreterError (WontCompile es) = "Could not compile code:\n" ++ unlines (map errMsg es)
formatInterpreterError (NotAllowed s) = "Not allowed here " ++ s
formatInterpreterError (GhcException e) = "GHC Exception"
        
{- | Return true if the String contains anywhere in it any keywords associated
   with dangerous functions. Unfortunately, this blacklist leaks like a sieve
   and will return many false positives (eg. 'unsafed "id \"unsafed\""' will
   evaluate to True, even though the phrase \"unsafe\" appears only in a String). But it
   will at least catch naive and simplistic invocations of "unsafePerformIO",
   "inlinePerformIO", and "unsafeCoerce". -}
unsafe :: String -> Bool
unsafe = \z -> any (`isInfixOf` z) unsafeNames

unsafeNames :: [String]
unsafeNames = ["unsafe", "inlinePerform", "liftIO", "Coerce", "Foreign",
               "Typeable", "Array", "IOBase", "Handle", "ByteString",
               "Editline", "GLUT", "lock", "ObjectIO", "System.Time",
               "OpenGL", "Control.Concurrent", "System.Posix",
               "throw", "Dyn", "cache", "stdin", "stdout", "stderr"]

catchInterpreterErrors :: IO a -> IO (Either String a)
catchInterpreterErrors action = 
        flip catch (return . Left . formatInterpreterError) $
        flip catch (\(MyException s) -> return (Left s))   $
        flip catch (\(ErrorCall s) -> return (Left s))   $
        Right `fmap` action

simpleInterpret :: [String] -> [String] -> String -> String -> IO String 
simpleInterpret mods imports defs what = 
	myInterpreter mods imports eval $
	   "let \n" ++
	    unlines (map (replicate 12 ' '++) (lines defs)) ++ 
            replicate 8 ' ' ++ "in " ++ what

simpleTypeOf :: [String] -> [String] -> String -> String -> IO String 
simpleTypeOf mods imports defs what = 
	myInterpreter mods imports typeOf $
	   "let \n" ++
	    unlines (map (replicate 12 ' '++) (lines defs)) ++ 
            replicate 8 ' ' ++ "in " ++ what
