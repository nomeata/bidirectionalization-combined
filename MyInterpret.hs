{-# LANGUAGE DeriveDataTypeable #-}

module MyInterpret
        ( simpleInterpret
	, simpleTypeOf
        , catchInterpreterErrors
        )
         where 

import Language.Haskell.Interpreter
import Language.Haskell.Interpreter.GHC.Unsafe

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
modules = [    "Data.Bff",
	       "SimpleTree",
               "Prelude",
               "Data.List"
--               "ShowQ",
--               "ShowFun",
--               "SimpleReflect",
--               "Data.Function",
        ]

data MyException = MyException String deriving (Typeable)

timeoutIO action = bracket
	(do mainId <- myThreadId
	    installHandler sigXCPU (CatchOnce $ throwDynTo mainId $ MyException "Time limit exceeded.") Nothing
	    forkIO $ do
		    threadDelay (5 * 1000 * 1000)
		    -- Time's up. It's a good day to die.
		    throwDynTo mainId (MyException "Time limit exceeded")

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
			 (throwTo mainId)      
		ret <- takeMVar mvar
		evaluate (length ret) -- make sure exceptions are handled here
		return ret
	)

-- myInterpreter :: String -> IO String
myInterpreter todo exp = timeoutIO $ do
        when (unsafe exp) $ throwDyn (MyException "Indicators for unsafe computations found in exp")

	eResult <- runInterpreter $ do
                setUseLanguageExtensions False
		-- Not available in hint-3.2?
                -- setOptimizations All

                reset
                -- no need for temporary files I hope
                setInstalledModsAreInScopeQualified True 
	
		unsafeSetGhcOption "-fno-monomorphism-restriction"
                
                setImports modules
                
		liftIO $ putStrLn exp
                todo exp
	
	case eResult of
		Left exception -> throw exception
		Right result -> return result
        
formatInterpreterError (UnknownError s) = "Unknown Interpreter Error " ++ s
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
        flip catchDyn (return . Left . formatInterpreterError) $
        flip catchDyn (\(MyException s) -> return (Left s))   $
	handleJust errorCalls (return . Left)                  $ -- bff in Bff.hs uses these
        Right `fmap` action

simpleInterpret :: String -> String -> IO String 
simpleInterpret defs what = 
	myInterpreter eval $
	   "let \n" ++
	    unlines (map (replicate 12 ' '++) (lines defs)) ++ 
            replicate 8 ' ' ++ "in " ++ what

simpleTypeOf :: String -> String -> IO String 
simpleTypeOf defs what = 
	myInterpreter typeOf $
	   "let \n" ++
	    unlines (map (replicate 12 ' '++) (lines defs)) ++ 
            replicate 8 ' ' ++ "in " ++ what
