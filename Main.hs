{-# OPTIONS -XFlexibleInstances #-}

module Main where

import Text.PrettyPrint
import Control.Monad.State
import Control.Monad.Error
import Data.List
import Data.Maybe (fromJust, isNothing, isJust)
import Debug.Trace 

import Data.Function (on)

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Graph 

import Util 
import AST

import Parser
import Type
import Shapify
import CodeGen 

import SemSyn

import System.IO
import System.Environment
import System.IO.Unsafe

data OptionAction 
    = NullaryAction (Config -> Config)
    | UnaryAction   (String -> Config -> Config)

interpretAction (NullaryAction f) xs c 
    = Just (xs, f c)
interpretAction (UnaryAction f) [] c 
    = Nothing
interpretAction (UnaryAction f) (x:xs) c 
    = Just (xs, f x c) 

      
data Option 
    = Option { optionString :: String, 
               optionLongString :: Maybe String,
               optionArgDescription :: Doc, 
               optionDescription :: Doc, 
               optionAction :: OptionAction }

instance Ppr Option where
    ppr (Option s ls argdesc desc _) =
        ppr s <> (case ls of 
                    Just ls -> comma <+> ppr ls
                    Nothing -> empty)
              <+> argdesc  $$
              nest 4 desc
    pprList opts =
        vcat $ (punctuate (text "\n") $ map ppr opts)
        

options = 
    [ Option "-f" (Just "--file") (text "FILENAME")
             (text "Specify program's input file")
             (UnaryAction (\x conf -> 
                               conf { inputFile = Just x })),
      Option "-s" (Just "--shapify") (empty)
             (text "Convert terms with type \"T a\" to \"T Unit\".")
             (NullaryAction (\conf -> conf {execMode = Shapify})),
      Option "-n" (Just "--natify") empty
             (text "Convert terms with \"List a\" to \"Nat\".")
             (NullaryAction (\conf -> conf {execMode = ShapifyPlus})),
      Option "-h"  (Just "--help") empty
             (text "Show this help message.")
             (NullaryAction (\conf -> conf {execMode = Help})),
      Option "-H"  (Just "--haskell-code") empty
             (text "(Obsolete) Return a Haskell source code of \"put\" function."
              $$ text "This options implies \"-n\".")
             (NullaryAction (\conf -> conf {outputMode = HaskellCode, execMode = ShapifyPlus})),
      Option "-P"  (Just "--pseudo-code") empty
             (text "(Obsolete) Return a pseudo code only after syntatic bidirectionalizatoin."
              $$ text "Note that \"wrapping\" code for semantic bidirectionalization is not produced.")
             (NullaryAction (\conf -> conf {outputMode = PseudoCode })),
      Option "-F"  (Just "--forward-only") empty
             (text"(Obsolete) Return a pseudo code without bidirecionalization.")
             (NullaryAction (\conf -> conf {outputMode = ForwardCode })), 
      Option "-U"  (Just "--without-type") empty 
             (text"Pseudo code without type. This option affects the output of \"-P\" and \"-F\".")
             (NullaryAction (\conf -> conf {isShowType = False})),
      Option "-T"  (Just "--with-type") empty 
             (text"Pseudo code with type. This option affects the output of \"-P\" and \"-F\".")
             (NullaryAction (\conf -> conf {isShowType = True})),
      Option "-no"  (Just "--no-bidrectionalization") empty
             (text"No Bidirectionalization (transformation stops after pre-processing)")
             (NullaryAction (\conf -> conf {b18nMode = NoB18n})),
      Option "-syn" (Just "--syntactic") empty 
             (text"Syntatic Bidirectionalization.")
             (NullaryAction (\conf -> conf {b18nMode = SyntacticB18n, outputMode = OM_NotSpecified  })),
      Option "-sem" (Just "--semantic") empty 
             (text"Semantic Bidirectionalization.")
             (NullaryAction (\conf -> conf {b18nMode = SemanticB18n, outputMode = OM_NotSpecified  })),
      Option "-comb" (Just "--combined") empty
             (text"Combined Bidirectionalization.")
             (NullaryAction (\conf -> conf {b18nMode = CombinedB18n, outputMode = OM_NotSpecified })),
      Option "-hs"   (Just "--haskell") empty
             (text"Output Haskell-runnable code.")
             (NullaryAction (\conf -> conf {isHaskellify = True}))
--       Option "-d" (Just "--debug-exec") empty
--              (text"Debug Execution (Do not use this option).")
--              (NullaryAction $ \conf -> conf {execMode = Debug})
    ]

      
matchOption optString options 
    = foldr f Nothing options 
    where f o r = 
              if (optionString o == optString) 
                 || (isJust (optionLongString o) 
                     && (fromJust (optionLongString o) == optString)) then 
                  Just o 
              else
                  r
           
parseArgs :: [[Char]] -> Config -> Config 
parseArgs args conf = 
    case args of 
      ("-d":xs) -> 
          parseArgs xs (conf { execMode = Debug })
      ("--debug":xs) -> 
          parseArgs xs (conf { execMode = Debug })
      (x:xs) -> case matchOption x options of 
                  Just o -> 
                      case  interpretAction (optionAction o) xs conf of 
                        Just (rest, c) -> 
                            parseArgs rest c 
                        Nothing ->
                            error "Error: #Argument of option mismatch." 
                  Nothing -> 
                      case x of 
                        '-':_ -> 
                            error $ "Error: Unknown option " ++ show x 
                        _ -> 
                            if isNothing (inputFile conf) then 
                                parseArgs xs (conf { inputFile = Just x })
                            else 
                                parseArgs xs conf
      []     -> conf



-- parseArgs :: [[Char]] -> Config -> Config 
-- parseArgs args conf =
--     case args of 
--       ("-f":x:xs) ->
--           parseArgs xs (conf { inputFile = Just x })
--       ("-s":xs) ->
--           parseArgs xs (conf { execMode = Shapify })
--       ("-ss":xs) ->
--           parseArgs xs (conf { execMode = ShapifyPlus })
--       ("-h":xs) ->
--           parseArgs xs (conf { execMode = Help })
--       ("-H":xs) ->
--           parseArgs xs (conf { outputMode = HaskellCode, execMode = ShapifyPlus } )
--       ("-P":xs) ->
--           parseArgs xs (conf { outputMode = PseudoCode } )
--       ("-d":xs) ->
--           parseArgs xs (conf { execMode = Debug })
--       (x:xs) | isNothing (inputFile conf) ->
--           parseArgs xs (conf { inputFile = Just x })
--       (x:xs) ->
--           parseArgs xs conf
--       [] ->
--           conf


progName = unsafePerformIO getProgName

usage = show $ 
    text "USAGE" $$
    text "-----" $$
         nest 4 (text $ progName ++ " (-n|-s) (-T|-U) (-no|-sem|-syn|-comb) [-hs] [-f] [FILENAME]\n") $+$ 
                  
         text ("This program is a prototype implementation of the paper:\n") $$
         nest 4 (sep [text "Janis Voigtlander, Zhenjiang Hu, Kazutaka Matsuda and Meng Wang:",
                       text "Combining Syntactic and Semantic Bidirectionalization.",
                       text "ICFP 2010.\n"])
         $$
         wrap 80 ( "Given a \"get\" function defined in a file specified by FILENAME, "
                  ++ "the program returns \"put\" function by combining "
                  ++ "semantic bidirectionalization (Janis Voiglander: POPL'09) "
                  ++ "and syntatic bidirectionalization (Kazutaka Matsuda et al.: ICFP'07). A typical usage is \""++ progName ++ " FILENAME\", which correspondes to the paper.\n"
                  ) $+$
    text "OPTIONS" $$
    text "-------" $$
         ppr options
    where
      pprOptions ps = vcat $ concatMap 
                      (\(a,b) -> [nest 4 a,nest 8 b]) ps 
      wrap n s = wrap' n s [] 
          where wrap' 0 (' ':s) buf = wrap' 0 s buf 
                wrap' 0 s buf  = (text (reverse buf)) $$ wrap' n s []
                wrap' m [] buf = (text (reverse buf))
                wrap' m (' ':s) buf  
                    | m - lnextSpace s < 0 =
                        text (reverse buf) $$ wrap' n s []
                    | otherwise = 
                        wrap' (m-1) s (' ':buf)
                wrap' m (c:s) buf | m > 0 =
                    wrap' (m-1) s (c:buf)
                lnextSpace [] = 0
                lnextSpace (' ':_) = 0
                lnextSpace (c:s)   = 1 + lnextSpace s 

main :: IO ()
main = do { args <- getArgs 
          ; let conf = adjustConfig $ parseArgs args defaultConfig
          ; case execMode conf of 
              Help -> putStrLn usage 
              _ -> 
                  do { csterr <- case inputFile conf of
                                   Nothing -> 
                                       do cont <- getContents
                                          return $ parseString cont
                                   Just filename ->
                                       parseFile filename
                     ; case csterr of
                         Left err -> hPutStrLn stderr (show err)
                         Right cprog ->  case typeInference cprog of
                            Left err -> hPutStrLn stderr err
                            Right typeChecked -> 
                             case execMode conf of 
--                                Normal | (b18nMode conf == SyntacticB18n || b18nMode conf == NoB18n) -> 
--                                    print $
--                                          outputCode conf False (cprog) (typeInference cprog)
-- --                                Shapify -> print $
-- --                                    outputCode conf False (cprog) (shapify $ typeInference cprog)
-- --                                    -- putStrLn "Not Supported Now."
--                                ShapifyPlus -> 
--                                    print $
--                                          outputCode conf True  (cprog) (introNat $ shapify $ typeInference cprog)
                               Debug ->
                                       putStrLn "Debug mode does nothing."
--                                    do { print $ ppr   $ cprog
--                                       -- ; print $ pprAM $ constructAutomaton (typeInference cprog) initTAMap
--                                       ; let (p1,p2,p3) = constructBwdFunction (typeInference cprog)
--                                       ; print $ ppr p1 $$ ppr p2 $$ ppr p3
--                                       ; print $ ppr $ constructTypeDecl p2 
--                                       ; print $ ppr $ generateCodeBwd (typeInference cprog, p1,p2,p3)
--                                       ; putStrLn ""
--                                       ; putStrLn $ "---- After \"Shapify\" ----" 
--                                       ; let cprog' = introNat $ shapify $ typeInference cprog 
--                                       -- ; print $ pprAM $ constructAutomaton cprog' initTAMap
--                                       ; print $ cprog'                                       
--                                       ; let (p1,p2,p3) = constructBwdFunction cprog' 
--                                       ; print $ ppr p1 $$ ppr p2 $$ ppr p3
--                                       ; putStrLn ""
--                                       }
                               _ | isNormalMode conf ->
                                     let transformed = typeChecked
                                     in checkAndDoBidirectionalize conf False cprog transformed
                               _ | isShapifyMode conf -> 
                                     let transformed = shapify $ typeChecked
                                     in checkAndDoBidirectionalize conf False cprog transformed
                               _ | isShapifyPlusMode conf || True -> 
                                     let transformed = introNat $ shapify $ typeChecked
                                     in checkAndDoBidirectionalize conf True cprog transformed 
                     }
          }
              where checkAndDoBidirectionalize conf isShapify orig ast =
                        if b18nMode conf == NoB18n || b18nMode conf == SemanticB18n then 
                            (print $ outputCode conf isShapify orig ast)
                        else
                            maybe (print $ outputCode conf isShapify orig ast)
                                  putStrLn 
                                  (checkBidirectionalizability ast)

