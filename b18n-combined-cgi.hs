{-# LANGUAGE RecordWildCards #-}
import Network.CGI
import Text.XHtml
import Data.Maybe
import Data.List
import Data.ByteString.Lazy.UTF8 (fromString)
import qualified Data.ByteString.Lazy as BS
import Control.Monad
import Control.Applicative ((<$>))
import Text.PrettyPrint.HughesPJ (render)
import System.IO
import System.IO.Error hiding ( catch )
import System.Directory
import Prelude hiding ( catch )
import Control.Exception
import System.Posix.Files ( isDirectory, getSymbolicLinkStatus )
import System.Posix.Env


import Parser
import SemSyn
import AST

import MyInterpret
import BundledCode
import JQuery

{-------------------------
 - Types (Logic/Presentation interface
 -------------------------}

data PageInfo = PageInfo
    { config :: Config
    , scrollX :: Maybe String
    , scrollY :: Maybe String
    , viewFunction :: String
    , astError :: Maybe String
    , generatedModuleMB :: Maybe String
    , showCode :: Bool
    , playCodeMB :: Maybe String
    , playErrorM :: Maybe String
    } 

data Run = Get | Check | Load | BiDi | EvalPut | EvalGet

{-------------------------
 - Default and example data
 -------------------------}

examples :: [(String, String)]
examples =
	[ ("init", unlines
		[ "init []      = []"
		, "init [a]     = []"
		, "init (a:b:x) = a:initWork b x"
		, ""
		, "initWork a []    = []"
		, "initWork a (b:x) = a:initWork b x"
		])
	, ("tail", unlines
		[ "tail []     = []"
		, "tail (x:xs) = xs"
		])
	, ("sieve", unlines
		[ "sieve []      = []"
		, "sieve [a]     = []"
		, "sieve (a:b:x) = b:sieve x"
		])
	, ("halve", unlines
		[ "halve []    = []"
		, "halve (a:x) = a:halveWork x x"
		, ""
		, "halveWork xs    []      = []"
		, "halveWork xs    [x]     = []"
		, "halveWork (a:x) (b:c:y) = a:halveWork x y"
		])
	, ("rev", unlines
		[ "reverse []     = []"
		, "reverse (x:xs) = rev xs [x]"
		, ""
		, "rev []    y = y"
		, "rev (a:x) y = rev x (a:y)"
		])
	]

defaultPlayCode :: Config -> String -> Maybe String
defaultPlayCode (Config{ b18nMode = SyntacticB18n}) get =
        Just $ unlines
            [ "get s = Main." ++ get ++ " s"
            , "put s v = " ++ get ++ "_B s v"
            , ""
            , "source = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10]"
            ]
defaultPlayCode (Config{ b18nMode = SemanticB18n}) get =
        Just $ unlines
            [ "get s = Main." ++ get ++ " s"
            , "put s v = " ++ get ++ "_B s v"
            , ""
            , "source = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10]"
            ]
defaultPlayCode (Config{ b18nMode = CombinedB18n}) get =
        Just $ unlines
            [ "get s = Main." ++ get ++ " s"
            , "put s v = fromMaybe (error \"Could not handle shape change.\") $ " ++
                 get ++ "_Bbd bias default_value s v"
            , "bias = rear         -- or another option, e.g., front, middle, borders"
            , "default_value = 42  -- or another value of the element type of source"
            , ""
            , "source = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10]"
            ]

defaultCode :: String
defaultCode = fromJust (lookup "init" examples)

{-------------------------
 - Program logic
 -------------------------}

-- This function will not work in all casses, but in most.
delDefinition :: String -> String -> String
delDefinition name code = unlines squashed
  where filtered = filter (not . defines name) (lines code)
	squash [] = []
	squash ("":_) = [""]
	squash ("\r":_) = [""]
	squash ls = ls
	squashed = concat $ map squash $ group $ filtered

addDefinition :: String -> String -> String -> String
addDefinition name def code = unlines (squashed ++ pad ++ new_line)
  where	squashed = lines (delDefinition name code)
	pad | last squashed == "" || last squashed == "\r" = []
            | otherwise                                    = [""]
	new_line = [name ++ " = " ++ def]
	
defines :: String -> String -> Bool
defines "" (' ':_) = True
defines "" ('=':_) = True
defines "" "" = False
defines "" _   = False
defines _  ""  = False
defines (i:is) (x:xs) | i == x    = defines is xs
                      | otherwise = False

formMain :: CGI CGIResult
formMain = do
        setHeader "Content-type" "text/html; charset=UTF-8"

        conf <- do
            b18nMode' <- maybe CombinedB18n read <$> getInput "b18nMode"
            return $ adjustConfig $ defaultConfig
                { isHaskellify = True
                , b18nMode = b18nMode'
                }
	
	todo <- msum <$> sequence (
            map (\what -> fmap (const what) <$> getInput (submitId what))
            [ BiDi, Get, Check, Load, EvalPut, EvalGet])
        
	code <- filter (/= '\r') <$> fromMaybe defaultCode <$> getInput "code"

        code <- case todo of
            Just Load -> do loadWhat <- getInput "loadCode"
                            return $ fromMaybe code $ loadWhat >>= flip lookup examples 
            _ -> return code
        
        let eAST = parseString code


        let astError = either (Just . show) checkBidirectionalizability eAST

        let (genCodeM,getM) = case (todo,eAST) of
                (Just Load, _) -> (Nothing, Nothing)
                (Just _, Right ast) ->
                    (  Just $ render $ renderCode conf ast
                    ,  firstDeclaredName ast
                    )
                _ -> (Nothing, Nothing)

        showCode <- maybe False read <$> getInput "showCode"

        pcM <- getInput "playCode" 
        (playCode, playErrorM) <-
            case (todo,getM,genCodeM,pcM) of
            -- The user successfully generated code to play with, insert default playCode.
            -- Do not use the user input, as he probably switched to a new example.
            (Just BiDi, Just get, Just _, _) ->
                return (defaultPlayCode conf get, Nothing)
            -- The user played with the code
            (Just EvalGet, Just get, Just genCode, Just pc) -> do
                view <- liftIO $ evaluateWith genCode pc ("get source")
                case view of 
                    Left err -> return $ (Just pc, Just err)
                    Right dat -> return $ (\r -> (Just r, Nothing))
                                        $ addDefinition "view" dat 
                                        $ delDefinition "result"
                                        $ pc
            (Just EvalGet, Just get, Just genCode, Nothing) -> do
                return (defaultPlayCode conf get, Nothing)
            (Just EvalPut, Just get, Just genCode, Just pc) -> do
                view <- liftIO $ evaluateWith genCode pc ("put source view")
                case view of 
                    Left err -> return $ (Just pc, Just err)
                    Right dat -> return $ (\r -> (Just r, Nothing))
                                        $ addDefinition "result" dat 
                                        $ pc
            (Just EvalPut, Just get, Just _, Nothing) -> do
                return (defaultPlayCode conf get, Nothing)
            _ -> return (Nothing, Nothing)

        scrollX <- getInput "scrollx"
        scrollY <- getInput "scrolly"

        outputFPS $ fromString $ showHtml $ page $
            PageInfo conf
                     scrollX
                     scrollY
                     code
                     astError
                     genCodeM
                     showCode
                     playCode
                     playErrorM

evaluateWith :: String -> String -> String -> IO (Either String String)
evaluateWith genCode playCode expr =
    withinTmpDir $ do
        BS.writeFile "BUtil.hs" bUtilCode
        writeFile "Main.hs" $ "module Main where\n" ++ genCode
        liftIO $ catchInterpreterErrors $ simpleInterpret mods imports playCode expr
  where mods = 
            [ "BUtil"
            , "Main"
            --, "Data.Maybe"
            ]
        imports = mods ++
            [ "Data.Maybe"
            , "Prelude"
            ]

{-------------------------
 - CGI Interface
 -------------------------}

main :: IO ()
main = runCGI (handleErrors cgiMain)

cgiMain :: CGI CGIResult
cgiMain = do
    qs <- queryString
    if qs == "jquery"
     then jQueryMain
     else formMain

jQueryMain :: CGI CGIResult
jQueryMain = do
        setHeader "Content-type" "text/javascript"
        setHeader "Expires" "Fri, 01 Jan 2100 00:00:00 +0100"
        setHeader "Cache-control" "max-age=36000000" -- 1000 h
        outputFPS $ jQueryCode
    

{-------------------------
 - HTML generation
 -------------------------}

submitId :: Run -> String
submitId Get = "get source"
submitId Check = "check"
submitId Load = "load"
submitId BiDi = "submitBiDi"
submitId EvalPut = "evalPut"
submitId EvalGet = "evalGet"

submitLabel :: Run -> String
submitLabel Check =   "Re-Parse definition"
submitLabel Load  =   "Load example"
submitLabel EvalGet = "view = get source"
submitLabel EvalPut = "result = put source view"
submitLabel BiDi =    "bidirectionalize"

b18nModeName :: B18nMode -> String
b18nModeName SemanticB18n = "Semantic bidir. (POPL’09)"
b18nModeName SyntacticB18n = "Syntactic bidir. (ICFP’07)"
b18nModeName CombinedB18n = "Combined bidir. (ICFP’10)"

mkSubmit :: Bool -> Run -> Html
mkSubmit active what = submit (submitId what) (submitLabel what)
     	               ! if active then [] else [disabled]

page :: PageInfo -> Html
page (PageInfo {..}) =
       header << (
	thetitle << "(Combining) Syntactic and Semantic Bidirectionalization" +++
	style ! [ thetype "text/css" ] << cdata cssStyle +++
	script ! [ thetype "text/javascript", src "?jquery" ] << noHtml +++
        script ! [ thetype "text/javascript" ] << cdata jsCode 
       ) +++
       body ! [ strAttr "onload" "restoreScroll()" ] << (
	thediv ! [theclass "top"] << (
		thespan ! [theclass "title"] << "(Combining) Syntactic and Semantic Bidirectionalization" +++
		thespan ! [theclass "subtitle"] << "Prototype implementation"
	) +++
	maindiv << (
        	p << "This tool allows you to experiment with the bidirectionalization methods described in the following papers: " +++
                ulist << (
                    li << (
                      "“" +++
		      hotlink "http://doi.acm.org/10.1145/1291151.1291162"
                        << "Bidirectionalization transformation based on automatic derivation of view complement functions" +++
		      "” (ICFP’07) by " +++
		      hotlink "http://www.kb.ecei.tohoku.ac.jp/~kztk/"
                        << "Kazutaka Matsuda" +++ ", " +++
                      "Zhenjiang Hu, " +++
                      "Keisuke Nakano, " +++
                      "Makoto Hamana, and " +++
                      "Masato Takeichi."
                    ) +++
                    li << (
                      "“" +++
		      hotlink "http://doi.acm.org/10.1145/1480881.1480904"
                        << "Bidirectionalization for free! (Pearl)" +++
		      "” (POPL’09) by " +++
		      hotlink "http://www.iai.uni-bonn.de/~jv/"
                        << "Janis Voigtländer"
                    ) +++
                    li << (
                      "“" +++
		      hotlink "http://www.iai.uni-bonn.de/~jv/icfp10.pdf"
                        << "Combining Syntactic and Semantic Bidirectionalization" +++
		      "” (ICFP’10) by " +++
                      "Janis Voigtländer" +++ ", " +++
                      "Zhenjiang Hu, " +++
                      "Kazutaka Matsuda" +++ ", and " +++
                      "Meng Wang"
                    )
		) +++
		p << (
		  "(For a stand-alone version on command line, which is also able to " +++
                  "show intermediate steps in the transformations, see " +++
		  hotlink "http://www.kb.ecei.tohoku.ac.jp/~kztk/b18n-combined/desc.html" << "here" +++ ". " +++
		  "For the technique from POPL’09 alone, a "+++
		  hotlink "http://www-ps.iai.uni-bonn.de/cgi-bin/bff.cgi"
                    << "separate web interface" +++
                  " offering more features is also available.)"
		)
	) +++
        form ! [method "post",
                action "#",
                strAttr "onsubmit" "saveScroll()"
            ] << (
                hidden "scrollx" (fromMaybe "0" scrollX) +++
                hidden "scrolly" (fromMaybe "0" scrollY) +++
                hidden "showCode" (show showCode) +++
		maindiv << (
			 p << (
				"Please enter the view function. The first function "+++
                                "defined will be assumed to be your view function. You "+++
                                "can use the first-order functional language from the "+++
                                "ICFP’07 paper, in Haskell syntax. The view function must "+++
                                "have type " +++ tt << "[a] -> [a]" +++ ". You can also load "+++
                                "some predefined examples."
			) +++

			p << (
				concatHtml (map (\(name,thisCode) -> 
					radio "loadCode" name
					! (if thisCode == viewFunction then [checked] else [])
					+++ name +++ " "
				) examples) +++
				mkSubmit True Load +++
				br +++
				textarea ! [name "code", cols "120", rows "7"] << viewFunction
			) 
			
		) +++
                ( case astError of 
                  Just err -> 
                     maindiv << p << (
                        "There was an error with the view function:" +++ br +++
                        pre << err +++ br +++
                        mkSubmit True Check +++
                        p << (
                            "The "+++
                            hotlink "http://www-ps.iai.uni-bonn.de/cgi-bin/bff.cgi"
                             << "purely semantic bidirectionalization technique" +++
                            " (POPL’09) may still be able to handle your view function. "+++
                            "For the combined technique (ICFP’10) it may be possible " +++
                            "to recover applicability by using some program transformation "+++
                            "techniques as discussed in Section 7 of the paper."
                        )
                     )
		  Nothing -> 
                    maindiv ! [ identifier "output" ]<< (
			p << ( "You can try all three bidirectionalization methods." ) +++
			p << (  concatHtml (map (\mode -> 
			          radio "b18nMode" (show mode) 
					! (guard (mode == b18nMode config) >> return checked)
					+++ b18nModeName mode +++ " "
                                ) [SyntacticB18n, SemanticB18n, CombinedB18n]) +++ " " +++
			       mkSubmit True BiDi
			) +++
                        ( htmlMB generatedModuleMB $ \ generatedModule -> 
                            p << ("Result Code" +++
                                thespan ! [ identifier "hideShow"
                                          , thestyle "display:none"] << (
                                    " (" +++ hotlink "javascript:" << "Hide/Show" +++ ")"
                                ) +++ ":" +++ br +++
                                pre ! [identifier "genCode" ] << generatedModule

                            )

                        )
                    ) +++
                    ( htmlMB playCodeMB $ \playCode -> maindiv << ( 
                        p << (  "You can now play with the code. You can modify the " +++
                                tt << "source" +++ " and calculate the " +++
                                tt << "view" +++ ", or modify the " +++
                                tt << "view" +++ " and calculate an updated "+++
                                tt << "source" +++ "." +++ br +++
                                textarea ! [name "playCode", cols "120", rows "12" ] << playCode
                        ) +++
                        p << ( "Evaluate " +++
                               mkSubmit True EvalGet +++ " " +++
                               mkSubmit True EvalPut
                        )
                    )) +++
                    ( htmlMB playErrorM $ \playError -> maindiv << ( 
                        p << (
                            strong << "An error occurred while evaluating your code:" +++ br +++
                            pre << playError
                            )
                    ))
		) 
	) +++
        maindiv << (
	    p << (
		"The source code of this application and the underlying library can be found " +++
		hotlink "http://hackage.haskell.org/package/bidirectionalization-combined" << "on hackage" +++
		". " +++
                "The code for the web interface is based on " +++
                hotlink "http://www-ps.iai.uni-bonn.de/cgi-bin/bff.cgi" << 
                    "the demo interface from “Bidirectionalization for free!”"
            ) +++
	    p << ("© 2010 Kazutaka Matsuda, Joachim Breitner <" +++
                hotlink "mailto:mail@joachim-breitner.de" << "mail@joachim-breitner.de" +++
	      ">")
	    )	
	)
       
cdata :: String -> Html
cdata s = primHtml $
    -- "<!--//--><![CDATA[//><!--\n" ++
    s
    -- ++"\n//--><!]]>"

maindiv :: Html -> Html
maindiv = thediv ! [theclass "main"]

{-------------------------
 - Static Web code
 -------------------------}

cssStyle :: String
cssStyle = unlines 
        [ "body { padding:0px; margin: 0px; }"
        , "div.top { margin:0px; padding:10px; margin-bottom:20px;"
        , "              background-color:#efefef;"
        , "              border-bottom:1px solid black; }"
        , "span.title { font-size:xx-large; font-weight:bold; }"
        , "span.subtitle { padding-left:30px; font-size:large; }"
        , "div.main { border:1px dotted black;"
        , "                   padding:10px; margin:10px; }"
        , "div.submain { padding:10px; margin:11px; }"
        , "p.subtitle { font-size:large; font-weight:bold; }"
        , "input.type { font-family:monospace; }"
        , "input[type=\"submit\"] { font-family:monospace; background-color:#efefef; }"
        , "span.mono { font-family:monospace; }"
        , "pre { margin:10px; margin-left:20px; padding:10px;"
        , "          border:1px solid black; }"
        , "textarea { margin:10px; margin-left:20px; padding:10px;  }"
        , "p { text-align:justify; }"
	]

jsCode :: String
jsCode = unlines 
    [ "function saveScroll () {"
    , "    $('#scrolly').val($('html').scrollTop());"
    , "}"
    , "function restoreScroll () {"
    , "    $('html').scrollTop($('#scrolly').val());"
    , "}"
    , "$(document).ready(function () {"
    , "   $('#hideShow').show();"
    , "   if ($('#showCode').val() == 'False')"
    , "     { $('#genCode').hide(); };"
    , "   $('#hideShow a').click(function () {"
    , "      $('#showCode').val("
    , "         $('#genCode').is(':visible') ? 'False' : 'True'"
    , "      );"
    , "      $('#genCode').toggle('slow');"
    , "   })"
    , "})"
    ]

{-------------------------
 - Utility functions
 -------------------------}

htmlMB :: Maybe t -> (t -> Html) -> Html
htmlMB Nothing  _ = noHtml
htmlMB (Just x) f = f x

firstDeclaredName :: AST -> Maybe String
firstDeclaredName (AST []) = Nothing
firstDeclaredName (AST (Decl n _ _ _:_)) = Just (show n)

{-
 - Temp-Dir functions taken from XMonad/Lock.hs and simplified.
 - It also changes TMP so that hint’s temporary files are stored within this directory
 -}
withinTmpDir :: IO a -> IO a
withinTmpDir job = do
  absolute_name <- (++ "/sem_syn.cgi") <$> getTemporaryDirectory
  formerdir <- getCurrentDirectory
  formerTMP <- getEnv "TMPDIR"
  bracket (do dir <- create_directory absolute_name 0
              setEnv "TMPDIR" dir True  
              return dir
          )
          (\dir -> do setCurrentDirectory formerdir
                      maybe (unsetEnv "TMPDIR") (\p -> setEnv "TMPDIR" p True) formerTMP
                      rmRecursive dir)
          (const job)
    where newname name 0 = name
          newname name n = name ++ "-" ++ show n
          create_directory :: FilePath -> Int -> IO FilePath
          create_directory name n
              = do createDirectory $ newname name n
                   setCurrentDirectory $ newname name n
                   getCurrentDirectory
                `catch` (\e -> if isAlreadyExistsError e
                               then create_directory name (n+1)
                               else throwIO e)

rmRecursive :: FilePath -> IO ()
rmRecursive d =
    do isd <- isDirectory <$> getSymbolicLinkStatus d
       if not isd
          then removeFile d
          else when isd $ do conts <- actual_dir_contents
                             withCurrentDirectory d $
                               mapM_ rmRecursive conts
                             removeDirectory d
    where actual_dir_contents = -- doesn't include . or ..
              do c <- getDirectoryContents d
                 return $ filter (/=".") $ filter (/="..") c

withCurrentDirectory :: FilePath -> IO r -> IO r
withCurrentDirectory name m =
    bracket
        (do cwd <- getCurrentDirectory
            when (name /= "") (setCurrentDirectory name)
            return cwd)
        (\oldwd -> setCurrentDirectory oldwd)
        (const m)

