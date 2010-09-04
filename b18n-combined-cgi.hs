{-# LANGUAGE RecordWildCards #-}
import Network.CGI
import Text.XHtml
import Data.Maybe
import Data.List
import Data.ByteString.Lazy.UTF8 (fromString)
import qualified Data.ByteString.Lazy as BS
import Control.Monad
import Control.Applicative ((<$>),(<*>))
import Text.PrettyPrint.HughesPJ (render)
import System.IO
import System.IO.Error hiding ( catch )
import Text.ParserCombinators.Parsec (ParseError)
import System.Directory
import Prelude hiding ( catch )
import Control.Exception
import System.Posix.Files ( isDirectory, getSymbolicLinkStatus )


import Parser
import SemSyn
import Type
import Shapify
import AST

import MyInterpret
import BundledCode
import JQuery

data PageInfo = PageInfo
    { scrollX :: Maybe String
    , scrollY :: Maybe String
    , viewFunction :: String
    , parseError :: Maybe String
    , exMode  :: ExecMode
    , outMode :: OutputMode
    , showTypes :: Bool
    , generatedModuleMB :: Maybe String
    , playCodeMB :: Maybe String
    , playErrorM :: Maybe String
    } 

page (PageInfo {..}) =
       header << (
	thetitle << "Combining Syntatic and Semantic Bidirectionalization" +++
	style ! [ thetype "text/css" ] << cdata cssStyle +++
	script ! [ thetype "text/javascript", src "?jquery" ] << noHtml +++
        script ! [ thetype "text/javascript" ] << cdata jsCode 
       ) +++
       body ! [ strAttr "onload" "restoreScroll()" ] << (
	thediv ! [theclass "top"] << (
		thespan ! [theclass "title"] << "Combining Syntatic and Semantic Bidirectionalization" +++
		thespan ! [theclass "subtitle"] << "Prototype implementation"
	) +++
	maindiv << (
        	p << ("This tool allows you to experiment with the "+++
                      "method described in the paper “" +++
		      hotlink "http://doi.acm.org/10.1145/1291151.1291162"
                        << "Bidirectionalization transformation based on automatic derivation of view complement functions" +++
		      "” (ICFP'10) by " +++
		      hotlink "http://www.kb.ecei.tohoku.ac.jp/~kztk/"
                        << "Kazutaka Matsuda" +++
	              "."
		)
			
	) +++
        form ! [method "POST",
                action "#",
                strAttr "onsubmit" "saveScroll()"
            ] << (
                hidden "scrollx" (fromMaybe "0" scrollX) +++
                hidden "scrolly" (fromMaybe "0" scrollY) +++
		maindiv << (
			 p << (
				"Please enter the view function. (TODO: Elaborate this text)"
			) +++

			p << (
				concatHtml (map (\(name,thisCode) -> 
					radio "load" name
					! (if thisCode == viewFunction then [checked] else [])
					+++ name +++ " "
				) examples) +++
				mkSubmit True Load +++
				br +++
				textarea ! [name "code", cols "120", rows "7"] << viewFunction
			) 
			
		) +++
                ( htmlMB parseError $ \err -> 
                     maindiv << p << (
                        "Can not parse your definition:" +++ br +++
                        pre << show err +++ br +++
                        mkSubmit True Check)
                ) +++
		-- p << astInfo mbAST +++
		maindiv ! [ identifier "output" ]<< (
			p << (
				"You can calculate a derived put function with various options:" ) +++
			p << ( "Execution mode: " +++
			       concatHtml (map (\mode -> 
			          radio "execMode" (show mode) 
					! (guard (mode == exMode) >> return checked)
					+++ show mode +++ " "
                                ) [Normal, Shapify, ShapifyPlus]) +++ br +++
			       "Output mode: " +++
			       concatHtml (map (\mode -> 
			          radio "outputMode" (show mode) 
					! (guard (mode == outMode) >> return checked)
					+++ show mode +++ " "
                                ) [PseudoCode, HaskellCode, ForwardCode]) +++ br +++
			       "Show types " +++ checkbox "showTypes" "showTypes"
                                        ! (guard showTypes >> return checked)
                                        +++ br +++
			       mkSubmit True BiDi
			) +++
                        ( htmlMB generatedModuleMB $ \ generatedModule -> 
                            {- maybe noHtml outputErrors errors +++ -}
                            p << ("Result:"+++ br +++
                                textarea ! [name "gencode", cols "120"
                                           , rows (show (1 + length (lines generatedModule)))
                                           , readOnly
                                           ] << generatedModule

                            )

                        )
		) +++
                ( htmlMB playCodeMB $ \playCode -> maindiv << ( 
                    p << (  "You can now play with the code. You can modify the " +++
                            tt << "source" +++ " and calculate the view, or modify the " +++
                            tt << "view" +++ " and calculate an updated souce." +++ br +++
                            textarea ! [name "playCode", cols "120", rows "8" ] << playCode
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
	) +++
        maindiv << (
		p << (
		"The source code of this application and the underlying library can be found " +++
		hotlink "TODO" << "here"+++
		".") +++
		p << ("© 2010 Joachim Breitner <" +++
                      hotlink "mailto:mail@joachim-breitner.de" << "mail@joachim-breitner.de" +++
		      ">")
		)	
	)
       

cdata s = primHtml ("<![CDATA[\n"++ s ++ "\n]]>")

maindiv = thediv ! [theclass "main"]
        
examples =
	[ ("init", unlines
		[ "init (Nil)         = Nil"
		, "init (Cons(a,Nil)) = Nil"
		, "init (Cons(a,Cons(b,x))) = Cons(a,initWork(b,x))"
		, "initWork(a,Nil)       = Nil"
		, "initWork(a,Cons(b,x)) = Cons(a,initWork(b,x))"
		])
	, ("initHalf", unlines
		[ "initHalf(Nil)       = Nil"
		, "initHalf(Cons(a,x)) = Cons(a,initHalfWork(x,x))"
		, ""
		, "initHalfWork(xs, Nil)         = Nil"
		, "initHalfWork(xs, Cons(x,Nil)) = Nil"
		, "initHalfWork(Cons(a,x), Cons(b,Cons(c,y)))"
		, "                    = Cons(a,initHalfWork(x,y))"
		])
	, ("seive", unlines
		[ "seive (Nil)               = Nil"
		, "seive (Cons(a,Nil))       = Nil"
		, "seive (Cons(a,Cons(b,x))) = Cons(b,seive(x))"
		])
	, ("rev", unlines
		[ "reverse(xs) = rev(xs,Nil)"
		, "rev(Nil,y)       = y"
		, "rev(Cons(a,x),y) = rev(x,Cons(a,y))"
		])
	]

defaultCode = fromJust (lookup "init" examples)
	
outputErrors :: String -> Html
outputErrors s = 
           p << (
                strong << "An error occurred:" +++ br +++
                pre << s
                )
                
mkSubmit active what = submit (submitId what) (submitLabel what)
     	               ! if active then [] else [disabled]

data Run = Get | Check | Load | BiDi | EvalPut | EvalGet


submitId Get = "get source"
submitId Check = "check"
submitId Load = "load"
submitId BiDi = "submitBiDi"
submitId EvalPut = "evalPut"
submitId EvalGet = "evalGet"

submitCode Get   = Just ("get source")
submitCode Check = Nothing
submitCode Load  = Nothing

submitLabel Check =   "Re-Parse definition"
submitLabel Load  =   "Load example"
submitLabel EvalGet = "view = get source"
submitLabel EvalPut = "result = put source view"
submitLabel BiDi =    "bidirectionalize"

main = runCGI (handleErrors cgiMain)

-- This function will not work in all casses, but in most.
delDefinition name code = unlines squashed
  where filtered = filter (not . defines name) (lines code)
	squash [] = []
	squash ("":_) = [""]
	squash ("\r":_) = [""]
	squash ls = ls
	squashed = concat $ map squash $ group $ filtered

addDefiniton name def code = unlines (squashed ++ pad ++ new_line)
  where	squashed = lines (delDefinition name code)
	pad | last squashed == "" || last squashed == "\r" = []
            | otherwise                                    = [""]
	new_line = [name ++ " = " ++ def]
	
defines "" (' ':_) = True
defines "" ('=':_) = True
defines "" "" = False
defines "" _   = False
defines _  ""  = False
defines (i:is) (x:xs) | i == x = defines is xs
                      | i /= x = False

cgiMain = do
    qs <- queryString
    if qs == "jquery"
     then jQueryMain
     else formMain

jQueryMain = do
        setHeader "Content-type" "text/javascript"
        setHeader "Expires" "Fri, 01 Jan 2100 00:00:00 +0100"
        setHeader "Cache-control" "max-age=36000000" -- 1000 h
        outputFPS $ jQueryCode
    
defaultPlayCode get = -- Are we only considering [Nat] here?
        Just $ unlines
            [ "get = " ++ get
            , "put = " ++ get ++ "_B" 
            , ""
            , "source = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10]"
            ]

formMain = do
        setHeader "Content-type" "application/xhtml+xml; charset=UTF-8"

        exMode  <- maybe Normal read <$> getInput "execMode"
        outMode <- maybe HaskellCode read <$> getInput "outputMode"
        showTypes <- isJust <$> getInput "showTypes"
	
	todo <- msum <$> sequence (
            map (\what -> fmap (const what) <$> getInput (submitId what))
            [ BiDi, Get, Check, Load, EvalPut, EvalGet])
        
	code <- fromMaybe defaultCode <$> getInput "code"
	
        code <- case todo of
            Just Load -> do loadWhat <- getInput "load"
                            return $ fromMaybe code $ loadWhat >>= flip lookup examples 
            _ -> return code
        
        let eAST = parseString code

        let conf = defaultConfig { outputMode = outMode, execMode = exMode, isShowType = showTypes }
        let parseError = either (Just . show) (const Nothing) eAST

        let (genCodeM,getM) = case (todo,eAST) of
                (Just Load, _) -> (Nothing, Nothing)
                (Just _, Right ast) ->
                    (  Just $ render $ case exMode of 
                       Normal -> outputCode conf False ast (typeInference ast)
                       Shapify -> outputCode conf False ast (shapify $ typeInference ast)
                       ShapifyPlus -> outputCode conf True  ast (introNat $ shapify $ typeInference ast)
                    ,  firstDeclaredName ast
                    )
                _ -> (Nothing, Nothing)

        pcM <- getInput "playCode" 
        (playCode, playErrorM) <- case (todo,getM,genCodeM,pcM) of
            -- The user successfully generated code to play with, insert default playCode.
            -- Do not use the user input, as he probably switched to a new example.
            (Just BiDi, Just get, Just _, _) -> return (defaultPlayCode get, Nothing)
            -- The user played with the code
            (Just EvalGet, Just get, Just genCode, Just pc) -> do
                view <- liftIO $ evaluateWith genCode pc ("get source")
                case view of 
                    Left err -> return $ (Just pc, Just err)
                    Right dat -> return $ (\r -> (Just r, Nothing))
                                        $ addDefiniton "view" dat 
                                        $ delDefinition "result"
                                        $ pc
            (Just EvalGet, Just get, Just genCode, Nothing) -> do
                return (defaultPlayCode get, Nothing)
            (Just EvalPut, Just get, Just genCode, Just pc) -> do
                view <- liftIO $ evaluateWith genCode pc ("put source view")
                case view of 
                    Left err -> return $ (Just pc, Just err)
                    Right dat -> return $ (\r -> (Just r, Nothing))
                                        $ addDefiniton "result" dat 
                                        $ pc
            (Just EvalPut, Just get, Just _, Nothing) -> do
                return (defaultPlayCode get, Nothing)
            _ -> return (Nothing, Nothing)

        scrollX <- getInput "scrollx"
        scrollY <- getInput "scrolly"

        outputFPS $ fromString $ showHtml $ page $
            PageInfo scrollX
                     scrollY
                     code
                     parseError
                     exMode
                     outMode
                     showTypes
                     genCodeM
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
            ]

withFullSource genCode playCode = genCode' ++ "\n" ++ playCode
    where genCode' = unlines . filter (not . isPrefixOf "import") . lines $ genCode

astInfo (Left err) = maindiv << p << (
	"Can not parse your definition:" +++ br +++
	pre << show err +++ br +++
	mkSubmit True Check)

astInfo (Right source) = maindiv << (
	p << ("Definition parsed succesfully") +++
	p << mkSubmit True Check
	)

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

jsCode = unlines 
    [ "function saveScroll () {"
    , "    $(\"#scrolly\").val($(\"html\").scrollTop());"
    , "}"
    , "function restoreScroll () {"
    , "    $(\"html\").scrollTop($(\"#scrolly\").val());"
    , "}"
    ]

htmlMB Nothing  f = noHtml
htmlMB (Just x) f = f x

readOnly = emptyAttr "readonly"


firstDeclaredName (AST []) = Nothing
firstDeclaredName (AST (Decl n _ _ _:_)) = Just (show n)

{-
 - Temp-Dir functions taken from XMonad/Lock.hs and simplified
 -}
withinTmpDir :: IO a -> IO a
withinTmpDir job = do
  absolute_name <- (++ "/sem_syn.cgi") <$> getTemporaryDirectory
  formerdir <- getCurrentDirectory
  bracket (create_directory absolute_name 0)
          (\dir -> do setCurrentDirectory formerdir
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

