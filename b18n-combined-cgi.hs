import Network.CGI
import Text.XHtml
import Data.Maybe
import Data.List
import Data.ByteString.Lazy.UTF8 (fromString)
import Control.Monad
import Control.Applicative ((<$>),(<*>))
import Text.PrettyPrint.HughesPJ (render)

import Parser
import SemSyn
import Type
import Shapify

page code pageContent =
       header << (
	thetitle << "Combining Syntatic and Semantic Bidirectionalization" +++
	style ! [ thetype "text/css" ] << cdata cssStyle
       ) +++
       body << (
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
        form ! [method "POST", action "#"] << (
		maindiv << (
			 p << (
				"Please enter the view function. (TODO: Elaborate this text)"
			) +++

			p << (
				concatHtml (map (\(name,thisCode) -> 
					radio "load" name
					! (if thisCode == code then [checked] else [])
					+++ name +++ " "
				) examples) +++
				mkSubmit True Load +++
				br +++
				textarea ! [name "code", cols "120", rows "7"] << code
			) 
			
		) +++
 		pageContent
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

defaultCode = fromJust (lookup "halve" examples)
	
outputErrors :: String -> Html
outputErrors s = 
           p << (
                strong << "An error occurred:" +++ br +++
                pre << s
                )
                
mkSubmit active what = submit (submitId what) (submitLabel what)
     	               ! if active then [] else [disabled]

data Run = Get | Check | Load | BiDi


submitId Get = "get source"
submitId Check = "check"
submitId Load = "load"
submitId BiDi = "submitBiDi"

submitCode Get   = Just ("get source")
submitCode Check = Nothing
submitCode Load  = Nothing
submitCode BiDi = Just ("bidirectionalize")

submitLabel Check = "Re-Parse definition"
submitLabel Load  = "Load example"
submitLabel x   = fromJust (submitCode x)

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
        setHeader "Content-type" "text/xml; charset=UTF-8"

        exMode  <- maybe Normal read <$> getInput "execMode"
        outMode <- maybe PseudoCode read <$> getInput "outputMode"
        showTypes <- isJust <$> getInput "showTypes"
	
	todo <- fromMaybe Check . msum <$> sequence (
            map (\what -> fmap (const what) <$> getInput (submitId what))
            [ BiDi, Get, Check, Load ])
        
	code <- fromMaybe defaultCode <$> getInput "code"
	
        let mbAST = parseString code

        code <- case todo of
            Load -> do loadWhat <- getInput "load"
                       return $ fromMaybe code $ loadWhat >>= flip lookup examples 
            _ -> return code

        let conf = defaultConfig { outputMode = outMode, execMode = exMode, isShowType = showTypes }
        let genCode = case mbAST of
              Left _ -> ""
              Right ast -> render $ case exMode of 
                   Normal -> outputCode conf False ast (typeInference ast)
                   Shapify -> outputCode conf False ast (shapify $ typeInference ast)
                   ShapifyPlus -> outputCode conf True  ast (introNat $ shapify $ typeInference ast)


        outputFPS $ fromString $ showHtml $ page code $
		{- p << astInfo mbAST +++ -}
		maindiv ! [ identifier "output" ]<< (
			p << (
				"You can calculate a derived put function with various options:" ) +++
			p << ( "Execution mode: " +++
			       concatHtml (map (\mode -> 
			          radio "execMode" (show mode) 
					! (if mode == exMode then [checked] else [])
					+++ show mode +++ " "
                                ) [Normal, Shapify, ShapifyPlus]) +++ br +++
			       "Output mode: " +++
			       concatHtml (map (\mode -> 
			          radio "outputMode" (show mode) 
					! (if mode == outMode then [checked] else [])
					+++ show mode +++ " "
                                ) [PseudoCode, HaskellCode, ForwardCode]) +++ br +++
			       "Show types " +++ checkbox "showTypes" "showTypes"
                                        ! (if showTypes then [checked] else [])
                                        +++ br +++
			       mkSubmit True BiDi
			) +++
			{- maybe noHtml outputErrors errors +++ -}
                        p << ("Result:"+++ br +++
			    textarea ! [name "gencode", cols "120"
                                       , rows (show (1 + length (lines genCode)))
                                       ] << genCode

                        )
		)
		
astInfo (Left err) = maindiv << p << (
	"Can not parse your definition:" +++ br +++
	pre << show err +++ br +++
	mkSubmit True Check)

astInfo (Right source) = maindiv << (
	p << ("Definition parsed succesfully"
{-		"Your definitions have the following types: " +++
		pre << ("get :: " ++ getType ++ "\n"++
		        "source :: " ++ sourceType) +++
		"Therefore, an updater can be derived by " +++
		case (canBff, canBffEq, canBffOrd) of
			(True, _, _) -> 
				tt << "bff" +++ ", " +++
				tt << "bff_Eq" +++ ", and " +++
				tt << "bff_Ord" +++ "."
			(False, True, _) -> 
				tt << "bff_Eq" +++ " and " +++
				tt << "bff_Ord" +++ "."
			(False, False, True) -> 
				tt << "bff_Ord" +++ " only."
			(False, False, False) -> 
				"none of the " +++ tt << "bff" +++ " functions."
-}                                
	) +++
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

