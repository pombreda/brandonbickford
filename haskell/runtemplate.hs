module Main where
import Data.Maybe
import Ctemplate

import qualified Data.Map as Map
import Data.Map (fromList, empty, singleton)
main = do
         run "hi there"
         run "{{hello:escape_javascript}}"
         run "{{/hello}}"
         run "{{#hello}} hi there {{X}} {{/hello}}"
         run "{{#hello}}{{#x}}{{/x}}{{/hello}}"
         run ""
         run "{{>x}}"
         testSubsection
         testFillK
         testFillSubTemplate
         testOutputFilter

testSubsection = do 
   print $ fill y [m]
   where y = runOrError "{{#results}}{{name}}, {{/results}}"
         m = (initTemplateValues <<$ ("results", initTemplateValues <<# ("name", "Juan")) <<$ ("results", initTemplateValues <<# ("name", "Brandon")))

testFillK = do 
   f <- readFile "k.tmpl" 
   let p = runOrError f
   let v = initTemplateValues <<# ("hithere", "Hi There")
   
   print $ fill p [v]

testOutputFilter = do 
   let t = runOrError "whats up {{hello:dots}}"
       dots s = "..." ++ s ++ "..."
       v = ((initTemplateValues <<# ("hello", "Brandon")) `addOutputFilter` "dots") dots
   print $ fill t [v]

testFillSubTemplate = do 
   f <- readFile "k.tmpl"
   g <- readFile "g.tmpl"
   let f'  = runOrError f
       g'  = runOrError g
       f'' = Template f' (initTemplateValues <<# ("hithere", "Hi There"))
       v = initTemplateValues `addSubTemplate` "K" $ f''
   print $ fill g' [v]
