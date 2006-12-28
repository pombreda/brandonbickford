module Main where
import Data.Maybe
import Ctemplate

import qualified Data.Map
import Data.Map (fromList, empty, singleton)

main = do
         run "hi there"
         run "{{hello:escape_javascript}}"
         run "{{/hello}}"
         run "{{#hello}} hi there {{X}} {{/hello}}"
         run "{{#hello}}{{#x}}{{/x}}{{/hello}}"
         run ""
         run "{{>x}}"
         let y = runOrError "{{#results}}{{name}}, {{/results}}"
             n name = singleton "name" name
             results x = singleton "results" x
             m = TemplateDictionary empty (results [TemplateDictionary (n "Brandon") empty, TemplateDictionary (n "John") empty])

         print $ fill y [m]

