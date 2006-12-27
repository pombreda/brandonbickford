module Main where
import Data.Maybe
import Ctemplate


main = do
         run "hi there"
         run "{{hello:escape_javascript}}"
         run "{{/hello}}"
         run "{{#hello}} hi there {{X}} {{/hello}}"
         run "{{#hello}}{{#x}}{{/x}}{{/hello}}"
         run ""
         run "{{>x}}"
