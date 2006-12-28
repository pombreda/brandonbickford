{-- parser for a language like Google Ctemplate.  --} 
module Ctemplate (parseTemplate, run, Block, fill, TemplateDictionary(TemplateDictionary), runOrError) where

import Text.ParserCombinators.Parsec
import Control.Monad
import qualified Text.ParserCombinators.Parsec.Token as P
import Data.Map (Map, member, findWithDefault) 
import Data.Maybe
import qualified Data.Map

data Block = Display String [String] | Section String [Block] | Raw String | IncludeFile String 
   deriving (Show, Ord, Eq) 

data TemplateDictionary = TemplateDictionary { values :: Map String String, sections :: Map String [TemplateDictionary]}


{-- FILLING --}

fill :: [Block] -> [TemplateDictionary] -> String
fill templateblocks template_dictionary_stack = foldl (++) "" $ map fill' templateblocks
   where fill' (Raw x) = x
         fill' (IncludeFile x) = "[[FILE " ++ x ++ "]]"
         fill' (Display name modifiers) =  
            let matching_settings = catMaybes $ maybe_matching_settings
                maybe_matching_settings :: [Maybe String]
                maybe_matching_settings = map (Data.Map.lookup name) settings_list
                settings_list :: [Map String String]
                settings_list = map values template_dictionary_stack

            in case matching_settings of 
               [] -> fail $ "can't find " ++ name
               matches -> head matches 

         fill' (Section name blocks) = 
            let subgroups = map (\x -> fill blocks $ x:template_dictionary_stack) subdictionaries 
                subdictionaries = findWithDefault [] name $ sections current_dictionary  
                current_dictionary = head template_dictionary_stack
            in foldl (++) "" subgroups  
                
{-- PARSING --}

in_tag = between (string "{{") (string "}}") 

section = do 
   section_name <- startsection
   blocks <- many block
   endsection section_name
   return $ Section section_name blocks
   where 
      startsection = in_tag $ do
         char '#'
         section_name <- parseIdentifier
         return section_name

      endsection s = in_tag $ do  
         char '/'
         string s
         return ()

display = in_tag $ do 
   idents <- sepBy1 parseIdentifier $ char ':'
   return $ Display (head idents) (tail idents)

includefile = liftM IncludeFile $ in_tag $ do 
   char '>'
   filename <- parseIdentifier
   return filename 

parseIdentifier = many1 (choice [alphaNum, char '_'])

optionMaybe a_parser = (try $ do 
      element <- a_parser
      return $ Just element 
   ) <|> (return Nothing)

rawtext = liftM Raw rawtext'
   where
      rawtext' = do
         beginning <- anyChar
         maybe_rest <- optionMaybe rawtext'
         case maybe_rest of
            Nothing -> return [beginning]
            Just rest ->
               let matches_start = beginning == '{' && (head rest) == '{'
               in if matches_start then unexpected rest else return $ beginning:rest 

block = choice [try section, try display, try includefile, try rawtext]

parseTemplate' = do
   blocks <- many block 
   eof
   return blocks 

run input = 
   case (parseTemplate input) of
      Left err -> do 
         print $ "parse error at " ++ (show err)
      Right x  -> 
         print x

runOrError input = 
   case (parseTemplate input) of
      Left err -> error $ "parse error" ++ (show err)
      Right x  -> x 
            
parseTemplate = parse parseTemplate' "" 
