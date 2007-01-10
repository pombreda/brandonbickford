{-- parser for a language like Google Ctemplate.  --} 
module Ctemplate (parseTemplate, run, Block, fill, TemplateValues(TemplateValues), runOrError, addSection, setValue, initTemplateValues, addSubTemplate, Template(Template), (<<#), (<<$), addOutputFilter) where

import Text.ParserCombinators.Parsec
import Control.Monad
import qualified Text.ParserCombinators.Parsec.Token as P
import Data.Map (Map, member, findWithDefault) 
import Data.Maybe
import qualified Data.Map as Map

data Block = Display String [String] 
   | Section String [Block] 
   | Raw String 
   | Include String 
   deriving (Show, Ord, Eq) 

type NameToTemplate = Map String Template
type NameToValue = Map String String
type NameToSubSections = Map String [TemplateValues]
type NameToOutputFilter = Map String (String -> String) 

data TemplateValues = TemplateValues { 
   values :: NameToValue, 
   sections :: NameToSubSections, 
   subtemplates :: NameToTemplate, 
   output_filters :: NameToOutputFilter
}

data Template = Template {
   blocks :: [Block], 
   template_values :: TemplateValues
}

infixl 2 <<$
infixl 1 <<#
templatedictionary <<$ (k, v) = addSection templatedictionary k v  
templatedictionary <<# (k, v) = setValue templatedictionary k v

initTemplateValues = TemplateValues {values = Map.empty, sections = Map.empty, subtemplates = Map.empty, output_filters = Map.empty}

setValue :: TemplateValues -> String -> String -> TemplateValues 
setValue template_values key value = 
   template_values { values = Map.insert key value $ values template_values}

addSubTemplate template_values subtemplate_name subtemplate = template_values {
   subtemplates = Map.insert subtemplate_name subtemplate $ subtemplates template_values
}

addOutputFilter template_values output_filter_name output_filter = template_values { 
   output_filters = Map.insert output_filter_name output_filter $ output_filters template_values
}

addSection :: TemplateValues -> String -> TemplateValues -> TemplateValues
addSection templatedictionary section_name section = 
   templatedictionary { sections = Map.insert section_name othersections $ sections templatedictionary }
   where othersections = section:(Map.findWithDefault [] section_name $ sections templatedictionary) 
{-- FILLING --}

fill :: [Block] -> [TemplateValues] -> String
fill template_blocks template_values_stack = foldl (++) "" $ map fill_block template_blocks 
   where fill_block (Raw x) = x
         fill_block (Include sub_section_name) = 
            let matching_sub_sections :: [Template]
                matching_sub_sections = catMaybes $ map (Map.lookup sub_section_name) subtemplates'
                subtemplates' = map subtemplates template_values_stack
                subtemplate :: Template
                subtemplate = case matching_sub_sections of
                  [] -> error $ "Can't find " ++ sub_section_name
                  matches -> head matches 
                in fill (blocks subtemplate) [(template_values subtemplate)]

         fill_block (Display name output_filter_names) =  
            let matching_settings = catMaybes $ maybe_matching_settings
                maybe_matching_settings :: [Maybe String]
                maybe_matching_settings = map (Map.lookup name) settings_list
                settings_list :: [Map String String]
                settings_list = map values template_values_stack
                match = case matching_settings of 
                   [] -> error $ "Can't find " ++ name
                   matches -> head matches 
                output_filters' = map find_output_filter output_filter_names   
            in foldl (\x y -> y x) match output_filters'

         fill_block (Section name blocks') = 
            let subgroups = map (\x -> fill blocks' $ x:template_values_stack) subdictionaries 
                subdictionaries = findWithDefault [] name $ sections current_dictionary  
                current_dictionary = head template_values_stack
            in foldl (++) "" subgroups  
         
         find_output_filter :: String -> (String -> String)
         find_output_filter name = 
            let output_filter_mappings :: [Map String (String -> String)]
                output_filter_mappings = map output_filters template_values_stack
                maybe_matching_output_filters = catMaybes $ map (Map.lookup name) output_filter_mappings
            in case maybe_matching_output_filters of 
               [] -> error $ "Can't find " ++ name
               (x:xs) -> x 
         

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

includefile = liftM Include $ in_tag $ do 
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
