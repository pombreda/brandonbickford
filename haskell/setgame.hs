
import qualified Data.Set 
import Control.Monad
import qualified Data.List as List

data ColorAttr = Red | Green | Purple 
   deriving (Ord, Eq, Show);

data ShapeAttr = Diamond | Oval | Squiggle
   deriving (Ord, Eq, Show);

data NumberAttr = N1 | N2 | N3 
   deriving (Ord, Eq, Show);

data FillAttr = Solid | Filled | Hatched
   deriving (Ord, Eq, Show);

data Card = Card FillAttr NumberAttr ShapeAttr ColorAttr 
   deriving (Ord, Eq, Show) 

data ASet = ASet Card Card Card deriving (Ord, Eq, Show) 

mkSet cards = ASet c1 c2 c3
   where cards' = List.sort cards
         c1 = cards' !! 0
         c2 = cards' !! 1
         c3 = cards' !! 2

deck =  [(Card w x y z) | w <- [Solid, Filled, Hatched], x <- [N1, N2, N3], y <- [Diamond, Oval, Squiggle], z <- [Red, Green, Purple]]

is_set :: Card -> Card -> Card -> Bool
is_set card1@(Card f1 n1 s1 c1) card2@(Card f2 n2 s2 c2) card3@(Card f3 n3 s3 c3) = (
         card1 /= card2 && card2 /= card3 && card1 /= card3 &&
         equal_or_different f1 f2 f3 && equal_or_different n1 n2 n3 && equal_or_different s1 s2 s3 && equal_or_different c1 c2 c3)
      where 
         equal_or_different x y z = (x == y && y == z) || (x /= y && y /= z && x /= z)

pairs acard = pairs'
   where pairs' = List.nub $ List.sort [(mkSet [acard, c1, c2]) | c1 <- deck, c2 <- deck, is_set acard c1 c2]

has_set cards = valid_set /= Nothing 
   where valid_set = foldr mplus Nothing [Just (c1, c2, c3) | c1 <- cards, c2 <- cards, c3 <- cards, c1 < c2, c2 < c3, is_set c1 c2 c3]

not_set k= not (has_set k)


not_pairs card cards = not_pairs' cards [card]
   where
      not_pairs' :: [Card] -> [Card] -> [Card]
      not_pairs' [] sofar = sofar
      not_pairs' (h:t) sofar = 
         if not_set (h:sofar) && not (h `elem` sofar)
         then 
            not_pairs' t (h:sofar)
         else
            not_pairs' t sofar

all_sets = List.nub $ List.sort [(mkSet [c1, c2, c3])|c1 <- deck, c2 <- deck, c3 <- deck, is_set c1 c2 c3]

to_cards :: [ASet] -> [Card]
to_cards sets = List.sort $ List.nub $ to_cards' sets
      where to_cards' [] = []
            to_cards' ((ASet c1 c2 c3):xs) = c1:c2:c3:(to_cards' xs) 

max_not_set_size = foldr max 0 $ map (\card -> length $ not_pairs card deck) deck          
