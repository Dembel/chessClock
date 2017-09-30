module NeatNumbers (constructNum) where

import Data.List.Split(chunksOf)

type NeatNumber = [String]

constructNum :: String -> [[String]]
constructNum num = map (\num -> numbers !! read num) $ chunksOf 1 num

numbers :: [NeatNumber]
numbers = [zero, one, two, three, four, five, six, seven, eight, nine]

zero :: NeatNumber
zero =  [" |||||| "
       , "||    ||"
       , "||    ||"
       , "||    ||"
       , "||    ||"
       , "||    ||"
       , "||    ||"
       , " |||||| "]
       
one :: NeatNumber
one =   ["   ||   "
       , " ||||   "
       , "|| ||   "
       , "   ||   "
       , "   ||   "
       , "   ||   "
       , "   ||   "
       , "||||||||"]
      
two :: NeatNumber
two =   [" |||||| "
       , "||    ||"
       , "||    ||"
       , "      ||"
       , "    ||  "
       , "  ||    "
       , "||      "
       , "||||||||"]
       
three :: NeatNumber
three = [" |||||| "
       , "||    ||"
       , "||    ||"
       , "    ||| "
       , "      ||"
       , "||    ||"
       , "||    ||"
       , " |||||| "]
        
four :: NeatNumber
four =  ["||    ||"
       , "||    ||"
       , "||    ||"
       , "||    ||"
       , "||||||||"
       , "      ||"
       , "      ||"
       , "      ||"]
        
five :: NeatNumber
five =  ["||||||||"
       , "||      "
       , "||      "
       , "||||||| "
       , "      ||"
       , "      ||"
       , "      ||"
       , "||||||| "]
        
six :: NeatNumber
six =   [" |||||| "
       , "||    ||"
       , "||      "
       , "||||||| "
       , "||    ||"
       , "||    ||"
       , "||    ||"
       , " |||||| "]
        
seven :: NeatNumber
seven = ["||||||||"
       , "     |||"
       , "    ||  "
       , "   ||   "
       , " ||     "
       , "||      "
       , "||      "
       , "||      "]
        
eight :: NeatNumber
eight = [" |||||| "
       , "||    ||"
       , "||    ||"
       , " |||||| "
       , "||    ||"
       , "||    ||"
       , "||    ||"
       , " |||||| "]
        
nine :: NeatNumber
nine =  [" |||||| "
       , "||    ||"
       , "||    ||"
       , "||    ||"
       , " |||||||"
       , "      ||"
       , "||    ||"
       , " |||||| "]
