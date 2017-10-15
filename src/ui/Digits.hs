module Digits (drawNum) where

import Data.List.Split(chunksOf)

type Digit = [String]

drawNum :: Word -> [Digit]
drawNum num = map (\num -> numbers !! read num) . chunksOf 1 $ serialize num

serialize :: Word -> String
serialize = addZero . show

addZero :: String -> String
addZero str = if length str < 2 then '0' : str else str

numbers :: [Digit]
numbers = [zero, one, two, three, four, five, six, seven, eight, nine]

zero :: Digit
zero =  [" |||||| "
       , "||    ||"
       , "||    ||"
       , "||    ||"
       , "||    ||"
       , "||    ||"
       , "||    ||"
       , " |||||| "]
       
one :: Digit
one =   ["   ||   "
       , " ||||   "
       , "|| ||   "
       , "   ||   "
       , "   ||   "
       , "   ||   "
       , "   ||   "
       , "||||||||"]
      
two :: Digit
two =   [" |||||| "
       , "||    ||"
       , "||    ||"
       , "      ||"
       , "    ||  "
       , "  ||    "
       , "||      "
       , "||||||||"]
       
three :: Digit
three = [" |||||| "
       , "||    ||"
       , "||    ||"
       , "    ||| "
       , "      ||"
       , "||    ||"
       , "||    ||"
       , " |||||| "]
        
four :: Digit
four =  ["||    ||"
       , "||    ||"
       , "||    ||"
       , "||    ||"
       , "||||||||"
       , "      ||"
       , "      ||"
       , "      ||"]
        
five :: Digit
five =  ["||||||||"
       , "||      "
       , "||      "
       , "||||||| "
       , "      ||"
       , "      ||"
       , "      ||"
       , "||||||| "]
        
six :: Digit
six =   [" |||||| "
       , "||    ||"
       , "||      "
       , "||||||| "
       , "||    ||"
       , "||    ||"
       , "||    ||"
       , " |||||| "]
        
seven :: Digit
seven = ["||||||||"
       , "     |||"
       , "    ||  "
       , "   ||   "
       , " ||     "
       , "||      "
       , "||      "
       , "||      "]
        
eight :: Digit
eight = [" |||||| "
       , "||    ||"
       , "||    ||"
       , " |||||| "
       , "||    ||"
       , "||    ||"
       , "||    ||"
       , " |||||| "]
        
nine :: Digit
nine =  [" |||||| "
       , "||    ||"
       , "||    ||"
       , "||    ||"
       , " |||||||"
       , "      ||"
       , "||    ||"
       , " |||||| "]
