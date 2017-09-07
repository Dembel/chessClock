module NeatNumbers (numbers) where

type NeatNumber = [String]

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
