import Prelude
import System.Environment (getArgs)
import System.IO
import Text.Regex
import Text.Regex.Posix

num = 5

occ :: String -> String
occ =  unlines . take num . filter (=~ "^.* [1|2] .*$") . lines

uno :: String -> String
uno =  unlines . reverse . take num . reverse . filter (=~ "^.* 0 .*$") . lines

spin :: String -> String -> (String, String)
spin s r = case matchRegexAll (mkRegex r) s of
            Just (pre,_,aft,_) ->  (pre, aft)
            otherwise ->  (s,"")

alpha :: String -> String
alpha s = fst (spin s "Beta MOs")

beta :: String -> String
beta s =  snd(spin s "Beta MOs")

main = do
     args <- getArgs
     file <- readFile $ head args
     putStrLn "alpha"
     putStr $ uno $ alpha file
     putStr $ occ $ alpha file
     putStrLn "Beta"
     putStr $ uno $ beta file
     putStr $ occ $ beta file
