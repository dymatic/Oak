import System.IO
import System.Process
import System.Environment

import Archimedes.Sequence.Clarify
import Archimedes.Sequence.Functional
import Archimedes.Sequence.Manipulate

generate :: [String] -> [String]
generate [] = ["cd .."]
generate (x:[]) = [("mkdir " ++ (rm x '|'))]
generate (x:a:xs)
  | depth a > depth x = makeCD : ("cd " ++ cleanx) : makeND : generate (a:xs)
  | depth a < depth x = (take (count x '|' - (count a '|')) (repeat "cd ..")) ++ generate (a:xs)
  | otherwise = makeCD : generate (a:xs) 
    where depth = (flip count) '|'
          cleanx = (rm x '|')
          makeCD = ("mkdir " ++ cleanx)
          makeND = ("mkdir " ++ (rm a '|'))

removeDoubles :: [String] -> [String]
removeDoubles [] = []
removeDoubles [x] = [x]
removeDoubles (x:a:xs)
  | and [x `contains` "cd",a `contains` "cd"] = x : removeDoubles (a:xs)
  | x == a = a : removeDoubles xs
  | otherwise = x : removeDoubles (a:xs)
                
main = do
  (a:_) <- getArgs
  file <- openFile a ReadMode
  contents <- hGetContents file
  let root = ((reverse (after (reverse a) '/'))++"/")
  putStrLn root
  writeFile (root ++ "layoutdirectories.sh") (unlines (removeDoubles(generate (lines contents))))

  system ( "chmod +x " ++ (root ++ "layoutdirectories.sh"))
  system ( "bash " ++ (root ++ "layoutdirectories.sh"))
  system ( "rm " ++ (root ++ "layoutdirectories.sh"))
  
