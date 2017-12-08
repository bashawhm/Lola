import qualified Run as Run

{-
The main function reads in the program form standard input
  then returns the memory after the program runs
  I either read a file on linux by redirection
  or I type in the program followed by ctrl-d when I am done
-}
main = do
  contents <- getContents
  let output = Run.run contents
  print output
  
