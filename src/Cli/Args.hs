module Cli.Args (
    parse
  ) where

{- This module parses arguments from the command line.

The main function is the 'parse' function. It takes a list of
arguments from the command line. If the arguments aren't correct
or there is an option for displaying help, then it will exit with
the error message or help. If the arguments are all file paths,
it will try to read all the contents of the files and return
a list of those contents. If it encounters an error while
reading the files, it exits with an error. -}

import Data.List (isPrefixOf)
import qualified Cli.Exit as Exit
import qualified Cli.File as File
import qualified Cli.Output as Output

{- Checks if a list of arguments is empty. -}
noArgs :: [String] -> Bool
noArgs [] = True
noArgs _ = False

{- Checks if a list of arguments contains -h or --help. -}
containsHelp :: [String] -> Bool
containsHelp [] = False
containsHelp args = elem "-h" args || elem "--help" args

{- Filters a list of arguments down to any that begin with
   a dash, which are not -h or --help. The only options
   for this program are -h and --help, so anything else
   is an invalid argument/option. -}
invalidOpts :: [String] -> [String]
invalidOpts args = 
  let isInvalid arg = isPrefixOf "-" arg && arg /= "-h" && arg /= "--help"
  in filter isInvalid args

{- Reads the contents of a file an returns that string, or errors. -}
getFile :: FilePath -> IO String
getFile file = do
  result <- File.read file
  case result of
    Left e -> Exit.exitWithErr $ Output.fileReadErr e
    Right contents -> return contents

{- Takes a list of file paths, then reads them or errors. -}
getFiles :: [FilePath] -> IO [String]
getFiles [] = return $ []
getFiles (file:files) = do
  result <- getFile file
  theRest <- getFiles files
  return (result:theRest)

{- Takes a list of command line arguments. The arguments should be
   a list of file paths. If so, we read those and return their contents.
   If the arguments have -h or --help in them, then we exit with the
   help/usage. If there are other problems, we exit with a message. -}
parse :: [String] -> IO [String]
parse args =
  case noArgs args of
    True -> Exit.exitWithErr Output.noArgsErr
    False -> case containsHelp args of
      True -> Exit.exitWithErr Output.usage
      False -> 
        let badOpts = invalidOpts args
        in case (length badOpts) > 0 of
          True -> Exit.exitWithErr $ Output.invalidOptsErr badOpts
          False -> getFiles args
