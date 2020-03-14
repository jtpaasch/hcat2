module Main (main) where

import qualified System.Environment as Environ
import qualified Cli.Args as CliArgs
import qualified App.Handler as Handler

-- The main entry point into the program.
main :: IO ()
main = do

  -- Get the arguments provided at the command line.
  args <- Environ.getArgs

  -- Parse those args. The arguments should be a list of filepaths,
  -- or -h/--help. The 'CliArgs.parse' function will show the help if
  -- the arguments include -h or --help, or it will exit with an
  -- appropriate error message if the arguments are not correct.
  -- If the arguments are a list of readable filepaths, then it will
  -- return a list of contents from those files.
  fileContents <- CliArgs.parse args

  -- Hand the file contents off to the application to handle them.
  -- Then print the results.
  let result = Handler.run fileContents
  putStr result
