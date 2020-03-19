module Cli.Output (
    usage
  , noArgsErr
  , invalidOptsErr
  , fileReadErr
  ) where

{- This module provides messages (e.g., errors) to print as output. -}

import Data.List (intercalate)
import qualified Cli.File as File

{- The help/usage message. -}
usage :: String
usage =
  unlines [
      "USAGE: hcat2 [OPTIONS] [ARGUMENTS]"
    , ""
    , "  A simple cat program."
    , ""
    , "EXAMPLES:"
    , "  hcat2 --help"
    , "  hcat2 /path/to/file1 /path/to/file2 ..."
    , ""
    , "OPTIONS:"
    , "  -h, --help       Display this help."
    , ""
    , "ARGUMENTS:"
    , "  /path/to/file1   A path to a file."
    , "  /path/to/file2   A path to a file."
    , "  ...              Ditto."
    ]

{- Message if no arguments/filepaths were provided to the command line. -}
noArgsErr :: String
noArgsErr = "No file paths were specified.\nSee: hcat --help"

{- Message if any command line arguments are invalid options. -} 
invalidOptsErr :: [String] -> String
invalidOptsErr args =
     "Unrecognized option(s): "
  ++ (intercalate ", " args)
  ++ "\nSee: hcat --help"

{- Message if there was an error reading a file. -}
fileReadErr :: File.Error -> String
fileReadErr e =
  case e of
    File.NoFile path -> "No such file: " ++ path
    File.NoPerm path -> "No permissions for: " ++ path
    File.InUse path -> "File is in use: " ++ path
    File.DiskFull path -> "Disk full. Cannot read: " ++ path
    File.Other msg -> "Unknown error: " ++ msg
