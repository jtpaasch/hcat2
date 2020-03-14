module Cli.File (
    Error (..)
  , Cli.File.read
  ) where

{- This module provides utilities for reading a file. -}

import qualified Control.Exception as Exc
import qualified System.IO as SysIO
import qualified System.IO.Error as SysIOError

{- Custom errors we might encounter while reading a file. -}
data Error =
    NoFile FilePath
  | NoPerm FilePath
  | InUse FilePath
  | DiskFull FilePath
  | Other String

{- Get the filename associated with an 'IOError'. -}
getFilename :: SysIOError.IOError -> FilePath
getFilename e =
  case SysIOError.ioeGetFileName e of
    Nothing -> "[Unknown file]"
    Just name -> name

{- Convert 'IOError's into custom 'Error's. -}
handleError :: SysIOError.IOError -> Maybe Error
handleError e
  | SysIOError.isDoesNotExistError e = Just $ NoFile (getFilename e)
  | SysIOError.isPermissionError e = Just $ NoPerm (getFilename e)
  | SysIOError.isAlreadyInUseError e = Just $ InUse (getFilename e)
  | SysIOError.isFullError e = Just $ DiskFull (getFilename e)
  | otherwise = Nothing

{- Read a file, and return success or an error. -}
read :: FilePath -> IO (Either Error String)
read path = Exc.tryJust handleError (SysIO.readFile path)
