module App.Handler (
    run
  ) where

{- The main application handler. -}

import Data.List (intercalate)

{- Running this application just concatenates the arguments. -}
run :: [String] -> String
run = intercalate ""

