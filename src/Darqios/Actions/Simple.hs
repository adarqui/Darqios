module Darqios.Launch.Simple (
) where

import System.DevUtils-Parser.Cmd

data Launch = Launch {
 _stores :: [Cmd],
 _action :: [Cmd]
} deriving (Show)
