
module Main (
        main
        ) where

import System.Environment
import ByteCodeParser.Reader
import ByteCodeParser.BasicTypes

import qualified Data.Text as T

main = do
    (path : _) <- getArgs
    result <- readRawClassFile (T.pack path)
    print result
