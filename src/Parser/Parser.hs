module Parser.Parser where

import Parser.Tree (Tree(..))

parse :: FilePath -> IO Tree
parse fpath = do
    _ <- readFile fpath
    return None
