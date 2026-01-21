module Botao
  ( buttonUnicode
  , 
  ) where

buttonUnicode :: String -> IO ()
buttonUnicode txt = do
  let width = length txt + 2
  putStrLn $ "┌" ++ replicate width '─' ++ "┐"
  putStrLn $ "│ " ++ txt ++ " │"
  putStrLn $ "└" ++ replicate width '─' ++ "┘"
