module Components.Button ( buttonUnicode ) where

-- Exibe um botão em Unicode no terminal, ajustando automaticamente
-- o tamanho das bordas de acordo com o texto informado
buttonUnicode :: String -> IO ()
buttonUnicode txt = do
  let width = length txt + 2
  putStrLn $ "┌" ++ replicate width '─' ++ "┐"
  putStrLn $ "│ " ++ txt ++ " │"
  putStrLn $ "└" ++ replicate width '─' ++ "┘"
