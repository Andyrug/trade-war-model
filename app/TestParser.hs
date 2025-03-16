module Main where

import TradeWar.StrategyParser
import TradeWar.StrategyDSL
import Data.Text (pack)

main :: IO ()
main = do
  putStrLn "Testing primitive strategy parser:"
  
  -- Test impose tariff
  putStrLn "\nTesting 'impose tariff on steel 0.20':"
  print $ parseStrategy (pack "impose tariff on steel 0.20")
  
  -- Test remove tariff
  putStrLn "\nTesting 'remove tariff on aluminum':"
  print $ parseStrategy (pack "remove tariff on aluminum")
  
  -- Test negotiate
  putStrLn "\nTesting 'negotiate with Canada':"
  print $ parseStrategy (pack "negotiate with Canada")
  
  -- Test do nothing
  putStrLn "\nTesting 'do nothing':"
  print $ parseStrategy (pack "do nothing")
  
  -- Test mirror
  putStrLn "\nTesting 'mirror':"
  print $ parseStrategy (pack "mirror")
  
  -- Test retaliate
  putStrLn "\nTesting 'retaliate':"
  print $ parseStrategy (pack "retaliate")
  
  -- Test cooperate
  putStrLn "\nTesting 'cooperate':"
  print $ parseStrategy (pack "cooperate") 