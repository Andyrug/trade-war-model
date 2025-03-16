import TradeWar.StrategyParser; import TradeWar.StrategyDSL; import Data.Text (pack); main = print $ parseStrategy (pack "impose tariff on steel 0.20")
