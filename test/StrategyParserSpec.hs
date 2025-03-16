{-# LANGUAGE OverloadedStrings #-}
module StrategyParserSpec (spec) where

import Test.Hspec
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void
import Text.Megaparsec (ParseErrorBundle)

import TradeWar.StrategyDSL
import TradeWar.StrategyParser
import qualified TradeWar.Types as Types

-- Helper function to create a simple industry for testing
makeTestIndustry :: Text -> Types.Industry
makeTestIndustry name = Types.Industry 
    { Types.industryName = name
    , Types.industryGDP = 0.0
    , Types.industryExportDependence = 0.0
    , Types.industryImportDependence = 0.0
    }

-- Helper function to create a simple country for testing
makeTestCountry :: Text -> Types.Country
makeTestCountry name = Types.Country
    { Types.countryName = name
    , Types.countryEconomy = Types.Economy 0.0 0.0 0.0 0.0
    , Types.countryIndustries = []
    }

spec :: Spec
spec = do
    describe "Strategy Parser" $ do
        it "can parse primitive strategies" $ do
            parseStrategy "impose tariff on steel at 20%" `shouldParse` 
                Primitive (ImposeTariff "steel" 20.0)
            
            parseStrategy "remove tariff on aluminum" `shouldParse` 
                Primitive (RemoveTariff "aluminum")
            
            parseStrategy "negotiate with Canada" `shouldParse` 
                Primitive (Negotiate "Canada")
            
            parseStrategy "do nothing" `shouldParse` 
                Primitive DoNothing
            
            parseStrategy "mirror" `shouldParse` 
                Primitive Mirror
            
            parseStrategy "retaliate" `shouldParse` 
                Primitive Retaliate
            
            parseStrategy "cooperate" `shouldParse` 
                Primitive Cooperate

        it "can parse combined strategies" $ do
            parseStrategy "sequential [impose tariff on steel at 20%, negotiate with Canada]" `shouldParse`
                Combinator Sequential 
                    [ Primitive (ImposeTariff "steel" 20.0)
                    , Primitive (Negotiate "Canada")
                    ]

        it "can parse conditional strategies" $ do
            parseStrategy "if payoff > 100 then retaliate else cooperate" `shouldParse`
                Conditional 
                    (IfThenElse (PayoffAbove 100.0))
                    (Primitive Retaliate)
                    (Primitive Cooperate)
            
            parseStrategy "when growth < 2 impose tariff on steel at 10%" `shouldParse`
                Conditional 
                    (When (GrowthBelow 2.0))
                    (Primitive (ImposeTariff "steel" 10.0))
                    (Primitive DoNothing)

            parseStrategy "when last action was impose_tariff steel 25 retaliate" `shouldParse`
                Conditional
                    (When (LastActionWas (Types.ImposeTariff (makeTestIndustry "steel") 25.0)))
                    (Primitive Retaliate)
                    (Primitive DoNothing)

        it "can parse predicates correctly" $ do
            parseStrategy "when has tariffs retaliate" `shouldParse`
                Conditional
                    (When HasTariffs)
                    (Primitive Retaliate)
                    (Primitive DoNothing)

        it "can parse random strategies" $ do
            parseStrategy "random choice [retaliate, cooperate]" `shouldParse`
                Random RandomChoice 
                    [ Primitive Retaliate
                    , Primitive Cooperate
                    ] []
            
            parseStrategy "weighted [impose tariff on steel at 20%, negotiate with Canada] [0.7, 0.3]" `shouldParse`
                Random Weighted
                    [ Primitive (ImposeTariff "steel" 20.0)
                    , Primitive (Negotiate "Canada")
                    ] [0.7, 0.3]

        it "can parse modified strategies" $ do
            parseStrategy "limit 5 retaliate" `shouldParse`
                Modified (Limit 5) (Primitive Retaliate)
            
            parseStrategy "with memory 3 cooperate" `shouldParse`
                Modified (WithMemory 3) (Primitive Cooperate)

        it "can parse complex nested strategies" $ do
            let complexStr = T.unlines
                    [ "if payoff > 100"
                    , "then limit 5 (random choice [retaliate, impose tariff on steel at 20%])"
                    , "else sequential [negotiate with Canada, cooperate]"
                    ]
            
            parseStrategy complexStr `shouldParse`
                Conditional 
                    (IfThenElse (PayoffAbove 100.0))
                    (Modified 
                        (Limit 5) 
                        (Random RandomChoice 
                            [ Primitive Retaliate
                            , Primitive (ImposeTariff "steel" 20.0)
                            ] []))
                    (Combinator Sequential 
                        [ Primitive (Negotiate "Canada")
                        , Primitive Cooperate
                        ])

-- Helper function for testing parsers
shouldParse :: Either (ParseErrorBundle Text Void) Strategy -> Strategy -> Expectation
shouldParse result expected = case result of
    Left err -> expectationFailure $ "Parse failed: " ++ show err
    Right actual -> actual `shouldBe` expected 