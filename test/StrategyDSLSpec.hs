{-# LANGUAGE OverloadedStrings #-}
module StrategyDSLSpec (spec) where

import Test.Hspec
import Data.Text (Text)
import qualified TradeWar.Types as T hiding (ImposeTariff, RemoveTariff, NegotiateTrade, NoAction)
import TradeWar.StrategyDSL

spec :: Spec
spec = do
    describe "Strategy DSL" $ do
        it "can construct primitive strategies" $ do
            let s1 = Primitive (ImposeTariff "steel" 20.0)
            let s2 = Primitive (RemoveTariff "aluminum")
            let s3 = Primitive (Negotiate "Canada")
            let s4 = Primitive DoNothing
            let s5 = Primitive Mirror
            let s6 = Primitive Retaliate
            let s7 = Primitive Cooperate
            
            s1 `shouldBe` Primitive (ImposeTariff "steel" 20.0)
            s4 `shouldBe` Primitive DoNothing

        it "can construct combined strategies" $ do
            let s1 = Primitive (ImposeTariff "steel" 20.0)
            let s2 = Primitive (Negotiate "Canada")
            let combined = Combinator Sequential [s1, s2]
            
            combined `shouldBe` Combinator Sequential 
                [ Primitive (ImposeTariff "steel" 20.0)
                , Primitive (Negotiate "Canada")
                ]

        it "can construct conditional strategies" $ do
            let condition = When (PayoffAbove 100.0)
            let thenStrat = Primitive Retaliate
            let elseStrat = Primitive Cooperate
            let conditional = Conditional (IfThenElse (PayoffAbove 100.0)) thenStrat elseStrat
            
            conditional `shouldBe` Conditional 
                (IfThenElse (PayoffAbove 100.0))
                (Primitive Retaliate)
                (Primitive Cooperate)

        it "can construct random strategies" $ do
            let s1 = Primitive (ImposeTariff "steel" 20.0)
            let s2 = Primitive (Negotiate "Canada")
            let random = Random RandomChoice [s1, s2] []
            let weighted = Random Weighted [s1, s2] [0.7, 0.3]
            
            random `shouldBe` Random RandomChoice 
                [ Primitive (ImposeTariff "steel" 20.0)
                , Primitive (Negotiate "Canada")
                ] []
            
            weighted `shouldBe` Random Weighted
                [ Primitive (ImposeTariff "steel" 20.0)
                , Primitive (Negotiate "Canada")
                ] [0.7, 0.3]

        it "can construct modified strategies" $ do
            let base = Primitive Retaliate
            let limited = Modified (Limit 5) base
            let timed = Modified (Times 3) base
            let untilPayoff = Modified (UntilPayoff 100.0) base
            
            limited `shouldBe` Modified (Limit 5) (Primitive Retaliate)
            timed `shouldBe` Modified (Times 3) (Primitive Retaliate)
            untilPayoff `shouldBe` Modified (UntilPayoff 100.0) (Primitive Retaliate)

        it "can construct complex nested strategies" $ do
            let complexStrategy = 
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
            
            complexStrategy `shouldBe` 
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