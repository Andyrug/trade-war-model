module StrategyCombinatorSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import qualified Data.Map as M
import Data.Text (pack)
import Control.Monad (forM_)
import Control.Monad.Random (evalRand, mkStdGen)

import TradeWar.Types
import TradeWar.Model
import TradeWar.Strategy
import TradeWar.Simulation
import TradeWar.StrategyCombinators

spec :: Spec
spec = do
    describe "Strategy Combinators" $ do
        it "can construct a tit-for-tat strategy using combinators" $ do
            let us = createTestCountry "US"
                canada = createTestCountry "Canada"
                initialTariffs = M.empty
                scenario = TradeWarScenario [us, canada] initialTariffs
                state = initializeGameState scenario
                
                -- Create a state with a history of actions
                stateWithHistory = addActionHistory canada (ImposeTariff (head $ countryIndustries us) 10.0) state
                
                -- Traditional tit-for-tat strategy
                traditionalAction = titForTatStrategy us [canada] stateWithHistory
                
                -- Combinator-based tit-for-tat strategy
                combinatorAction = mirrorLastAction us [canada] stateWithHistory
                
            -- The actions should be identical
            combinatorAction `shouldBe` traditionalAction
            
        it "can handle empty opponent list in both implementations" $ do
            let us = createTestCountry "US"
                state = initializeGameState (TradeWarScenario [us] M.empty)
                
                -- Both strategies should return NoAction when there are no opponents
                traditionalAction = titForTatStrategy us [] state
                combinatorAction = mirrorLastAction us [] state
                
            traditionalAction `shouldBe` NoAction
            combinatorAction `shouldBe` NoAction
            
        it "can handle no previous action in both implementations" $ do
            let us = createTestCountry "US"
                canada = createTestCountry "Canada"
                state = initializeGameState (TradeWarScenario [us, canada] M.empty)
                
                -- Both strategies should return NoAction when there's no previous action
                traditionalAction = titForTatStrategy us [canada] state
                combinatorAction = mirrorLastAction us [canada] state
                
            traditionalAction `shouldBe` NoAction
            combinatorAction `shouldBe` NoAction
            
        it "can construct complex strategies using combinators" $ do
            let us = createTestCountry "US"
                canada = createTestCountry "Canada"
                state = initializeGameState (TradeWarScenario [us, canada] M.empty)
                
                -- Create a complex strategy: if opponent has imposed tariffs, retaliate,
                -- otherwise cooperate by negotiating
                hasImposedTariffs self opponents gameState = 
                    case opponents of
                        [] -> False
                        opponent:_ -> 
                            case getLastAction opponent gameState of
                                Just (ImposeTariff _ _) -> True
                                _ -> False
                
                complexStrategy = ifThenElse 
                    hasImposedTariffs 
                    retaliateAgainst 
                    (negotiateWith canada)
                
                -- Test with different previous actions
                stateWithTariff = addActionHistory canada (ImposeTariff (head $ countryIndustries us) 10.0) state
                stateWithNegotiation = addActionHistory canada (NegotiateTrade us) state
                
                actionAfterTariff = complexStrategy us [canada] stateWithTariff
                actionAfterNegotiation = complexStrategy us [canada] stateWithNegotiation
                
            -- After tariff, should retaliate
            case actionAfterTariff of
                ImposeTariff _ _ -> True `shouldBe` True
                _ -> expectationFailure "Should have retaliated with a tariff"
                
            -- After negotiation, should negotiate back
            actionAfterNegotiation `shouldBe` NegotiateTrade canada
            
    describe "Randomized Strategy Combinators" $ do
        it "can use randomChoiceR to select a strategy randomly" $ do
            let us = createTestCountry "US"
                canada = createTestCountry "Canada"
                state = initializeGameState (TradeWarScenario [us, canada] M.empty)
                
                -- Create a list of strategies
                strategies = [
                    imposeTariffOn (head $ countryIndustries canada) 10.0,
                    removeTariffOn (head $ countryIndustries canada),
                    negotiateWith canada,
                    doNothing
                    ]
                
                -- Use a fixed seed for reproducible tests
                seed = 42
                
                -- Run the randomized strategy with the fixed seed
                randomAction = evalRand (randomChoiceR strategies us [canada] state) (mkStdGen seed)
                
            -- With seed 42, we should get a specific action (depends on the random generator)
            -- We're just checking that it returns one of our strategies
            randomAction `shouldSatisfy` (`elem` [s us [canada] state | s <- strategies])
            
        it "can use weightedR to select a strategy based on weights" $ do
            let us = createTestCountry "US"
                canada = createTestCountry "Canada"
                state = initializeGameState (TradeWarScenario [us, canada] M.empty)
                
                -- Create weighted strategies
                weightedStrategies = [
                    (0.7, imposeTariffOn (head $ countryIndustries canada) 10.0),
                    (0.2, removeTariffOn (head $ countryIndustries canada)),
                    (0.1, negotiateWith canada)
                    ]
                
                -- Use a fixed seed for reproducible tests
                seed = 42
                
                -- Run the weighted strategy with the fixed seed
                weightedAction = evalRand (weightedR weightedStrategies us [canada] state) (mkStdGen seed)
                
            -- With seed 42, we should get a specific action (depends on the random generator)
            -- We're just checking that it returns one of our strategies
            weightedAction `shouldSatisfy` (`elem` [s us [canada] state | (_, s) <- weightedStrategies])
            
        it "can use withProbabilityR to apply a strategy with a certain probability" $ do
            let us = createTestCountry "US"
                canada = createTestCountry "Canada"
                state = initializeGameState (TradeWarScenario [us, canada] M.empty)
                
                -- Create a strategy with 80% probability
                strategy = imposeTariffOn (head $ countryIndustries canada) 10.0
                probabilityStrategy = withProbabilityR 0.8 strategy
                
                -- Use a fixed seed for reproducible tests
                seed = 42
                
                -- Run the probabilistic strategy with the fixed seed
                probAction = evalRand (probabilityStrategy us [canada] state) (mkStdGen seed)
                
            -- With seed 42 and 80% probability, we should get the strategy applied
            -- (or not, depending on the random generator)
            probAction `shouldSatisfy` (`elem` [strategy us [canada] state, NoAction])

-- Helper function to create a test country
createTestCountry :: String -> Country
createTestCountry name = createCountry (pack name) 1000.0 2.0 2.0 5.0
    [ createIndustry (pack "Industry1") 10.0 0.5 0.3
    , createIndustry (pack "Industry2") 20.0 0.3 0.4
    ]

-- Helper function to add an action to a country's history
addActionHistory :: Country -> TradeAction -> GameState -> GameState
addActionHistory country action state =
    let currentHistory = M.findWithDefault [] country (gameHistory state)
        newHistory = M.insert country (action : currentHistory) (gameHistory state)
    in state { gameHistory = newHistory } 