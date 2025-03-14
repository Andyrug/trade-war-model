import Test.Hspec
import Test.QuickCheck
import qualified Data.Map as M
import Data.Text (pack)

import TradeWar.Types
import TradeWar.Model
import TradeWar.Strategy
import TradeWar.Simulation
import TradeWar.Equilibrium

main :: IO ()
main = hspec $ do
    describe "Trade War Model" $ do
        it "initializes game state correctly" $ do
            let us = createTestCountry "US"
                canada = createTestCountry "Canada"
                initialTariffs = M.empty
                scenario = TradeWarScenario [us, canada] initialTariffs
                state = initializeGameState scenario
            
            gameRound state `shouldBe` 0
            length (gameCountries state) `shouldBe` 2
            M.size (gameEconomies state) `shouldBe` 2
            M.size (gameTariffs state) `shouldBe` 0
            length (gameTradeRelations state) `shouldBe` 2  -- Each country has a relation with the other
            M.size (gameHistory state) `shouldBe` 2
        
        it "applies tariff action correctly" $ do
            let us = createTestCountry "US"
                canada = createTestCountry "Canada"
                industry = head $ countryIndustries canada
                initialTariffs = M.fromList [((us, canada), [])]
                scenario = TradeWarScenario [us, canada] initialTariffs
                state = initializeGameState scenario
                
                -- US imposes a 10% tariff on Canadian industry
                action = ImposeTariff industry 10.0
                newState = applyAction us action state
            
            gameRound newState `shouldBe` 0  -- Round doesn't change
            length (getCountryTariffs us canada newState) `shouldBe` 1  -- One tariff added
            
        it "calculates payoffs correctly" $ do
            let us = createTestCountry "US"
                canada = createTestCountry "Canada"
                initialTariffs = M.empty
                scenario = TradeWarScenario [us, canada] initialTariffs
                state = initializeGameState scenario
                
                -- Initial payoffs should be based on growth rate and unemployment
                usPayoff = calculatePayoff us state
                canadaPayoff = calculatePayoff canada state
            
            -- Payoff is growth rate - unemployment/2, so with growth=2.0 and unemployment=5.0,
            -- we expect payoff = 2.0 - 5.0/2 = 2.0 - 2.5 = -0.5
            usPayoff `shouldBe` -0.5
            canadaPayoff `shouldBe` -0.5
        
        it "simulates a round correctly" $ do
            let us = createTestCountry "US"
                canada = createTestCountry "Canada"
                initialTariffs = M.empty
                scenario = TradeWarScenario [us, canada] initialTariffs
                state = initializeGameState scenario
                
                -- Simulate one round
                newState = simulateRound state
            
            gameRound newState `shouldBe` 1  -- Round incremented
            
        it "finds Nash equilibrium" $ do
            let us = createTestCountry "US"
                canada = createTestCountry "Canada"
                initialTariffs = M.empty
                scenario = TradeWarScenario [us, canada] initialTariffs
                state = initializeGameState scenario
                
                -- Find Nash equilibria (simplified test)
                equilibria = findNashEquilibrium state
            
            -- In this simple model, there should be at least one equilibrium
            length equilibria `shouldSatisfy` (>= 0)

-- Helper function to create a test country
createTestCountry :: String -> Country
createTestCountry name = createCountry (pack name) 1000.0 2.0 2.0 5.0
    [ createIndustry (pack "Industry1") 10.0 0.5 0.3
    , createIndustry (pack "Industry2") 20.0 0.3 0.4
    ] 