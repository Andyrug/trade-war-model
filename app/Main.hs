module Main where

import qualified Data.Map as M
import Data.Text (Text, pack)
import Data.Maybe (fromMaybe)

import TradeWar.Types
import TradeWar.Model
import TradeWar.Strategy
import TradeWar.Equilibrium
import TradeWar.Simulation

-- | Main entry point for the trade war simulation
main :: IO ()
main = do
    putStrLn "Trade War Simulation"
    putStrLn "===================="
    
    -- Create initial scenario
    let scenario = createUSCanadaScenario
    
    -- Initialize game state
    let initialState = initializeGameState scenario
    
    -- Create strategy map with tit-for-tat for all countries
    let strategies = M.fromList [(c, titForTatStrategy) | c <- scenarioCountries scenario]
    
    -- Run simulation with tit-for-tat strategy
    let result = runSimulationWithStrategies initialState strategies 5
    
    -- Display results
    displayInitialState (head $ simStates result)
    displayFinalState (last $ simStates result)
    
    -- Find equilibria
    let nashEquilibria = findNashEquilibrium (last $ simStates result)
    displayEquilibria "Nash Equilibria" nashEquilibria
    
    let paretoOptimal = findParetoOptimal (last $ simStates result)
    displayEquilibria "Pareto Optimal Outcomes" paretoOptimal
    
    -- Find optimal joint strategy
    let (optimalJointStrategy, totalWelfare) = findOptimalJointStrategy (last $ simStates result)
    putStrLn $ "\nOptimal Joint Strategy (Total Welfare: " ++ show totalWelfare ++ "):"
    displayStrategy optimalJointStrategy

-- | Create a scenario modeling the US-Canada trade war
createUSCanadaScenario :: TradeWarScenario
createUSCanadaScenario = 
    let -- Define industries
        steel = Industry (pack "Steel") 1.5 0.3 0.2
        aluminum = Industry (pack "Aluminum") 0.8 0.4 0.3
        dairy = Industry (pack "Dairy") 2.0 0.1 0.05
        lumber = Industry (pack "Lumber") 1.2 0.5 0.2
        
        -- Define countries
        us = Country 
            { countryName = pack "United States"
            , countryEconomy = Economy 21400 2.3 2.1 3.9
            , countryIndustries = [steel, aluminum, dairy]
            }
        
        canada = Country
            { countryName = pack "Canada"
            , countryEconomy = Economy 1700 1.8 1.9 5.8
            , countryIndustries = [steel, aluminum, dairy, lumber]
            }
        
        -- Define initial tariffs
        usTariffs = [Tariff steel 25.0 0.2, Tariff aluminum 10.0 0.1]
        canadaTariffs = [Tariff dairy 270.0 0.3]
        
        -- Create the scenario
        initialTariffs = M.fromList [((us, canada), usTariffs), ((canada, us), canadaTariffs)]
    in TradeWarScenario [us, canada] initialTariffs

-- | Display the initial state of the simulation
displayInitialState :: GameState -> IO ()
displayInitialState state = do
    putStrLn "\nInitial State:"
    displayState state

-- | Display the final state of the simulation
displayFinalState :: GameState -> IO ()
displayFinalState state = do
    putStrLn "\nFinal State (after 5 rounds):"
    displayState state

-- | Display a game state
displayState :: GameState -> IO ()
displayState state = do
    putStrLn $ "Round: " ++ show (gameRound state)
    
    -- Display country information
    forM_ (gameCountries state) $ \country -> do
        let economy = fromMaybe (countryEconomy country) (M.lookup country (gameEconomies state))
        putStrLn $ "Country: " ++ show (countryName country)
        putStrLn $ "  GDP: $" ++ show (economyGDP economy) ++ " billion"
        putStrLn $ "  Growth Rate: " ++ show (economyGrowthRate economy) ++ "%"
        putStrLn $ "  Unemployment: " ++ show (economyUnemploymentRate economy) ++ "%"
    
    -- Display tariffs
    putStrLn "\nTariffs:"
    forM_ (M.toList (gameTariffs state)) $ \((from, to), tariffs) -> do
        putStrLn $ show (countryName from) ++ " -> " ++ show (countryName to) ++ ":"
        forM_ tariffs $ \tariff -> do
            putStrLn $ "  " ++ show (industryName (tariffIndustry tariff)) ++ 
                       ": " ++ show (tariffRate tariff) ++ "%"

-- | Display equilibria
displayEquilibria :: String -> [(M.Map Country TradeAction, M.Map Country Double)] -> IO ()
displayEquilibria title equilibria = do
    putStrLn $ "\n" ++ title ++ " (" ++ show (length equilibria) ++ " found):"
    forM_ (zip [1..] equilibria) $ \(i, (actions, payoffs)) -> do
        putStrLn $ "Equilibrium " ++ show i ++ ":"
        displayStrategy actions
        putStrLn "Payoffs:"
        forM_ (M.toList payoffs) $ \(country, payoff) -> do
            putStrLn $ "  " ++ show (countryName country) ++ ": " ++ show payoff

-- | Display a strategy
displayStrategy :: M.Map Country TradeAction -> IO ()
displayStrategy strategy = do
    putStrLn "Strategy:"
    forM_ (M.toList strategy) $ \(country, action) -> do
        putStrLn $ "  " ++ show (countryName country) ++ ": " ++ show action

-- | Helper function for forM_ since it's not imported
forM_ :: Monad m => [a] -> (a -> m b) -> m ()
forM_ xs f = sequence_ (map f xs)

{- 
TODO: Web UI Development Plan

1. Frontend (React/TypeScript):
   - Country configuration form
   - Trade relation setup
   - Strategy builder interface
   - Simulation controls
   - Results visualization with charts
   - Scenario saving/loading

2. Backend (Haskell with Servant):
   - API endpoints for:
     - Running simulations
     - Calculating equilibria
     - Fetching real-world economic data
     - Saving/loading scenarios
   - Integration with our existing model

3. Data Visualization:
   - Time series charts for economic indicators
   - Network graphs for trade relations
   - Heatmaps for payoff matrices
   - Comparative bar charts for strategy outcomes

4. Real-time Data Integration:
   - APIs for economic data (World Bank, IMF, etc.)
   - News sentiment analysis for trade relations
   - Automated scenario generation based on current events

5. Strategy Combinator System:
   - Basic strategy primitives
   - Combinators for sequencing, alternating, and conditional strategies
   - Strategy visualization and explanation
-} 