module TradeWar.Simulation
    ( runSimulation
    , runSimulationWithStrategies
    , simulateRound
    , simulateRoundWithStrategies
    ) where

import qualified Data.Map as M
import Data.Maybe (fromMaybe)

import TradeWar.Types
import TradeWar.Model (initializeGameState, applyAction)
import TradeWar.Strategy (titForTatStrategy)

-- | Run a simulation for a specified number of rounds
runSimulation :: TradeWarScenario -> Int -> SimulationResult
runSimulation scenario rounds =
    let initialState = initializeGameState scenario
        -- Default to tit-for-tat strategy for all countries
        strategies = M.fromList [(c, titForTatStrategy) | c <- scenarioCountries scenario]
    in runSimulationWithStrategies initialState strategies rounds

-- | Run a simulation with specified strategies for each country
runSimulationWithStrategies :: GameState -> M.Map Country (Country -> [Country] -> GameState -> TradeAction) -> Int -> SimulationResult
runSimulationWithStrategies initialState strategies rounds =
    let finalState = iterate (simulateRoundWithStrategies strategies) initialState !! rounds
        states = take (rounds + 1) $ iterate (simulateRoundWithStrategies strategies) initialState
    in SimulationResult
        { simStates = states
        , simFinalEconomies = gameEconomies finalState
        , simActions = gameHistory finalState
        }

-- | Simulate a single round of the trade war
simulateRound :: GameState -> GameState
simulateRound state =
    let countries = gameCountries state
        -- Default to tit-for-tat strategy for all countries
        strategies = M.fromList [(c, titForTatStrategy) | c <- countries]
    in simulateRoundWithStrategies strategies state

-- | Simulate a single round with specified strategies
simulateRoundWithStrategies :: M.Map Country (Country -> [Country] -> GameState -> TradeAction) -> GameState -> GameState
simulateRoundWithStrategies strategies state =
    let countries = gameCountries state
        -- For each country, determine its action based on its strategy
        countryActions = [(country, determineAction country strategies state) | country <- countries]
        -- Apply all actions to get the new state
        newState = foldl (\s (country, action) -> applyAction country action s) 
                         (state { gameRound = gameRound state + 1 }) 
                         countryActions
    in newState

-- | Determine the action for a country based on its strategy
determineAction :: Country -> M.Map Country (Country -> [Country] -> GameState -> TradeAction) -> GameState -> TradeAction
determineAction country strategies state =
    let strategy = fromMaybe titForTatStrategy (M.lookup country strategies)
        opponents = filter (/= country) (gameCountries state)
    in strategy country opponents state 