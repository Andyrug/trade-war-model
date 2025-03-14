module TradeWar.Equilibrium
    ( findNashEquilibrium
    , findParetoOptimal
    , findEquilibrium
    , calculatePayoffMatrix
    , isNashEquilibrium
    , isParetoOptimal
    , findOptimalJointStrategy
    ) where

import qualified Data.Map as M
import Data.List (maximumBy, nub)
import Data.Ord (comparing)
import Data.Maybe (fromMaybe)

import TradeWar.Types
import TradeWar.Model (applyAction, calculatePayoff)
import TradeWar.Strategy (generatePossibleActions)

-- | Find Nash equilibria in the current game state
findNashEquilibrium :: GameState -> [(M.Map Country TradeAction, M.Map Country Double)]
findNashEquilibrium state =
    let countries = gameCountries state
        -- Generate all possible action combinations
        actionCombinations = generateActionCombinations countries state
        -- Calculate payoffs for each combination
        payoffs = [(actions, calculatePayoffs actions state) | actions <- actionCombinations]
        -- Filter for Nash equilibria
        nashEquilibria = filter (isNashEquilibrium state) payoffs
    in nashEquilibria

-- | Find Pareto optimal outcomes in the current game state
findParetoOptimal :: GameState -> [(M.Map Country TradeAction, M.Map Country Double)]
findParetoOptimal state =
    let countries = gameCountries state
        -- Generate all possible action combinations
        actionCombinations = generateActionCombinations countries state
        -- Calculate payoffs for each combination
        payoffs = [(actions, calculatePayoffs actions state) | actions <- actionCombinations]
        -- Filter for Pareto optimal outcomes
        paretoOptimal = filter (isParetoOptimal payoffs) payoffs
    in paretoOptimal

-- | Find equilibrium points that are both Nash equilibria and Pareto optimal
findEquilibrium :: GameState -> [(M.Map Country TradeAction, M.Map Country Double)]
findEquilibrium state =
    let nashEquilibria = findNashEquilibrium state
        paretoOptimal = findParetoOptimal state
        -- Find the intersection
        equilibria = filter (\(actions, _) -> any (\(actions', _) -> actions == actions') nashEquilibria) paretoOptimal
    in equilibria

-- | Calculate a payoff matrix for all possible action combinations
calculatePayoffMatrix :: GameState -> M.Map (M.Map Country TradeAction) (M.Map Country Double)
calculatePayoffMatrix state =
    let countries = gameCountries state
        -- Generate all possible action combinations
        actionCombinations = generateActionCombinations countries state
        -- Calculate payoffs for each combination
        payoffs = [(actions, calculatePayoffs actions state) | actions <- actionCombinations]
    in M.fromList payoffs

-- | Check if a given action combination is a Nash equilibrium
isNashEquilibrium :: GameState -> (M.Map Country TradeAction, M.Map Country Double) -> Bool
isNashEquilibrium state (actions, payoffs) =
    let countries = gameCountries state
    in all (isCountryBestResponse state actions payoffs) countries

-- | Check if a country's action is a best response to other countries' actions
isCountryBestResponse :: GameState -> M.Map Country TradeAction -> M.Map Country Double -> Country -> Bool
isCountryBestResponse state actions payoffs country =
    let currentAction = fromMaybe NoAction (M.lookup country actions)
        currentPayoff = fromMaybe 0 (M.lookup country payoffs)
        
        -- Generate all possible actions for this country
        possibleActions = generatePossibleActions country (filter (/= country) (gameCountries state)) state
        
        -- Calculate payoffs for each alternative action
        alternativePayoffs = [calculateAlternativePayoff state actions country action | action <- possibleActions]
        
        -- Check if current payoff is at least as good as all alternatives
        isBestResponse = all (currentPayoff >=) alternativePayoffs
    in isBestResponse

-- | Calculate the payoff for a country if it changes its action
calculateAlternativePayoff :: GameState -> M.Map Country TradeAction -> Country -> TradeAction -> Double
calculateAlternativePayoff state actions country alternativeAction =
    let -- Create a new action map with the alternative action
        newActions = M.insert country alternativeAction actions
        
        -- Apply all actions to get the new state
        newState = foldl (\s (c, a) -> applyAction c a s) state (M.toList newActions)
        
        -- Calculate the payoff for the country in the new state
        payoff = calculatePayoff country newState
    in payoff

-- | Check if an outcome is Pareto optimal
isParetoOptimal :: [(M.Map Country TradeAction, M.Map Country Double)] -> (M.Map Country TradeAction, M.Map Country Double) -> Bool
isParetoOptimal allPayoffs (actions, payoffs) =
    not $ any (dominates payoffs) [p | (a, p) <- allPayoffs, a /= actions]

-- | Check if one payoff vector dominates another
dominates :: M.Map Country Double -> M.Map Country Double -> Bool
dominates p1 p2 =
    let countries = nub (M.keys p1 ++ M.keys p2)
        -- Check if p2 is at least as good as p1 for all countries
        allAtLeastAsGood = all (\c -> fromMaybe 0 (M.lookup c p2) >= fromMaybe 0 (M.lookup c p1)) countries
        -- Check if p2 is strictly better for at least one country
        someStrictlyBetter = any (\c -> fromMaybe 0 (M.lookup c p2) > fromMaybe 0 (M.lookup c p1)) countries
    in allAtLeastAsGood && someStrictlyBetter

-- | Generate all possible combinations of actions for all countries
generateActionCombinations :: [Country] -> GameState -> [M.Map Country TradeAction]
generateActionCombinations [] _ = [M.empty]
generateActionCombinations (country:rest) state =
    let -- Generate all possible actions for this country
        possibleActions = generatePossibleActions country (filter (/= country) (gameCountries state)) state
        -- Generate all combinations for the rest of the countries
        restCombinations = generateActionCombinations rest state
        -- Combine this country's actions with all combinations for the rest
        allCombinations = [M.insert country action combination | 
                           action <- possibleActions, 
                           combination <- restCombinations]
    in allCombinations

-- | Calculate payoffs for all countries given a set of actions
calculatePayoffs :: M.Map Country TradeAction -> GameState -> M.Map Country Double
calculatePayoffs actions state =
    let -- Apply all actions to get the new state
        newState = foldl (\s (country, action) -> applyAction country action s) state (M.toList actions)
        -- Calculate payoffs for each country
        countries = gameCountries state
        payoffs = [(country, calculatePayoff country newState) | country <- countries]
    in M.fromList payoffs

-- | Find the optimal joint strategy that maximizes total welfare
findOptimalJointStrategy :: GameState -> (M.Map Country TradeAction, Double)
findOptimalJointStrategy state =
    let countries = gameCountries state
        -- Generate all possible action combinations
        actionCombinations = generateActionCombinations countries state
        -- Calculate total payoff for each combination
        totalPayoffs = [(actions, sum . M.elems $ calculatePayoffs actions state) | actions <- actionCombinations]
        -- Find the combination with the highest total payoff
        optimalStrategy = maximumBy (comparing snd) totalPayoffs
    in optimalStrategy 