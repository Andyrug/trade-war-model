module TradeWar.Strategy
    ( titForTatStrategy
    , aggressiveStrategy
    , defensiveStrategy
    , negotiationStrategy
    , optimalStrategy
    , getLastAction
    , getCountryTariffs
    , generatePossibleActions
    ) where

import qualified Data.Map as M
import Data.Maybe (fromMaybe, listToMaybe)
import Data.List (maximumBy)
import Data.Ord (comparing)

import TradeWar.Types
import TradeWar.Model (calculatePayoff)

-- | Tit-for-tat strategy: respond in kind to the opponent's last action
titForTatStrategy :: Country -> [Country] -> GameState -> TradeAction
titForTatStrategy self opponents state =
    case opponents of
        [] -> NoAction
        opponent:_ -> 
            let lastAction = getLastAction opponent state
            in mirrorAction self opponent lastAction

-- | Mirror an action taken by an opponent
mirrorAction :: Country -> Country -> Maybe TradeAction -> TradeAction
mirrorAction self opponent Nothing = NoAction
mirrorAction self opponent (Just action) = case action of
    ImposeTariff industry rate -> 
        -- Respond with a similar tariff
        ImposeTariff industry rate
    
    RemoveTariff industry -> 
        -- Reciprocate by removing a tariff
        RemoveTariff industry
    
    NegotiateTrade target -> 
        if target == self
        then NegotiateTrade opponent
        else NoAction
    
    NoAction -> NoAction

-- | Aggressive strategy: always impose tariffs on the most vulnerable industry
aggressiveStrategy :: Country -> [Country] -> GameState -> TradeAction
aggressiveStrategy self opponents state =
    case opponents of
        [] -> NoAction
        opponent:_ -> 
            case findVulnerableIndustry opponent of
                Nothing -> NoAction
                Just industry -> ImposeTariff industry 25.0  -- 25% tariff
  where
    findVulnerableIndustry :: Country -> Maybe Industry
    findVulnerableIndustry country = 
        listToMaybe $ 
        sortByVulnerability $ 
        countryIndustries country
    
    sortByVulnerability :: [Industry] -> [Industry]
    sortByVulnerability = 
        reverse . 
        sortByExportDependence
    
    sortByExportDependence :: [Industry] -> [Industry]
    sortByExportDependence = 
        sortBy (comparing industryExportDependence)
    
    sortBy :: (a -> a -> Ordering) -> [a] -> [a]
    sortBy f = foldr (insertBy f) []
    
    insertBy :: (a -> a -> Ordering) -> a -> [a] -> [a]
    insertBy _ x [] = [x]
    insertBy f x (y:ys)
        | f x y == GT = y : insertBy f x ys
        | otherwise   = x : y : ys

-- | Defensive strategy: remove tariffs to protect domestic economy
defensiveStrategy :: Country -> [Country] -> GameState -> TradeAction
defensiveStrategy self opponents state =
    let economy = fromMaybe (countryEconomy self) (M.lookup self (gameEconomies state))
        -- If growth is negative, try to remove tariffs
        shouldRemoveTariffs = economyGrowthRate economy < 0
    in
    if shouldRemoveTariffs
    then case getOwnTariffs self opponents state of
        [] -> NoAction
        (tariff:_) -> RemoveTariff (tariffIndustry tariff)
    else NoAction

-- | Negotiation strategy: prefer negotiation over tariffs
negotiationStrategy :: Country -> [Country] -> GameState -> TradeAction
negotiationStrategy self opponents state =
    case opponents of
        [] -> NoAction
        opponent:_ -> 
            let relationshipScore = calculateRelationshipScore self opponent state
            in if relationshipScore < 0
               then NegotiateTrade opponent
               else NoAction

-- | Calculate a score representing the relationship between two countries
calculateRelationshipScore :: Country -> Country -> GameState -> Double
calculateRelationshipScore country1 country2 state =
    let tariffs1to2 = getCountryTariffs country1 country2 state
        tariffs2to1 = getCountryTariffs country2 country1 state
        tariffImpact1to2 = sum [tariffImpact t | t <- tariffs1to2]
        tariffImpact2to1 = sum [tariffImpact t | t <- tariffs2to1]
    in tariffImpact1to2 - tariffImpact2to1  -- Positive means country1 is "winning"

-- | Optimal strategy: choose the action that maximizes payoff
optimalStrategy :: Country -> [Country] -> GameState -> TradeAction
optimalStrategy self opponents state =
    let possibleActions = generatePossibleActions self opponents state
        actionPayoffs = [(action, evaluateAction self action opponents state) | action <- possibleActions]
    in fst $ maximumBy (comparing snd) actionPayoffs

-- | Generate all possible actions for a country
generatePossibleActions :: Country -> [Country] -> GameState -> [TradeAction]
generatePossibleActions self opponents state =
    NoAction :  -- Always include the option to do nothing
    [ImposeTariff industry rate | 
        opponent <- opponents,
        industry <- countryIndustries opponent,
        rate <- [10.0, 20.0, 30.0]] ++
    [RemoveTariff (tariffIndustry tariff) | 
        opponent <- opponents,
        tariff <- getCountryTariffs self opponent state] ++
    [NegotiateTrade opponent | 
        opponent <- opponents]

-- | Evaluate the expected payoff of an action
evaluateAction :: Country -> TradeAction -> [Country] -> GameState -> Double
evaluateAction self action opponents state =
    let -- Simulate the action
        newState = applyActionSimulation self action opponents state
        -- Calculate the payoff in the new state
        payoff = calculatePayoff self newState
    in payoff

-- | Simulate applying an action without modifying the original state
applyActionSimulation :: Country -> TradeAction -> [Country] -> GameState -> GameState
applyActionSimulation self action opponents state =
    -- This is a simplified simulation that doesn't account for opponent responses
    -- In a real implementation, you would model opponent responses more accurately
    case action of
        ImposeTariff industry rate -> 
            let impact = rate / 100 * industryExportDependence industry * industryGDP industry / 100
                -- Reduce opponent economies
                newEconomies = foldr (\opponent acc -> 
                                        M.adjust (\e -> e { economyGrowthRate = economyGrowthRate e - impact }) 
                                                opponent acc)
                                    (gameEconomies state) opponents
            in state { gameEconomies = newEconomies }
        
        RemoveTariff _ -> 
            -- Simplified: removing a tariff slightly improves relations
            let newEconomies = foldr (\opponent acc -> 
                                        M.adjust (\e -> e { economyGrowthRate = economyGrowthRate e + 0.1 }) 
                                                opponent acc)
                                    (gameEconomies state) opponents
            in state { gameEconomies = newEconomies }
        
        NegotiateTrade _ -> 
            -- Negotiation improves relations significantly
            let newEconomies = foldr (\opponent acc -> 
                                        M.adjust (\e -> e { economyGrowthRate = economyGrowthRate e + 0.5 }) 
                                                opponent acc)
                                    (gameEconomies state) opponents
            in state { gameEconomies = newEconomies }
        
        NoAction -> state

-- | Get the last action taken by a country
getLastAction :: Country -> GameState -> Maybe TradeAction
getLastAction country state =
    listToMaybe =<< M.lookup country (gameHistory state)

-- | Get tariffs imposed by one country on another
getCountryTariffs :: Country -> Country -> GameState -> [Tariff]
getCountryTariffs from to state =
    fromMaybe [] (M.lookup (from, to) (gameTariffs state))

-- | Get tariffs imposed by a country on all opponents
getOwnTariffs :: Country -> [Country] -> GameState -> [Tariff]
getOwnTariffs self opponents state =
    concatMap (\opponent -> getCountryTariffs self opponent state) opponents 