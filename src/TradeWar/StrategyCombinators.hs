{-# LANGUAGE RankNTypes #-}
module TradeWar.StrategyCombinators
    ( -- * Strategy combinators
      always
    , when
    , unless
    , ifThenElse
    , sequential
    , alternate
    , randomChoice
    , weighted
    , withProbability
    , compose
    , limit
    , times
    , untilPayoff
    , bestOf
    , worstOf
    , averageOf
    
    -- * Strategy modifiers
    , withMemory
    , withLookAhead
    , withPayoffThreshold
    , withCountryFilter
    
    -- * Strategy primitives
    , imposeTariffOn
    , removeTariffOn
    , negotiateWith
    , doNothing
    , mirrorLastAction
    , retaliateAgainst
    , cooperateWith
    
    -- * Random strategy helpers
    , RandomizedStrategyFn
    , randomChoiceR
    , weightedR
    , withProbabilityR
    , runRandomizedStrategy
    , liftStrategy
    ) where

import qualified Data.Map as M
import Data.Maybe (fromMaybe, listToMaybe)
import Data.List (maximumBy, minimumBy)
import Data.Ord (comparing)
import qualified Control.Monad.Random as MR

import TradeWar.Types
import TradeWar.Model (calculatePayoff)
import TradeWar.Strategy (getLastAction, generatePossibleActions)

-- | Type alias for a pure strategy function
type StrategyFn = Country -> [Country] -> GameState -> TradeAction

-- | Type alias for a randomized strategy function
type RandomizedStrategyFn = forall m. MR.MonadRandom m => Country -> [Country] -> GameState -> m TradeAction

-- | Type alias for a strategy predicate
type StrategyPredicate = Country -> [Country] -> GameState -> Bool

-- | Run a randomized strategy with a default random seed
runRandomizedStrategy :: RandomizedStrategyFn -> Country -> [Country] -> GameState -> TradeAction
runRandomizedStrategy randomStrategy self opponents state =
    -- Use a fixed seed (42) for reproducible results
    -- In a real application, you might want to use a different seed or IO
    MR.evalRand (randomStrategy self opponents state) (MR.mkStdGen 42)

-- | Lift a pure strategy to a randomized strategy
liftStrategy :: StrategyFn -> RandomizedStrategyFn
liftStrategy strategy self opponents state = 
    return $ strategy self opponents state

-- | Always use a specific action regardless of state
always :: TradeAction -> StrategyFn
always action _ _ _ = action

-- | Apply a strategy only when a condition is true, otherwise do nothing
when :: StrategyPredicate -> StrategyFn -> StrategyFn
when predicate strategy self opponents state =
    if predicate self opponents state
    then strategy self opponents state
    else NoAction

-- | Apply a strategy only when a condition is false, otherwise do nothing
unless :: StrategyPredicate -> StrategyFn -> StrategyFn
unless predicate strategy self opponents state =
    if not (predicate self opponents state)
    then strategy self opponents state
    else NoAction

-- | If-then-else combinator for strategies
ifThenElse :: StrategyPredicate -> StrategyFn -> StrategyFn -> StrategyFn
ifThenElse predicate thenStrategy elseStrategy self opponents state =
    if predicate self opponents state
    then thenStrategy self opponents state
    else elseStrategy self opponents state

-- | Apply strategies in sequence, using the first one that doesn't return NoAction
sequential :: [StrategyFn] -> StrategyFn
sequential strategies self opponents state =
    case dropWhile (\s -> s self opponents state == NoAction) strategies of
        [] -> NoAction
        (s:_) -> s self opponents state

-- | Alternate between strategies based on the game round
alternate :: [StrategyFn] -> StrategyFn
alternate strategies self opponents state =
    let gameRoundNum = gameRound state
        index = gameRoundNum `mod` length strategies
    in (strategies !! index) self opponents state

-- | Choose a strategy randomly from a list (pure version - uses first strategy)
randomChoice :: [StrategyFn] -> StrategyFn
randomChoice strategies self opponents state =
    if null strategies
    then NoAction
    else (head strategies) self opponents state

-- | Choose a strategy randomly from a list (randomized version)
randomChoiceR :: [StrategyFn] -> RandomizedStrategyFn
randomChoiceR strategies self opponents state = do
    if null strategies
    then return NoAction
    else do
        index <- MR.getRandomR (0, length strategies - 1)
        return $ (strategies !! index) self opponents state

-- | Choose a strategy with weighted probability (pure version - uses first strategy)
weighted :: [(Double, StrategyFn)] -> StrategyFn
weighted weightedStrategies self opponents state =
    if null weightedStrategies
    then NoAction
    else (snd $ head weightedStrategies) self opponents state

-- | Choose a strategy with weighted probability (randomized version)
weightedR :: [(Double, StrategyFn)] -> RandomizedStrategyFn
weightedR weightedStrategies self opponents state = do
    if null weightedStrategies
    then return NoAction
    else do
        -- Implement our own weighted selection
        let strategies = map snd weightedStrategies
            weights = map fst weightedStrategies
            totalWeight = sum weights
        r <- MR.getRandomR (0, totalWeight)
        return $ selectByWeight r (zip weights strategies) self opponents state
  where
    -- Helper function to select a strategy based on weight
    selectByWeight :: Double -> [(Double, StrategyFn)] -> Country -> [Country] -> GameState -> TradeAction
    selectByWeight _ [] _ _ _ = NoAction
    selectByWeight r ((w, s):rest) self opponents state
        | r <= w = s self opponents state
        | otherwise = selectByWeight (r - w) rest self opponents state

-- | Apply a strategy with a certain probability, otherwise do nothing (pure version)
withProbability :: Double -> StrategyFn -> StrategyFn
withProbability prob strategy self opponents state =
    if prob > 0.5  -- Simplified condition
    then strategy self opponents state
    else NoAction

-- | Apply a strategy with a certain probability, otherwise do nothing (randomized version)
withProbabilityR :: Double -> StrategyFn -> RandomizedStrategyFn
withProbabilityR prob strategy self opponents state = do
    r <- MR.getRandom
    if r <= prob
    then return $ strategy self opponents state
    else return NoAction

-- | Compose two strategies, applying the second to the result of the first
compose :: StrategyFn -> StrategyFn -> StrategyFn
compose s1 s2 self opponents state =
    let action1 = s1 self opponents state
        -- Apply the first action to get a new state
        newState = applyActionToState self action1 state
    in if action1 == NoAction
       then s2 self opponents state
       else action1

-- | Limit a strategy to a maximum number of applications
limit :: Int -> StrategyFn -> StrategyFn
limit maxUses strategy self opponents state =
    let history = fromMaybe [] $ M.lookup self (gameHistory state)
        usageCount = length history
    in if usageCount < maxUses
       then strategy self opponents state
       else NoAction

-- | Apply a strategy exactly n times, then do nothing
times :: Int -> StrategyFn -> StrategyFn
times n strategy = limit n strategy

-- | Apply a strategy until a payoff threshold is reached
untilPayoff :: Double -> StrategyFn -> StrategyFn
untilPayoff threshold strategy self opponents state =
    let currentPayoff = calculatePayoff self state
    in if currentPayoff < threshold
       then strategy self opponents state
       else NoAction

-- | Choose the best strategy based on expected payoff
bestOf :: [StrategyFn] -> StrategyFn
bestOf strategies self opponents state =
    let actions = [s self opponents state | s <- strategies]
        payoffs = [evaluateAction self action state | action <- actions]
        bestIndex = fst $ maximumBy (comparing snd) $ zip [0..] payoffs
    in if null strategies
       then NoAction
       else (strategies !! bestIndex) self opponents state

-- | Choose the worst strategy based on expected payoff (useful for testing)
worstOf :: [StrategyFn] -> StrategyFn
worstOf strategies self opponents state =
    let actions = [s self opponents state | s <- strategies]
        payoffs = [evaluateAction self action state | action <- actions]
        worstIndex = fst $ minimumBy (comparing snd) $ zip [0..] payoffs
    in if null strategies
       then NoAction
       else (strategies !! worstIndex) self opponents state

-- | Average the payoffs of multiple strategies and choose the action with the highest average
averageOf :: [StrategyFn] -> StrategyFn
averageOf strategies self opponents state =
    let possibleActions = nub $ concat [[s self opponents state] | s <- strategies]
        avgPayoffs = [(action, averagePayoff action) | action <- possibleActions]
        bestAction = fst $ maximumBy (comparing snd) avgPayoffs
    in bestAction
  where
    averagePayoff action =
        let payoff = evaluateAction self action state
        in payoff / fromIntegral (length strategies)

-- | Add memory to a strategy by considering past N states
-- Note: Simplified version that doesn't use memory
-- TODO: Implement memory
withMemory :: Int -> ([GameState] -> StrategyFn) -> StrategyFn
withMemory n strategyWithMemory self opponents state =
    strategyWithMemory [state] self opponents state

-- | Add look-ahead to a strategy by simulating future states
-- Note: Simplified version that doesn't actually look ahead
withLookAhead :: Int -> ([GameState] -> StrategyFn) -> StrategyFn
withLookAhead n strategyWithLookAhead self opponents state =
    strategyWithLookAhead [state] self opponents state

-- | Apply a strategy only if the expected payoff exceeds a threshold
withPayoffThreshold :: Double -> StrategyFn -> StrategyFn
withPayoffThreshold threshold strategy self opponents state =
    let action = strategy self opponents state
        expectedPayoff = evaluateAction self action state
    in if expectedPayoff >= threshold
       then action
       else NoAction

-- | Apply a strategy only to specific countries
withCountryFilter :: (Country -> Bool) -> StrategyFn -> StrategyFn
withCountryFilter predicate strategy self opponents state =
    let filteredOpponents = filter predicate opponents
    in strategy self filteredOpponents state

-- | Strategy primitive: impose a tariff on a specific industry
imposeTariffOn :: Industry -> Double -> StrategyFn
imposeTariffOn industry rate _ _ _ = ImposeTariff industry rate

-- | Strategy primitive: remove a tariff on a specific industry
removeTariffOn :: Industry -> StrategyFn
removeTariffOn industry _ _ _ = RemoveTariff industry

-- | Strategy primitive: negotiate with a specific country
negotiateWith :: Country -> StrategyFn
negotiateWith country _ _ _ = NegotiateTrade country

-- | Strategy primitive: do nothing
doNothing :: StrategyFn
doNothing _ _ _ = NoAction

-- | Strategy primitive: mirror the last action of the opponent
mirrorLastAction :: StrategyFn
mirrorLastAction self (opponent:_) state =
    case getLastAction opponent state of
        Nothing -> NoAction
        Just (ImposeTariff industry rate) -> ImposeTariff industry rate
        Just (RemoveTariff industry) -> RemoveTariff industry
        Just (NegotiateTrade target) -> 
            if target == self then NegotiateTrade opponent else NoAction
        Just NoAction -> NoAction
mirrorLastAction _ [] _ = NoAction

-- | Strategy primitive: retaliate against a country that imposed tariffs
retaliateAgainst :: StrategyFn
retaliateAgainst self (opponent:_) state =
    case getLastAction opponent state of
        Just (ImposeTariff industry rate) -> 
            -- Find an industry to retaliate against
            case listToMaybe (countryIndustries opponent) of
                Nothing -> NoAction
                Just targetIndustry -> ImposeTariff targetIndustry (rate * 1.2)  -- 20% higher
        _ -> NoAction
retaliateAgainst _ [] _ = NoAction

-- | Strategy primitive: cooperate with a country that negotiated or removed tariffs
cooperateWith :: StrategyFn
cooperateWith self (opponent:_) state =
    case getLastAction opponent state of
        Just (NegotiateTrade target) -> 
            if target == self then NegotiateTrade opponent else NoAction
        Just (RemoveTariff _) -> 
            -- Find a tariff to remove in response
            case listToMaybe (getCountryTariffs self opponent state) of
                Nothing -> NoAction
                Just tariff -> RemoveTariff (tariffIndustry tariff)
        _ -> NoAction
cooperateWith _ [] _ = NoAction

-- | Helper function to evaluate the expected payoff of an action
evaluateAction :: Country -> TradeAction -> GameState -> Double
evaluateAction self action state =
    let newState = applyActionToState self action state
    in calculatePayoff self newState

-- | Helper function to apply an action to a state without modifying the original
applyActionToState :: Country -> TradeAction -> GameState -> GameState
applyActionToState self action state =
    -- This is a simplified version that should be replaced with actual logic
    -- from your TradeWar.Model module
    state  -- Placeholder

-- | Helper function to get tariffs imposed by one country on another
getCountryTariffs :: Country -> Country -> GameState -> [Tariff]
getCountryTariffs from to state =
    fromMaybe [] (M.lookup (from, to) (gameTariffs state))

-- | Helper function for nub (remove duplicates) since it's not imported
nub :: Eq a => [a] -> [a]
nub = nubBy (==)

-- | Helper function for nubBy since it's not imported
nubBy :: (a -> a -> Bool) -> [a] -> [a]
nubBy _ [] = []
nubBy eq (x:xs) = x : nubBy eq (filter (\y -> not (eq x y)) xs) 