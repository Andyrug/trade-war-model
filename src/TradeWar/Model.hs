module TradeWar.Model
    ( initializeGameState
    , applyAction
    , calculatePayoff
    , calculateEconomicImpact
    , updateEconomy
    , createCountry
    , createIndustry
    , createTariff
    , createTradeRelation
    ) where

import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)

import TradeWar.Types

-- | Initialize a game state from a scenario
initializeGameState :: TradeWarScenario -> GameState
initializeGameState scenario = GameState
    { gameRound = 0
    , gameCountries = scenarioCountries scenario
    , gameEconomies = M.fromList [(c, countryEconomy c) | c <- scenarioCountries scenario]
    , gameTariffs = scenarioInitialTariffs scenario
    , gameTradeRelations = generateTradeRelations (scenarioCountries scenario) (scenarioInitialTariffs scenario)
    , gameHistory = M.fromList [(c, []) | c <- scenarioCountries scenario]
    }

-- | Generate initial trade relations between countries
generateTradeRelations :: [Country] -> M.Map (Country, Country) [Tariff] -> [TradeRelation]
generateTradeRelations countries tariffs = 
    [ TradeRelation source dest tariffList 0.0
    | source <- countries
    , dest <- countries
    , source /= dest
    , let tariffList = fromMaybe [] (M.lookup (source, dest) tariffs)
    ]

-- | Apply a trade action to the game state
applyAction :: Country -> TradeAction -> GameState -> GameState
applyAction actor action state = case action of
    ImposeTariff industry rate -> 
        let newTariff = Tariff industry rate (calculateTariffImpact industry rate)
            targets = filter (/= actor) (gameCountries state)
            newTariffs = foldr (\target acc -> 
                                    M.adjust (newTariff :) (actor, target) acc) 
                                (gameTariffs state) targets
        in updateGameState actor action newTariffs state
    
    RemoveTariff industry ->
        let targets = filter (/= actor) (gameCountries state)
            newTariffs = foldr (\target acc -> 
                                    M.adjust (filter (\t -> industryName (tariffIndustry t) /= industryName industry)) 
                                            (actor, target) acc) 
                                (gameTariffs state) targets
        in updateGameState actor action newTariffs state
    
    NegotiateTrade target ->
        -- Negotiation reduces tariffs by 50% between the two countries
        let reduceTariffs = map (\t -> t { tariffRate = tariffRate t * 0.5 })
            newTariffs = M.adjust reduceTariffs (actor, target) $
                         M.adjust reduceTariffs (target, actor) (gameTariffs state)
        in updateGameState actor action newTariffs state
    
    NoAction -> 
        updateGameState actor action (gameTariffs state) state

-- | Update the game state after an action
updateGameState :: Country -> TradeAction -> M.Map (Country, Country) [Tariff] -> GameState -> GameState
updateGameState actor action newTariffs state = 
    let newTradeRelations = updateTradeRelations (gameTradeRelations state) newTariffs
        newEconomies = calculateEconomicImpact newTradeRelations (gameEconomies state)
        newHistory = M.adjust (action :) actor (gameHistory state)
    in state { gameTariffs = newTariffs
             , gameTradeRelations = newTradeRelations
             , gameEconomies = newEconomies
             , gameHistory = newHistory
             }

-- | Update trade relations based on new tariffs
updateTradeRelations :: [TradeRelation] -> M.Map (Country, Country) [Tariff] -> [TradeRelation]
updateTradeRelations relations tariffs =
    [ relation { tradeTariffs = fromMaybe [] (M.lookup (tradeSource relation, tradeDestination relation) tariffs) }
    | relation <- relations
    ]

-- | Calculate the economic impact of trade relations on economies
calculateEconomicImpact :: [TradeRelation] -> M.Map Country Economy -> M.Map Country Economy
calculateEconomicImpact relations economies =
    foldr updateEconomyFromRelation economies relations

-- | Update an economy based on a trade relation
updateEconomyFromRelation :: TradeRelation -> M.Map Country Economy -> M.Map Country Economy
updateEconomyFromRelation relation economies =
    let source = tradeSource relation
        dest = tradeDestination relation
        tariffs = tradeTariffs relation
        
        -- Calculate impact on source (exporter)
        sourceImpact = sum [tariffImpact t | t <- tariffs]
        sourceEconomy = fromMaybe (countryEconomy source) (M.lookup source economies)
        newSourceEconomy = updateEconomy sourceEconomy (-sourceImpact)
        
        -- Calculate impact on destination (importer)
        destImpact = sum [tariffImpact t * 0.5 | t <- tariffs]  -- Importers are affected less
        destEconomy = fromMaybe (countryEconomy dest) (M.lookup dest economies)
        newDestEconomy = updateEconomy destEconomy (-destImpact)
    in
    M.insert source newSourceEconomy $ M.insert dest newDestEconomy economies

-- | Update an economy based on an impact percentage
updateEconomy :: Economy -> Double -> Economy
updateEconomy economy impact =
    let growthAdjustment = impact / 10  -- Convert impact to growth rate adjustment
    in economy { economyGrowthRate = max 0 (economyGrowthRate economy + growthAdjustment)
               , economyUnemploymentRate = max 0 (economyUnemploymentRate economy - growthAdjustment / 2)
               }

-- | Calculate the payoff for a country in the current game state
calculatePayoff :: Country -> GameState -> Double
calculatePayoff country state =
    let economy = fromMaybe (countryEconomy country) (M.lookup country (gameEconomies state))
        -- Payoff is based on GDP growth and unemployment
        payoff = economyGrowthRate economy - economyUnemploymentRate economy / 2
    in payoff

-- | Calculate the impact of a tariff based on industry characteristics
calculateTariffImpact :: Industry -> Double -> Double
calculateTariffImpact industry rate =
    let baseTariffImpact = rate / 100  -- Convert percentage to decimal
        -- Industries with high export dependence are more affected by tariffs
        scaledImpact = baseTariffImpact * industryExportDependence industry * industryGDP industry / 100
    in scaledImpact

-- | Helper function to create a country
createCountry :: Text -> Double -> Double -> Double -> Double -> [Industry] -> Country
createCountry name gdp growth inflation unemployment industries =
    Country name (Economy gdp growth inflation unemployment) industries

-- | Helper function to create an industry
createIndustry :: Text -> Double -> Double -> Double -> Industry
createIndustry name gdpContribution exportDep importDep =
    Industry name gdpContribution exportDep importDep

-- | Helper function to create a tariff
createTariff :: Industry -> Double -> Tariff
createTariff industry rate =
    Tariff industry rate (calculateTariffImpact industry rate)

-- | Helper function to create a trade relation
createTradeRelation :: Country -> Country -> [Tariff] -> Double -> TradeRelation
createTradeRelation source dest tariffs volume =
    TradeRelation source dest tariffs volume 