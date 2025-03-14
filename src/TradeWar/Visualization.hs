module TradeWar.Visualization
    ( -- * Chart generation
      generateGDPChart
    , generateGrowthRateChart
    , generateUnemploymentChart
    , generateTariffChart
    , generatePayoffMatrix
    , generateTradeRelationNetwork
    , generateStrategyComparisonChart
    
    -- * Data export
    , exportToCSV
    , exportToJSON
    
    -- * Visualization utilities
    , createTimeSeriesData
    , createHeatmapData
    , createNetworkData
    , createBarChartData
    ) where

import qualified Data.Map as M
import Data.Text (Text, unpack)
import Data.List (transpose, nub)
import Data.Maybe (fromMaybe)

import TradeWar.Types
import TradeWar.Model (calculatePayoff)

-- | Generate a GDP chart for countries over time
generateGDPChart :: SimulationResult -> ChartData
generateGDPChart result =
    let states = simStates result
        countries = gameCountries (head states)
        rounds = map gameRound states
        
        -- Extract GDP for each country at each round
        gdpSeries = [(country, [(round, getGDP country state) | (round, state) <- zip rounds states]) 
                    | country <- countries]
    in ChartData
        { chartTitle = "GDP Over Time"
        , chartXLabel = "Round"
        , chartYLabel = "GDP (billions $)"
        , chartSeries = [(unpack $ countryName country, values) | (country, values) <- gdpSeries]
        }

-- | Generate a growth rate chart for countries over time
generateGrowthRateChart :: SimulationResult -> ChartData
generateGrowthRateChart result =
    let states = simStates result
        countries = gameCountries (head states)
        rounds = map gameRound states
        
        -- Extract growth rates for each country at each round
        growthSeries = [(country, [(round, getGrowthRate country state) | (round, state) <- zip rounds states]) 
                       | country <- countries]
    in ChartData
        { chartTitle = "Economic Growth Rate Over Time"
        , chartXLabel = "Round"
        , chartYLabel = "Growth Rate (%)"
        , chartSeries = [(unpack $ countryName country, values) | (country, growthSeries) <- growthSeries]
        }

-- | Generate an unemployment chart for countries over time
generateUnemploymentChart :: SimulationResult -> ChartData
generateUnemploymentChart result =
    let states = simStates result
        countries = gameCountries (head states)
        rounds = map gameRound states
        
        -- Extract unemployment rates for each country at each round
        unemploymentSeries = [(country, [(round, getUnemploymentRate country state) | (round, state) <- zip rounds states]) 
                             | country <- countries]
    in ChartData
        { chartTitle = "Unemployment Rate Over Time"
        , chartXLabel = "Round"
        , chartYLabel = "Unemployment Rate (%)"
        , chartSeries = [(unpack $ countryName country, values) | (country, values) <- unemploymentSeries]
        }

-- | Generate a chart showing tariff rates over time
generateTariffChart :: SimulationResult -> Country -> Country -> Industry -> ChartData
generateTariffChart result from to industry =
    let states = simStates result
        rounds = map gameRound states
        
        -- Extract tariff rates for the specific industry between countries
        tariffSeries = [(round, getTariffRate from to industry state) | (round, state) <- zip rounds states]
    in ChartData
        { chartTitle = "Tariff Rate: " ++ unpack (countryName from) ++ " -> " ++ 
                      unpack (countryName to) ++ " (" ++ unpack (industryName industry) ++ ")"
        , chartXLabel = "Round"
        , chartYLabel = "Tariff Rate (%)"
        , chartSeries = [("Tariff Rate", tariffSeries)]
        }

-- | Generate a payoff matrix visualization for two countries
generatePayoffMatrix :: GameState -> Country -> Country -> HeatmapData
generatePayoffMatrix state country1 country2 =
    let -- Generate all possible actions for both countries
        actions1 = generatePossibleActions country1 state
        actions2 = generatePossibleActions country2 state
        
        -- Calculate payoffs for all combinations
        payoffs = [((a1, a2), (calculatePayoffForAction country1 a1 country2 a2 state,
                              calculatePayoffForAction country2 a2 country1 a1 state))
                  | a1 <- actions1, a2 <- actions2]
        
        -- Extract payoff matrices
        payoffMatrix1 = [[fst (snd p) | p <- row] | row <- groupByFst payoffs]
        payoffMatrix2 = [[snd (snd p) | p <- row] | row <- groupByFst payoffs]
        
        -- Create labels for actions
        actionLabels1 = map show actions1
        actionLabels2 = map show actions2
    in HeatmapData
        { heatmapTitle = "Payoff Matrix: " ++ unpack (countryName country1) ++ 
                        " vs " ++ unpack (countryName country2)
        , heatmapXLabels = actionLabels2
        , heatmapYLabels = actionLabels1
        , heatmapValues = payoffMatrix1
        , heatmapSecondaryValues = Just payoffMatrix2
        , heatmapXAxisLabel = unpack (countryName country2) ++ " Actions"
        , heatmapYAxisLabel = unpack (countryName country1) ++ " Actions"
        }

-- | Generate a network visualization of trade relations
generateTradeRelationNetwork :: GameState -> NetworkData
generateTradeRelationNetwork state =
    let countries = gameCountries state
        
        -- Create nodes for each country
        nodes = [(country, unpack (countryName country)) | country <- countries]
        
        -- Create edges for trade relations
        edges = [((from, to), tradeVolume relation, hasTariffs relation) 
                | relation <- gameTradeRelations state
                , let from = tradeSource relation
                , let to = tradeDestination relation]
    in NetworkData
        { networkTitle = "Trade Relations Network"
        , networkNodes = nodes
        , networkEdges = edges
        }

-- | Generate a chart comparing different strategies
generateStrategyComparisonChart :: GameState -> [StrategyFn] -> [Text] -> BarChartData
generateStrategyComparisonChart state strategies strategyNames =
    let countries = gameCountries state
        
        -- Calculate payoffs for each strategy for each country
        strategyPayoffs = [[(country, calculateStrategyPayoff country strategy state) 
                           | country <- countries]
                          | strategy <- strategies]
        
        -- Organize data by country
        countryData = [(country, [payoff | strategyPayoffs' <- strategyPayoffs, 
                                 (country', payoff) <- strategyPayoffs', 
                                 country' == country])
                      | country <- countries]
    in BarChartData
        { barChartTitle = "Strategy Comparison"
        , barChartXLabels = map unpack strategyNames
        , barChartYLabel = "Expected Payoff"
        , barChartSeries = [(unpack (countryName country), payoffs) | (country, payoffs) <- countryData]
        }

-- | Export simulation results to CSV format
exportToCSV :: SimulationResult -> FilePath -> IO ()
exportToCSV result filePath = do
    let states = simStates result
        countries = gameCountries (head states)
        
        -- Create header row
        header = "Round,Country,GDP,GrowthRate,UnemploymentRate,Action"
        
        -- Create data rows
        rows = [createCSVRow round country state (simActions result)
               | state <- states
               , let round = gameRound state
               , country <- countries]
        
        -- Combine all rows
        csvContent = unlines (header : rows)
    
    -- Write to file
    writeFile filePath csvContent

-- | Export simulation results to JSON format
exportToJSON :: SimulationResult -> FilePath -> IO ()
exportToJSON result filePath = do
    let jsonContent = createJSONContent result
    writeFile filePath jsonContent

-- | Create time series data for charts
createTimeSeriesData :: SimulationResult -> (Country -> GameState -> Double) -> Text -> ChartData
createTimeSeriesData result valueExtractor yLabel =
    let states = simStates result
        countries = gameCountries (head states)
        rounds = map gameRound states
        
        -- Extract values for each country at each round
        series = [(country, [(round, valueExtractor country state) | (round, state) <- zip rounds states]) 
                 | country <- countries]
    in ChartData
        { chartTitle = unpack yLabel ++ " Over Time"
        , chartXLabel = "Round"
        , chartYLabel = unpack yLabel
        , chartSeries = [(unpack $ countryName country, values) | (country, values) <- series]
        }

-- | Create heatmap data for visualizations
createHeatmapData :: [[Double]] -> [String] -> [String] -> String -> HeatmapData
createHeatmapData values xLabels yLabels title =
    HeatmapData
        { heatmapTitle = title
        , heatmapXLabels = xLabels
        , heatmapYLabels = yLabels
        , heatmapValues = values
        , heatmapSecondaryValues = Nothing
        , heatmapXAxisLabel = "X Axis"
        , heatmapYAxisLabel = "Y Axis"
        }

-- | Create network data for visualizations
createNetworkData :: [(a, String)] -> [((a, a), Double, Bool)] -> String -> NetworkData
createNetworkData nodes edges title =
    NetworkData
        { networkTitle = title
        , networkNodes = nodes
        , networkEdges = edges
        }

-- | Create bar chart data for visualizations
createBarChartData :: [String] -> [String] -> [[Double]] -> String -> BarChartData
createBarChartData categories seriesNames values title =
    let series = zip seriesNames (transpose values)
    in BarChartData
        { barChartTitle = title
        , barChartXLabels = categories
        , barChartYLabel = "Value"
        , barChartSeries = series
        }

-- | Helper function to get GDP for a country in a game state
getGDP :: Country -> GameState -> Double
getGDP country state =
    let economy = fromMaybe (countryEconomy country) (M.lookup country (gameEconomies state))
    in economyGDP economy

-- | Helper function to get growth rate for a country in a game state
getGrowthRate :: Country -> GameState -> Double
getGrowthRate country state =
    let economy = fromMaybe (countryEconomy country) (M.lookup country (gameEconomies state))
    in economyGrowthRate economy

-- | Helper function to get unemployment rate for a country in a game state
getUnemploymentRate :: Country -> GameState -> Double
getUnemploymentRate country state =
    let economy = fromMaybe (countryEconomy country) (M.lookup country (gameEconomies state))
    in economyUnemploymentRate economy

-- | Helper function to get tariff rate for a specific industry between countries
getTariffRate :: Country -> Country -> Industry -> GameState -> Double
getTariffRate from to industry state =
    let tariffs = fromMaybe [] (M.lookup (from, to) (gameTariffs state))
        matchingTariffs = filter (\t -> industryName (tariffIndustry t) == industryName industry) tariffs
    in case matchingTariffs of
        [] -> 0.0
        (t:_) -> tariffRate t

-- | Helper function to calculate payoff for a specific action
calculatePayoffForAction :: Country -> TradeAction -> Country -> TradeAction -> GameState -> Double
calculatePayoffForAction country1 action1 country2 action2 state =
    let -- Create a new state with both actions applied
        newState = applyAction country1 action1 (applyAction country2 action2 state)
        
        -- Calculate payoff for country1 in the new state
        payoff = calculatePayoff country1 newState
    in payoff

-- | Helper function to calculate expected payoff for a strategy
calculateStrategyPayoff :: Country -> StrategyFn -> GameState -> Double
calculateStrategyPayoff country strategy state =
    let action = strategy country (filter (/= country) (gameCountries state)) state
        newState = applyAction country action state
        payoff = calculatePayoff country newState
    in payoff

-- | Helper function to create a CSV row
createCSVRow :: Int -> Country -> GameState -> M.Map Country [TradeAction] -> String
createCSVRow round country state actions =
    let economy = fromMaybe (countryEconomy country) (M.lookup country (gameEconomies state))
        action = case M.lookup country actions of
            Nothing -> "NoAction"
            Just [] -> "NoAction"
            Just (a:_) -> show a
    in show round ++ "," ++
       show (countryName country) ++ "," ++
       show (economyGDP economy) ++ "," ++
       show (economyGrowthRate economy) ++ "," ++
       show (economyUnemploymentRate economy) ++ "," ++
       action

-- | Helper function to create JSON content
createJSONContent :: SimulationResult -> String
createJSONContent result =
    let states = simStates result
        countries = gameCountries (head states)
        
        -- Create JSON structure (simplified)
        jsonStates = map createJSONState states
        jsonActions = createJSONActions (simActions result)
        
        -- Combine into final JSON
        json = "{\n" ++
               "  \"states\": [" ++ intercalate ",\n" jsonStates ++ "],\n" ++
               "  \"actions\": " ++ jsonActions ++ "\n" ++
               "}"
    in json

-- | Helper function to create JSON for a game state
createJSONState :: GameState -> String
createJSONState state =
    let round = gameRound state
        countries = gameCountries state
        
        -- Create JSON for each country
        countryJSON = map (createJSONCountry state) countries
        
        -- Create JSON for tariffs
        tariffJSON = createJSONTariffs (gameTariffs state)
    in "{\n" ++
       "    \"round\": " ++ show round ++ ",\n" ++
       "    \"countries\": [" ++ intercalate ",\n" countryJSON ++ "],\n" ++
       "    \"tariffs\": " ++ tariffJSON ++ "\n" ++
       "  }"

-- | Helper function to create JSON for a country
createJSONCountry :: GameState -> Country -> String
createJSONCountry state country =
    let economy = fromMaybe (countryEconomy country) (M.lookup country (gameEconomies state))
    in "      {\n" ++
       "        \"name\": \"" ++ unpack (countryName country) ++ "\",\n" ++
       "        \"gdp\": " ++ show (economyGDP economy) ++ ",\n" ++
       "        \"growthRate\": " ++ show (economyGrowthRate economy) ++ ",\n" ++
       "        \"unemploymentRate\": " ++ show (economyUnemploymentRate economy) ++ "\n" ++
       "      }"

-- | Helper function to create JSON for tariffs
createJSONTariffs :: M.Map (Country, Country) [Tariff] -> String
createJSONTariffs tariffs =
    let tariffEntries = map createJSONTariffEntry (M.toList tariffs)
    in "[\n" ++ intercalate ",\n" tariffEntries ++ "\n    ]"

-- | Helper function to create JSON for a tariff entry
createJSONTariffEntry :: ((Country, Country), [Tariff]) -> String
createJSONTariffEntry ((from, to), tariffs) =
    let tariffJSON = map createJSONTariff tariffs
    in "      {\n" ++
       "        \"from\": \"" ++ unpack (countryName from) ++ "\",\n" ++
       "        \"to\": \"" ++ unpack (countryName to) ++ "\",\n" ++
       "        \"tariffs\": [" ++ intercalate ", " tariffJSON ++ "]\n" ++
       "      }"

-- | Helper function to create JSON for a tariff
createJSONTariff :: Tariff -> String
createJSONTariff tariff =
    "{\n" ++
    "          \"industry\": \"" ++ unpack (industryName (tariffIndustry tariff)) ++ "\",\n" ++
    "          \"rate\": " ++ show (tariffRate tariff) ++ ",\n" ++
    "          \"impact\": " ++ show (tariffImpact tariff) ++ "\n" ++
    "        }"

-- | Helper function to create JSON for actions
createJSONActions :: M.Map Country [TradeAction] -> String
createJSONActions actions =
    let actionEntries = map createJSONActionEntry (M.toList actions)
    in "{\n" ++ intercalate ",\n" actionEntries ++ "\n  }"

-- | Helper function to create JSON for an action entry
createJSONActionEntry :: (Country, [TradeAction]) -> String
createJSONActionEntry (country, actions) =
    let actionJSON = map show actions
    in "    \"" ++ unpack (countryName country) ++ "\": [" ++ 
       intercalate ", " (map (\a -> "\"" ++ a ++ "\"") actionJSON) ++ "]"

-- | Helper function to apply an action to a state
applyAction :: Country -> TradeAction -> GameState -> GameState
applyAction = error "This function should be imported from TradeWar.Model"

-- | Helper function to generate possible actions for a country
generatePossibleActions :: Country -> GameState -> [TradeAction]
generatePossibleActions = error "This function should be imported from TradeWar.Strategy"

-- | Helper function to group a list by the first element of pairs
groupByFst :: Eq a => [((a, b), c)] -> [[((a, b), c)]]
groupByFst [] = []
groupByFst (x@((a, _), _):xs) =
    let (same, rest) = partition (\((a', _), _) -> a' == a) xs
    in (x : same) : groupByFst rest

-- | Helper function for partition since it's not imported
partition :: (a -> Bool) -> [a] -> ([a], [a])
partition p xs = (filter p xs, filter (not . p) xs)

-- | Helper function for intercalate since it's not imported
intercalate :: String -> [String] -> String
intercalate sep [] = ""
intercalate sep [x] = x
intercalate sep (x:xs) = x ++ sep ++ intercalate sep xs

-- | Data type for chart data
data ChartData = ChartData
    { chartTitle :: String
    , chartXLabel :: String
    , chartYLabel :: String
    , chartSeries :: [(String, [(Int, Double)])]
    }

-- | Data type for heatmap data
data HeatmapData = HeatmapData
    { heatmapTitle :: String
    , heatmapXLabels :: [String]
    , heatmapYLabels :: [String]
    , heatmapValues :: [[Double]]
    , heatmapSecondaryValues :: Maybe [[Double]]
    , heatmapXAxisLabel :: String
    , heatmapYAxisLabel :: String
    }

-- | Data type for network data
data NetworkData = NetworkData
    { networkTitle :: String
    , networkNodes :: [(Country, String)]
    , networkEdges :: [((Country, Country), Double, Bool)]
    }

-- | Data type for bar chart data
data BarChartData = BarChartData
    { barChartTitle :: String
    , barChartXLabels :: [String]
    , barChartYLabel :: String
    , barChartSeries :: [(String, [Double])]
    }

-- | Helper function to check if a trade relation has tariffs
hasTariffs :: TradeRelation -> Bool
hasTariffs relation = not (null (tradeTariffs relation)) 