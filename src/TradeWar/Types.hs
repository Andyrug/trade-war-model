module TradeWar.Types
    ( Country(..)
    , Industry(..)
    , Economy(..)
    , Tariff(..)
    , TradeRelation(..)
    , TradeAction(..)
    , Strategy
    , TradeWarScenario(..)
    , SimulationResult(..)
    , GameState(..)
    ) where

import qualified Data.Map as M
import Data.Text (Text)

-- | Represents a country in the trade war
data Country = Country
    { countryName :: Text           -- ^ Name of the country
    , countryEconomy :: Economy     -- ^ Economic parameters of the country
    , countryIndustries :: [Industry] -- ^ Industries in the country
    } deriving (Show, Eq)

-- Make Country manually implement Ord to avoid the Economy Ord requirement
instance Ord Country where
    compare c1 c2 = compare (countryName c1) (countryName c2)

-- | Represents an industry sector
data Industry = Industry
    { industryName :: Text          -- ^ Name of the industry
    , industryGDP :: Double         -- ^ Contribution to GDP (percentage)
    , industryExportDependence :: Double -- ^ Dependence on exports (0-1)
    , industryImportDependence :: Double -- ^ Dependence on imports (0-1)
    } deriving (Show, Eq, Ord)

-- | Economic parameters of a country
data Economy = Economy
    { economyGDP :: Double          -- ^ GDP in billions of dollars
    , economyGrowthRate :: Double   -- ^ Annual growth rate (percentage)
    , economyInflationRate :: Double -- ^ Inflation rate (percentage)
    , economyUnemploymentRate :: Double -- ^ Unemployment rate (percentage)
    } deriving (Show, Eq)

-- | Represents a tariff on goods
data Tariff = Tariff
    { tariffIndustry :: Industry    -- ^ Industry affected by the tariff
    , tariffRate :: Double          -- ^ Tariff rate (percentage)
    , tariffImpact :: Double        -- ^ Economic impact (percentage of GDP)
    } deriving (Show, Eq)

-- | Represents a trade relationship between two countries
data TradeRelation = TradeRelation
    { tradeSource :: Country        -- ^ Exporting country
    , tradeDestination :: Country   -- ^ Importing country
    , tradeTariffs :: [Tariff]      -- ^ Tariffs applied to trade
    , tradeVolume :: Double         -- ^ Trade volume in billions of dollars
    } deriving (Show, Eq)

-- | Possible actions in a trade war
data TradeAction
    = ImposeTariff Industry Double  -- ^ Impose a tariff on an industry with a rate
    | RemoveTariff Industry         -- ^ Remove a tariff on an industry
    | NegotiateTrade Country        -- ^ Negotiate with another country
    | NoAction                      -- ^ Take no action
    deriving (Show, Eq)

-- | Implement Ord for TradeAction to allow it to be used as a key in Maps
instance Ord TradeAction where
    compare (ImposeTariff i1 r1) (ImposeTariff i2 r2) = 
        case compare i1 i2 of
            EQ -> compare r1 r2
            other -> other
    compare (RemoveTariff i1) (RemoveTariff i2) = compare i1 i2
    compare (NegotiateTrade c1) (NegotiateTrade c2) = compare c1 c2
    compare NoAction NoAction = EQ
    compare NoAction _ = LT
    compare _ NoAction = GT
    compare (ImposeTariff _ _) _ = LT
    compare _ (ImposeTariff _ _) = GT
    compare (RemoveTariff _) _ = LT
    compare _ (RemoveTariff _) = GT

-- | A strategy is a function that takes the current game state and returns an action
type Strategy = GameState -> TradeAction

-- | Represents a trade war scenario
data TradeWarScenario = TradeWarScenario
    { scenarioCountries :: [Country]  -- ^ Countries involved in the trade war
    , scenarioInitialTariffs :: M.Map (Country, Country) [Tariff] -- ^ Initial tariffs between countries
    } deriving (Show)

-- | Results of a trade war simulation
data SimulationResult = SimulationResult
    { simStates :: [GameState]      -- ^ Game states over time
    , simFinalEconomies :: M.Map Country Economy -- ^ Final economic state of each country
    , simActions :: M.Map Country [TradeAction] -- ^ Actions taken by each country
    } deriving (Show)

-- | Represents the state of the trade war game at a point in time
data GameState = GameState
    { gameRound :: Int              -- ^ Current round of the game
    , gameCountries :: [Country]    -- ^ Countries in the game
    , gameEconomies :: M.Map Country Economy -- ^ Current economic state of each country
    , gameTariffs :: M.Map (Country, Country) [Tariff] -- ^ Current tariffs between countries
    , gameTradeRelations :: [TradeRelation] -- ^ Current trade relations
    , gameHistory :: M.Map Country [TradeAction] -- ^ History of actions taken by each country
    } deriving (Show) 