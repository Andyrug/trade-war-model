# Trade War Simulation

A game theory-based simulation of international trade wars, with a focus on modeling real-world economic impacts and finding optimal strategies.

## Overview

This project provides a framework for simulating trade wars between countries, analyzing the economic impacts of different trade policies, and finding optimal strategies using game theory concepts like Nash equilibria and Pareto optimality.

The simulation allows users to:
- Create custom countries with realistic economic parameters
- Define trade relationships and tariffs between countries
- Run simulations with different strategies
- Analyze the outcomes using game theory
- Visualize the results with interactive charts and graphs

## Features

### Core Simulation
- Model countries with economic parameters (GDP, growth rate, inflation, unemployment)
- Define industries with varying export/import dependencies
- Implement tariffs with specific rates and economic impacts
- Calculate payoffs based on economic outcomes
- Find Nash equilibria and Pareto optimal outcomes

### Strategy System 
- Built-in strategies (tit-for-tat, aggressive, defensive, negotiation)
- Compositional strategy building with combinators
- Strategy evaluation and comparison
- Look-ahead simulation for strategy planning

### Data Integration (Planned)
- Import real-world economic data from sources like World Bank, IMF, and WTO
- Create realistic scenarios based on current global trade situations
- Update simulations with the latest economic indicators

### Visualization (Planned)
- Time series charts for economic indicators
- Payoff matrices for game theory analysis
- Network graphs for trade relationships
- Comparative charts for strategy outcomes

### Web Interface (Planned)
- Interactive country configuration
- Real-time simulation controls
- Visual results dashboard
- Strategy builder interface
- Scenario saving and sharing

## Example Simulation Results

Below is the output from a proof-of-concept simulation of a US-Canada trade war scenario:

```
Trade War Simulation
====================

Initial State:
Round: 0
Country: "United States"
  GDP: $21400.0 billion
  Growth Rate: 2.3%
  Unemployment: 3.9%
Country: "Canada"
  GDP: $1700.0 billion
  Growth Rate: 1.8%
  Unemployment: 5.8%

Tariffs:
"Canada" -> "United States":
  "Dairy": 270.0%
"United States" -> "Canada":
  "Steel": 25.0%
  "Aluminum": 10.0%

Final State (after 5 rounds):
Round: 5
Country: "United States"
  GDP: $21400.0 billion
  Growth Rate: 1.8500000000000003%
  Unemployment: 4.124999999999999%
Country: "Canada"
  GDP: $1700.0 billion
  Growth Rate: 1.3500000000000008%
  Unemployment: 6.0249999999999995%

Nash Equilibria (1 found):
Equilibrium 1:
Strategy:
  "Canada": RemoveTariff (Industry {industryName = "Dairy", industryGDP = 2.0, industryExportDependence = 0.1, industryImportDependence = 5.0e-2})
  "United States": RemoveTariff (Industry {industryName = "Steel", industryGDP = 1.5, industryExportDependence = 0.3, industryImportDependence = 0.2})
Payoffs:
  "Canada": -1.6874999999999991
  "United States": -0.26249999999999907

Pareto Optimal Outcomes (1 found):
Equilibrium 1:
Strategy:
  "Canada": RemoveTariff (Industry {industryName = "Dairy", industryGDP = 2.0, industryExportDependence = 0.1, industryImportDependence = 5.0e-2})
  "United States": RemoveTariff (Industry {industryName = "Steel", industryGDP = 1.5, industryExportDependence = 0.3, industryImportDependence = 0.2})
Payoffs:
  "Canada": -1.6874999999999991
  "United States": -0.26249999999999907

Optimal Joint Strategy (Total Welfare: -1.9499999999999982):
Strategy:
  "Canada": RemoveTariff (Industry {industryName = "Dairy", industryGDP = 2.0, industryExportDependence = 0.1, industryImportDependence = 5.0e-2})
  "United States": RemoveTariff (Industry {industryName = "Steel", industryGDP = 1.5, industryExportDependence = 0.3, industryImportDependence = 0.2})
```

### Analysis of Results

The simulation demonstrates several key insights about trade wars:

1. **Economic Decline**: Both countries experience economic deterioration over the 5 rounds:
   - US growth rate decreases from 2.3% to 1.85%
   - US unemployment increases from 3.9% to 4.12%
   - Canada growth rate decreases from 1.8% to 1.35%
   - Canada unemployment increases from 5.8% to 6.02%

2. **Nash Equilibrium**: The simulation finds a single Nash equilibrium where:
   - Canada removes its 270% tariff on dairy products
   - US removes its 25% tariff on steel

3. **Pareto Optimality**: The same strategy is also Pareto optimal, meaning no country can improve its position without making the other worse off.

4. **Optimal Joint Strategy**: The optimal strategy for maximizing total welfare matches the Nash equilibrium, with a total welfare of -1.95.

These results align with economic theory, which predicts that trade wars generally lead to negative outcomes for all parties involved, and that removing tariffs is typically the optimal strategy for maximizing economic welfare.

## Getting Started

### Prerequisites
- GHC (Glasgow Haskell Compiler) 9.4 or later
- Cabal 3.6 or later

### Installation

1. Clone the repository:
```bash
git clone https://github.com/yourusername/trade-war-model.git
cd trade-war-model
```

2. Build the project:
```bash
cabal update
cabal build
```

3. Run the simulation:
```bash
cabal run trade-war-sim
```

## Usage

### Basic Simulation

```haskell
-- Create a scenario
let scenario = createUSCanadaScenario

-- Initialize game state
let initialState = initializeGameState scenario

-- Create strategy map with tit-for-tat for all countries
let strategies = M.fromList [(c, titForTatStrategy) | c <- scenarioCountries scenario]

-- Run simulation with tit-for-tat strategy for 5 rounds
let result = runSimulationWithStrategies initialState strategies 5

-- Find Nash equilibria
let nashEquilibria = findNashEquilibrium (last $ simStates result)

-- Find Pareto optimal outcomes
let paretoOptimal = findParetoOptimal (last $ simStates result)

-- Find optimal joint strategy
let (optimalStrategy, totalWelfare) = findOptimalJointStrategy (last $ simStates result)
```

### Creating Custom Countries

```haskell
-- Define industries
let steel = Industry (pack "Steel") 1.5 0.3 0.2
let aluminum = Industry (pack "Aluminum") 0.8 0.4 0.3

-- Define a country
let usa = Country 
    { countryName = pack "United States"
    , countryEconomy = Economy 21400 2.3 2.1 3.9
    , countryIndustries = [steel, aluminum]
    }
```

### Building Strategies with Combinators

```haskell
-- Create a strategy that imposes tariffs when growth rate is negative
let defensiveStrategy = when 
    (\self _ state -> getGrowthRate self state < 0)
    (imposeTariffOn steel 25.0)

-- Create a strategy that alternates between negotiation and retaliation
let mixedStrategy = alternate 
    [negotiateWith canada, retaliateAgainst]

-- Create a strategy with a 70% chance of cooperation and 30% chance of retaliation
let probabilisticStrategy = weighted
    [(0.7, cooperateWith), (0.3, retaliateAgainst)]
```

## Development Roadmap

### Phase 1: Core Simulation (Completed)
- ✅ Basic economic model
- ✅ Game theory calculations
- ✅ Strategy implementation
- ✅ Command-line interface

### Phase 2: Enhanced Features (In Progress)
- 🔄 Strategy combinators
- 🔄 Data visualization
- 🔄 Real-world data integration
- 🔄 Advanced economic modeling

### Phase 3: Web Interface (Planned)
- 📅 Frontend development (React/TypeScript)
- 📅 Backend API (Haskell with Servant)
- 📅 Interactive visualizations (D3.js)
- 📅 User account system

### Phase 4: Advanced Features (Future)
- 📅 Machine learning for strategy optimization
- 📅 Multi-country coalition modeling
- 📅 Stochastic simulation with uncertainty
- 📅 Real-time data updates

## Architecture

The project is structured around several core modules:

- `TradeWar.Types`: Core data types for the simulation
- `TradeWar.Model`: Economic model and payoff calculations
- `TradeWar.Strategy`: Strategy definitions and implementations
- `TradeWar.Equilibrium`: Game theory calculations
- `TradeWar.Simulation`: Simulation engine
- `TradeWar.StrategyCombinators`: Compositional strategy building
- `TradeWar.Visualization`: Data visualization tools
- `TradeWar.RealWorldData`: Integration with external data sources

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request.

1. Fork the repository
2. Create your feature branch (`git checkout -b feature/amazing-feature`)
3. Commit your changes (`git commit -m 'Add some amazing feature'`)
4. Push to the branch (`git push origin feature/amazing-feature`)
5. Open a Pull Request

## License

This project is licensed under the MIT License - see the LICENSE file for details.

## Acknowledgments

- Game theory concepts based on works by John Nash, Thomas Schelling, and Robert Axelrod
- Economic modeling inspired by research from the World Trade Organization and International Monetary Fund
- Special thanks to the Haskell community for providing excellent libraries and tools 
- This project is dedicated to the memory of my Father Dr. David Michael Kuhlman PhD aka Mike.  Dad was a Social Psychologist concerned with choice in social dilemmas, social judgment, interpersonal trust, and emotion. Game theory was one of his passions and one of my earliest memories is of him explaing the Prisoners Dilema and utility.
