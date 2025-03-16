{-# LANGUAGE OverloadedStrings #-}
module TradeWar.StrategyDSL
    ( Strategy(..)
    , PrimitiveStrategy(..)
    , CombinatorType(..)
    , CondType(..)
    , RandomType(..)
    , ModifierType(..)
    , Predicate(..)
    ) where

import Data.Text (Text)
import TradeWar.Types (TradeAction)

-- | Core strategy expression type
data Strategy
    = Primitive PrimitiveStrategy
    | Combinator CombinatorType [Strategy]
    | Conditional CondType Strategy Strategy
    | Random RandomType [Strategy] [Double]
    | Modified ModifierType Strategy
    deriving (Show, Eq)

-- | Basic strategy primitives
data PrimitiveStrategy
    = ImposeTariff Text Double    -- industry name and rate
    | RemoveTariff Text           -- industry name
    | Negotiate Text              -- country name
    | DoNothing
    | Mirror                      -- mirrorLastAction
    | Retaliate                   -- retaliateAgainst
    | Cooperate                   -- cooperateWith
    deriving (Show, Eq)

-- | Core strategy combinators
data CombinatorType
    = Always TradeAction
    | Sequential
    | Alternate
    | Compose
    deriving (Show, Eq)

-- | Conditional combinators
data CondType
    = When Predicate
    | Unless Predicate
    | IfThenElse Predicate
    deriving (Show, Eq)

-- | Random strategy types
data RandomType
    = RandomChoice
    | Weighted
    | WithProbability Double
    deriving (Show, Eq)

-- | Strategy modifiers
data ModifierType
    = Limit Int
    | Times Int
    | UntilPayoff Double
    | WithMemory Int
    | WithLookAhead Int
    | WithPayoffThreshold Double
    | WithCountryFilter Text       -- country name predicate
    deriving (Show, Eq)

-- | Strategy predicates
data Predicate
    = PayoffAbove Double
    | GrowthBelow Double
    | HasTariffs
    | LastActionWas TradeAction
    deriving (Show, Eq) 