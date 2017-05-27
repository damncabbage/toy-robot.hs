{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Robot.String
    ( Board(..)
    , BotState(..)
    , Cardinal(..)
    , Command(..)
    , Error(..)
    , Pos(..)
    , Warning(..)
    , interpretCommand
    , interpretMultiple
    , defaultState
    , parseLines
    , parseLine
    , left
    , right
    ) where

import           Prelude
import           Text.Read (readMaybe)
import           Data.Monoid ((<>))
import           Data.These (These(..))
import           Data.Traversable (traverse)
import           Control.Monad (foldM)
import           Control.Monad.Identity (Identity)

-- Definition order of Cardinal matters; if you want to be
-- explicit, create another function that defines the ordering
-- as a list.
data Cardinal =
    North
  | East
  | South
  | West
  deriving (Eq, Show, Enum, Bounded)

data Command =
    PlaceAt (Int,Int) Cardinal
  | MoveForward
  | TurnLeft
  | TurnRight
  | Report
  deriving (Eq, Show)

data Warning =
    IgnoredBeforePlace
  | PlaceOffBoard
  | MovementOffBoard
  deriving (Eq, Show)

data Error =
    InvalidCommand String
  | NoInput
  deriving (Eq, Show)

data Pos = Pos (Int,Int) Cardinal
  deriving (Eq, Show)

newtype Board = Board (Int,Int)
  deriving (Eq, Show)

data BotState = BotState {
    board :: Board
  , position :: Maybe Pos
  } deriving (Eq, Show)


parseLines :: String -> Either Error [Command]
parseLines input =
  case (lines input) of
    [] ->
      Left NoInput
    line ->
      traverse parseLine line

parseLine :: String -> Either Error Command
parseLine input =
  case input of
    -- This case illustrates where a small parser would be useful;
    -- this only works because x/y will only be one character.
    ('P':'L':'A':'C':'E':' ':xRaw:',':yRaw:',':facingRaw) ->
      nothingToLeft (InvalidCommand input) $ do
        xPos <- readMaybe [xRaw]
        yPos <- readMaybe [yRaw]
        facing <- readMaybeCardinal facingRaw
        pure $ PlaceAt (xPos,yPos) facing
    "REPORT" ->
      pure Report
    "MOVE" ->
      pure MoveForward
    "LEFT" ->
      pure TurnLeft
    "RIGHT" ->
      pure TurnRight
    _ ->
      Left (InvalidCommand input)
  where
    readMaybeCardinal :: String -> Maybe Cardinal
    readMaybeCardinal = \case
      "NORTH" -> Just North
      "EAST" -> Just East
      "SOUTH" -> Just South
      "WEST" -> Just West
      _ -> Nothing


defaultState :: BotState
defaultState =
  BotState (Board (5,5)) Nothing

-- This tuple smashing is pretty ugly.
interpretMultiple ::
  BotState ->
  [Command] ->
  (BotState, [(Command, Warning)], [BotState])
interpretMultiple initial =
  flip foldl (initial, [], []) $ \(bot, warns, reps) cmd ->
    let
      (bot2, warns2, reps2) = interpretCommand bot cmd
      toList :: Maybe a -> [a]
      toList = maybe [] pure
    in
      (bot2, warns <> (toList warns2), reps <> (toList reps2))

interpretCommand ::
  BotState ->
  Command ->
  (BotState, Maybe (Command,Warning), Maybe BotState)
interpretCommand bot@(BotState board@(Board (w,h)) mpos) cmd =
  case cmd of
    PlaceAt xy card ->
      if insideBoard board xy
        then update $ bot { position = Just (Pos xy card) }
        else warn PlaceOffBoard
    MoveForward ->
      withPlacement $ \pos ->
        let newPos@(Pos xy _) = moveForward pos
        in  if insideBoard board xy
              then update $ bot { position = Just newPos }
              else warn MovementOffBoard
    TurnLeft ->
      withPlacement $ \(Pos xy dir) ->
        update $ bot { position = Just (Pos xy $ left dir) }
    TurnRight ->
      withPlacement $ \(Pos xy dir) ->
        update $ bot { position = Just (Pos xy $ right dir) }
    Report ->
      (bot, Nothing, Just bot)
  where
    update newBot = (newBot, Nothing, Nothing)
    warn w = (bot, Just (cmd, w), Nothing)
    withPlacement f = maybe (warn IgnoredBeforePlace) f mpos


insideBoard :: Board -> (Int,Int) -> Bool
insideBoard (Board (w,h)) (x,y) =
  x >= 0 && x < w && y >= 0 && y < h

-- 0,0 (origin) is the South-West corner.
-- The Toy Robot is obviously PostScript-based.
moveForward :: Pos -> Pos
moveForward (Pos (x,y) dir) =
  flip Pos dir $ case dir of
    North -> (x,     y + 1)
    East ->  (x + 1, y    )
    South -> (x,     y - 1)
    West ->  (x - 1, y    )




---- Misc ----

nothingToLeft :: b -> Maybe a -> Either b a
nothingToLeft b ma =
  case ma of
    Just a -> Right a
    Nothing -> Left b

right :: (Eq a, Enum a, Bounded a) => a -> a
right x =
  if x == maxBound then minBound else succ x

left :: (Eq a, Enum a, Bounded a) => a -> a
left x =
  if x == minBound then maxBound else pred x
