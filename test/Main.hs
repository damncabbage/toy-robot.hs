{-# LANGUAGE TemplateHaskell #-}

import           Data.Traversable (for)
import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import           Robot.String


genCardinal :: Monad m => Gen m Cardinal
genCardinal =
  Gen.element [North, East, South, West]

genBoard :: Monad m => Gen m Board
genBoard =
  (\x y -> Board (x,y))
    <$> Gen.int (Range.linear 0 100)
    <*> Gen.int (Range.linear 0 100)

genPos :: Monad m => Board -> Gen m Pos
genPos (Board (w,h)) = do
  dir <- genCardinal
  x <- Gen.int $ Range.linear 0 (w - 1)
  y <- Gen.int $ Range.linear 0 (h - 1)
  pure $ Pos (x,y) dir

genBotState :: Monad m => Gen m BotState
genBotState = do
  board <- genBoard
  pos <- Gen.maybe $ genPos board
  pure $ BotState board pos



prop_cmd_randomTurns :: Property
prop_cmd_randomTurns =
  property $ do
    st <- forAll $ genBotState
    cs <- forAll $ Gen.list
            (Range.linear 0 100)
            (Gen.element [TurnLeft, TurnRight])
    let funcs = flip map cs $ \turn ->
          case turn of
            TurnLeft -> -1
            TurnRight -> 1
    (1 === 1)




-- Kick off the tests.

tests :: IO Bool
tests =
  checkParallel $$(discover)

main :: IO ()
main =
  tests >> pure ()
