module Direction (Direction(..), Turn(..), turn, rightOf, leftOf) where

import Text.Show.Functions

data Direction = North
               | South
               | East
               | West
  deriving Show 

data Turn = LeftTurn | RightTurn 
  deriving Show

leftOf :: Direction -> Direction
leftOf North = West
leftOf West = South
leftOf South = East
leftOf East = North

rightOf :: Direction -> Direction
rightOf North = East
rightOf East = South
rightOf South = West
rightOf West = North

turn :: Direction -> Turn -> Direction
turn dir LeftTurn = leftOf dir
turn dir RightTurn = rightOf dir