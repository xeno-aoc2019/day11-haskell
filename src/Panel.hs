module Panel (Position, initialOutdoors, turnAndMove, paintedSquares, Panel, Turn(..), Color(..), move, paint, colorAtRobot, Outdoors) where

import Direction
import Data.List.Index
import Data.Map.Strict
import Data.Sequence
import Prelude hiding (filter, lookup, map, null)
import Text.Show.Functions


data Color = Black | White deriving Show
data Position = Position Int Int Direction deriving Show
data Panel = Panel (Map (Int,Int) Color) deriving Show
data Outdoors = Outdoors Panel Position deriving Show

initialOutdoors = Outdoors (Panel Data.Map.Strict.empty) (Position 0 0 North)

move :: Position -> Position
move (Position x y North) = Position x (y+1) North
move (Position x y East)  = Position (x + 1) y East
move (Position x y South) = Position x (y-1) South
move (Position x y West)  = Position (x-1) y West

turnAndMove :: Outdoors -> Turn -> Outdoors
turnAndMove (Outdoors panel (Position x y dir)) turnDir =
  let newPos = Position x y (turn dir turnDir)
   in Outdoors panel $ move newPos

paintPanel :: Panel -> Position -> Color -> Panel
paintPanel (Panel panel) (Position x y dir) color = Panel $ insert (x,y) color panel

paint :: Outdoors -> Color -> Outdoors
paint (Outdoors panel position) color = Outdoors (paintPanel panel position color) position

colorAtRobot :: Outdoors -> Color
colorAtRobot (Outdoors (Panel panel) (Position x y dir)) = findWithDefault Black (x,y) panel

paintedSquares :: Outdoors -> Int
paintedSquares (Outdoors (Panel panel) pos) = size panel
