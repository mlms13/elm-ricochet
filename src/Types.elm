module Types exposing (Grid, createGrid, setCell)

import List.Extra exposing (setAt)

type Grid = Grid Row Col (List CellContent) (List CellEdge) (List CellEdge)

type Row = Row Int
type Col = Col Int

createGrid : Row -> Col -> Grid
createGrid (Row rows) (Col cols) =
  let
    size : Int
    size = rows * cols

    horizontalEdges : List CellEdge
    horizontalEdges = List.repeat (cols * (rows + 1)) NoBlock

    verticalEdges : List CellEdge
    verticalEdges = List.repeat (rows * (cols + 1)) NoBlock
  in
    Grid (Row rows) (Col cols) (List.repeat size Empty) horizontalEdges verticalEdges


cellIndex : Row -> Col -> Grid -> Int
cellIndex (Row row) (Col col) (Grid _ (Col width) _ _ _) =
  (row * width) + col

setCell : Row -> Col -> CellContent -> Grid -> Maybe Grid
setCell row col cell grid =
  case grid of
    Grid rows cols cells hEdges vEdges ->
      let
          idx : Int
          idx = cellIndex row col grid
      in
        Maybe.map (\cells -> (Grid rows cols cells hEdges vEdges)) (setAt idx cell cells)

edgeIndex : Int -> Int -> Int -> Bool -> Int
edgeIndex dim1 dim2 maxDim2 isBefore =
  let
    offset : Int
    offset = case isBefore of
      True -> 0
      False -> 1
  in
    (dim1 * (maxDim2 + offset)) + dim2

setEdge : Row -> Col -> Direction -> CellEdge -> Grid -> Maybe Grid
setEdge row col dir edge grid =
  let
    (Grid rows cols cells hEdges vEdges) = grid
    nRows : Row -> Int
    nRows (Row rows) = rows
    nCols : Col -> Int
    nCols (Col cols) = cols
    setHorizontal : Row -> Col -> Bool -> CellEdge -> Maybe (List CellEdge)
    setHorizontal (Row row) (Col col) isTop edge = -- TODO isTop
      setAt (edgeIndex row col (nCols cols) isTop) edge hEdges
    setVertical : Row -> Col -> Bool -> CellEdge -> Maybe (List CellEdge)
    setVertical (Row row) (Col col) isLeft edge = -- TODO isLeft
      setAt (edgeIndex col row (nRows rows) isLeft) edge vEdges
  in
    case dir of
      Top    -> Maybe.map (\edges -> Grid rows cols cells edges vEdges) (setHorizontal row col True edge)
      Bottom -> Maybe.map (\edges -> Grid rows cols cells edges vEdges) (setHorizontal row col False edge)
      Left   -> Maybe.map (\edges -> Grid rows cols cells hEdges edges) (setVertical row col True edge)
      Right  -> Maybe.map (\edges -> Grid rows cols cells hEdges edges) (setVertical row col False edge)

type Direction
  = Top
  | Left
  | Bottom
  | Right

type CellEdge
  = NoBlock
  | BlockNW -- can't move from the cell up or left
  | BlockSE -- same, but down or right
  | Block

type CellContent
  = Empty
  | TargetCell Target
  | RobotCell Robot

type Target
  = RobotTarget Robot
  | SpecialTarget Special -- TODO?

type Special
  = Any -- any color robot can move to this target
  -- can add more, duh.

type Robot
  = Yellow
  | Blue
  | Red
  | Green
  -- | Magenta
  -- | Orange
  -- | Maroon
  -- | Aqua
  -- | Lime
  -- | Pink

