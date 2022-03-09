type direction =
  | Up
  | Right
  | Down
  | Left

type position = {
  x: int,
  y: int
}

type tile = {
  val: int,
  pos: position
}

type board = list<list<option<int>>>

let gridSize = 4

let winningValue = 2048

let getCls = (styles: Js.Dict.t<'a>, name: string): string =>
  styles
  -> Js.Dict.get("default")
  -> Belt.Option.flatMap(defaults => Js.Dict.get(defaults, name))
  -> Belt.Option.getWithDefault("")

let tilesToBoard = (size: int, tiles: list<tile>): board => {
  Belt.List.makeBy(size, y => {
    Belt.List.makeBy(size, x => {
      Belt.Option.map(
        Belt.List.getBy(
          tiles,
          tile => tile.pos.x === x && tile.pos.y === y
        ),
        tile => tile.val
      )
    })
  })
}

let createTile = (~val, ~x, ~y): tile => { val: val, pos: { x: x, y: y } }

let getPair = (max: int, idx: int): option<(int, int)> => {
  if idx >= max * max {
    None
  } else {
    let y = mod(idx, max)
    Some(idx / max, y)
  }
}

let positionFilterPred = (tiles: list<tile>, position: (int, int)) => switch position {
| (-1, -1) => false
| (x, y)   => !Belt.List.some(tiles, ({ pos }) => pos.x === x && pos.y === y)
}

// SIDE EFFECT - CREATION OF NEW RANDOM TILE
let createNewTile = (tiles: list<tile>): tile => {
  let allPositions = Belt.List.makeBy(gridSize * gridSize, getPair(gridSize))
  let availablePositions = allPositions
    -> Belt.List.map(pos => Belt.Option.getWithDefault(pos, (-1, -1)))
    -> Belt.List.keep(positionFilterPred(tiles))
  let idx = Js.Math.random_int(0, Belt.List.size(availablePositions))
  let (x, y) = availablePositions -> Belt.List.get(idx) -> Belt.Option.getWithDefault((0, 0))
  createTile(~val = 2, ~x = x, ~y = y) // TODO: sometimes it's 4
}

let keyCodeToDirection = (code: int): option<direction> => {
  switch code {
  | 37 => Some(Left)
  | 38 => Some(Up)
  | 39 => Some(Right)
  | 40 => Some(Down)
  | _  => None
  }
}

let isWinningValue = tile => tile.val === winningValue

let transpose = (tiles: list<tile>): list<tile> => {
  Belt.List.map(tiles, tile => { val: tile.val, pos: { x: tile.pos.y, y: tile.pos.x } })
}

let reverseRow = (tiles: list<tile>, size: int): list<tile> => {
  Belt.List.map(tiles, tile => { val: tile.val, pos: { x: size - 1 - tile.pos.x, y: tile.pos.y }})
}

let rotateClockwise = (tiles: list<tile>, size: int): list<tile> => {
  tiles -> transpose -> reverseRow(size)
}

let rotateAntiClockwise = (tiles: list<tile>, size: int): list<tile> => {
  tiles -> reverseRow(size) -> transpose
}

let isMoveToRightPossible = (tiles: list<tile>, size: int): bool => {
  Belt.List.some(tiles, tile => {
    let neighbour = Belt.List.getBy(tiles, t => t.pos.y === tile.pos.y && t.pos.x === tile.pos.x + 1)
    tile.pos.x < size - 1 && Belt.Option.mapWithDefault(neighbour, true, t => t.val === tile.val)
  })
}

let rotateToMoveToRight = (size: int, dir: direction, tiles: list<tile>) => {
  switch dir {
  | Up    => rotateClockwise(tiles, size)
  | Right => tiles
  | Down  => rotateAntiClockwise(tiles, size)
  | Left  => tiles -> rotateClockwise(size) -> rotateClockwise(size)
  }
}

let rotateBack = (size: int, dir: direction, tiles: list<tile>) => {
  switch dir {
  | Up    => rotateAntiClockwise(tiles, size)
  | Right => tiles
  | Down  => rotateClockwise(tiles, size)
  | Left  => tiles -> rotateClockwise(size) -> rotateClockwise(size)
  }
}

let isMovePossible = (tiles: list<tile>, size: int, dir: direction) => {
  isMoveToRightPossible(rotateToMoveToRight(size, dir, tiles), size)
}

let isWin = (tiles: list<tile>): bool => {
  Belt.List.some(tiles, isWinningValue)
}

let isLoss = (size: int, tiles: list<tile>): bool => {
  let isPossibleToMoveSomewhere = isMovePossible(tiles, size)

  size * size === Belt.List.size(tiles)  &&
  !Belt.List.some(tiles, isWinningValue) &&
  !isPossibleToMoveSomewhere(Up)         &&
  !isPossibleToMoveSomewhere(Right)      &&
  !isPossibleToMoveSomewhere(Down)       &&
  !isPossibleToMoveSomewhere(Left)
}

let sortTilesByColumn = (tiles: list<tile>): list<tile> => {
  Belt.List.sort(tiles, (a, b) => a.pos.y === b.pos.y ? a.pos.x - b.pos.x : a.pos.y - b.pos.y)
}

let setColumn = (tile: tile, x: int) => { val: tile.val, pos: { x: x, y: tile.pos.y } } 

let movementReducer = (ts, tile) => {
  let addTile = x => Belt.List.add(ts, setColumn(tile, x))

  Belt.Option.mapWithDefault(
    Belt.List.head(ts),
    addTile(0),
    t => {
      if (t.pos.y === tile.pos.y) {
        addTile(t.pos.x + 1)
      } else {
        addTile(0)
      }
    }
  )
}

let moveRight = (size: int, tiles: list<tile>): list<tile> => {
  tiles
  -> reverseRow(size)
  -> sortTilesByColumn
  -> Belt.List.reduce(list{}, movementReducer)
  -> reverseRow(size)
}

// * transform the tiles that move is always to right !
// * check if move is possible
// * move tiles (if possible)
// * check if it's a win or a loss
// * add new tile
// * check if move is possible
// * check if it's a loss
// * transform back
let move = (dir: direction, tiles: list<tile>) => {
  let size = gridSize
  let rotated = rotateToMoveToRight(size, dir, tiles)

  if isMoveToRightPossible(rotated, size) {
    let moved = moveRight(size, rotated)

    if isWin(moved) || isLoss(size, moved) {
      rotateBack(size, dir, moved)
    } else {
      let updated = Belt.List.add(moved, createNewTile(moved))

      if isWin(updated) || isLoss(size, updated) {
        rotateBack(size, dir, updated) // TODO add handling for Win | Loss | Play
      } else {
        rotateBack(size, dir, updated)
      }
    }
  } else {
    tiles
  }
}
