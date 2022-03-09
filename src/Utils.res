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
  createTile(~val = 2, ~x = x, ~y = y)
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

let isWin = (tiles: list<tile>): bool => {
  Belt.List.some(tiles, isWinningValue)
}

let isLoss = (size: int, tiles: list<tile>): bool => {
  size === Belt.List.size(tiles) && !Belt.List.some(tiles, isWinningValue) // TODO: add check if move is possible
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
  tiles
}
