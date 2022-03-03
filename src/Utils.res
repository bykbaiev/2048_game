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

let gridSize = 4

let winningValue = 2048

let getCls = (styles: Js.Dict.t<'a>, name: string): string =>
  styles
  -> Js.Dict.get("default")
  -> Belt.Option.flatMap(defaults => Js.Dict.get(defaults, name))
  -> Belt.Option.getWithDefault("")

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
