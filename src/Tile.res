module GameTile = {
  type position = {
    x: int,
    y: int
  }

  type tile = {
    id:     string,
    val:    int,
    pos:    position,
    new:    bool,
    merged: bool,
  }

  let gridSize = 4

  let createTile = (~id, ~val, ~x, ~y): tile => {
    id: id,
    val: val,
    pos: { x: x, y: y },
    new: true,
    merged: false,
  }

  module Getters = {
    let id = tile => tile.id

    let val = tile => tile.val

    let x = tile => tile.pos.x

    let y = tile => tile.pos.y

    let new = tile => tile.new

    let merged = tile => tile.merged
  }

  module Setters = {
    let id = (tile, id) => { ...tile, id: id }

    let x = (tile, x) => { ...tile, pos: { ...tile.pos, x: x } }

    let y = (tile, y) => { ...tile, pos: { ...tile.pos, y: y } }

    let val = (tile, val) => { ...tile, val: val }

    let new = (tile, new) => { ...tile, new: new }

    let merged = (tile, merged) => { ...tile, merged: merged }
  }

  let positionFilterPred = (tiles: list<tile>, position: (int, int)) => switch position {
  | (-1, -1) => false
  | (x, y)   => !Belt.List.some(tiles, ({ pos }) => pos.x === x && pos.y === y)
  }

  let getPair = (max: int, idx: int): option<(int, int)> => {
    if idx >= max * max {
      None
    } else {
      let y = mod(idx, max)
      Some(idx / max, y)
    }
  }

  // SIDE EFFECT - CREATION OF NEW RANDOM TILE
  let createNewTile = (tiles: list<tile>): tile => {
    let allPositions = Belt.List.makeBy(gridSize * gridSize, getPair(gridSize))
    let availablePositions = allPositions
      -> Belt.List.map(pos => Belt.Option.getWithDefault(pos, (-1, -1)))
      -> Belt.List.keep(positionFilterPred(tiles))
    let idx = Js.Math.random_int(0, Belt.List.size(availablePositions))
    let (x, y) = availablePositions -> Belt.List.get(idx) -> Belt.Option.getWithDefault((0, 0))
    let randTileIndicator = Js.Math.random_int(0, 4)
    let id = `tile-${Belt.Float.toString(Js.Math.random())}`
    createTile(
      ~id = id,
      ~val = randTileIndicator === 3 ? 4 : 2,
      ~x = x,
      ~y = y
    )
  }

  let isWinningValue = (tile: tile) => tile.val === Constants.winningValue
}
