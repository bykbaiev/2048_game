open Tile

let gridSize = 4

let getCls = (styles: Js.Dict.t<'a>, name: string): string =>
  styles
  -> Js.Dict.get("default")
  -> Belt.Option.flatMap(defaults => Js.Dict.get(defaults, name))
  -> Belt.Option.getWithDefault("")

let keyCodeToDirection = (code: int): option<Constants.direction> => {
  switch code {
  | 37 => Some(Constants.Left)
  | 38 => Some(Constants.Up)
  | 39 => Some(Constants.Right)
  | 40 => Some(Constants.Down)
  | _  => None
  }
}

let transpose = (tiles: list<GameTile.tile>): list<GameTile.tile> => {
  Belt.List.map(
    tiles,
    tile => tile
            -> GameTile.Setters.x(GameTile.Getters.y(tile))
            -> GameTile.Setters.y(GameTile.Getters.x(tile))
  )
}

let reverseRow = (tiles: list<GameTile.tile>, size: int): list<GameTile.tile> => {
  Belt.List.map(tiles, tile => GameTile.Setters.x(tile, size - 1 - GameTile.Getters.x(tile)))
}

let rotateClockwise = (tiles: list<GameTile.tile>, size: int): list<GameTile.tile> => {
  tiles -> transpose -> reverseRow(size)
}

let rotateAntiClockwise = (tiles: list<GameTile.tile>, size: int): list<GameTile.tile> => {
  tiles -> reverseRow(size) -> transpose
}

let isMoveToRightPossible = (tiles: list<GameTile.tile>, size: int): bool => {
  Belt.List.some(tiles, tile => {
    let neighbour = Belt.List.getBy(
      tiles,
      t => GameTile.Getters.y(t) === GameTile.Getters.y(tile) && GameTile.Getters.x(t) === GameTile.Getters.x(tile) + 1
    )
    GameTile.Getters.x(tile) < size - 1 && Belt.Option.mapWithDefault(neighbour, true, t => GameTile.Getters.val(t) === GameTile.Getters.val(tile))
  })
}

let rotateToMoveToRight = (size: int, dir: Constants.direction, tiles: list<GameTile.tile>) => {
  switch dir {
  | Up    => rotateClockwise(tiles, size)
  | Right => tiles
  | Down  => rotateAntiClockwise(tiles, size)
  | Left  => tiles -> rotateClockwise(size) -> rotateClockwise(size)
  }
}

let rotateBack = (size: int, dir: Constants.direction, tiles: list<GameTile.tile>) => {
  switch dir {
  | Up    => rotateAntiClockwise(tiles, size)
  | Right => tiles
  | Down  => rotateClockwise(tiles, size)
  | Left  => tiles -> rotateClockwise(size) -> rotateClockwise(size)
  }
}

let isMovePossible = (tiles: list<GameTile.tile>, size: int, dir: Constants.direction) => {
  isMoveToRightPossible(rotateToMoveToRight(size, dir, tiles), size)
}

let isWin = (tiles: list<GameTile.tile>): bool => {
  Belt.List.some(tiles, GameTile.isWinningValue)
}

let isLoss = (size: int, tiles: list<GameTile.tile>): bool => {
  let isPossibleToMoveSomewhere = isMovePossible(tiles, size)

  size * size === Belt.List.size(tiles)  &&
  !Belt.List.some(tiles, GameTile.isWinningValue) &&
  !isPossibleToMoveSomewhere(Up)         &&
  !isPossibleToMoveSomewhere(Right)      &&
  !isPossibleToMoveSomewhere(Down)       &&
  !isPossibleToMoveSomewhere(Left)
}

let sortTilesByColumn = (tiles: list<GameTile.tile>): list<GameTile.tile> => {
  Belt.List.sort(
    tiles,
    (a, b) => GameTile.Getters.y(a) === GameTile.Getters.y(b)
                ? GameTile.Getters.x(a) - GameTile.Getters.x(b)
                : GameTile.Getters.y(a) - GameTile.Getters.y(b)
  )
}

let movementReducer = (ts, tile) => {
  let addTile = x => Belt.List.add(
    ts,
    tile
    -> GameTile.Setters.x(x)
    -> GameTile.Converters.toAverage
  )

  Belt.Option.mapWithDefault(
    Belt.List.head(ts),
    addTile(0),
    t => {
      if (GameTile.Getters.y(t) === GameTile.Getters.y(tile)) {
        if (GameTile.Getters.val(t) === GameTile.Getters.val(tile) && !GameTile.Getters.merged(t)) {
          Belt.List.add(
            Belt.Option.getWithDefault(Belt.List.drop(ts, 1), list{}),
            t
            -> GameTile.Setters.id(GameTile.Getters.x(tile) > GameTile.Getters.x(t) ? GameTile.Getters.id(tile) : GameTile.Getters.id(t))
            -> GameTile.Setters.val(GameTile.Getters.val(t) * 2)
            -> GameTile.Converters.toMerged
          )
        } else {
          addTile(GameTile.Getters.x(t) + 1)  
        }
      } else {
        addTile(0)
      }
    }
  )
}

let moveRight = (size: int, tiles: list<GameTile.tile>): list<GameTile.tile> => {
  tiles
  -> reverseRow(size)
  -> sortTilesByColumn
  -> Belt.List.reduce(list{}, movementReducer)
  -> reverseRow(size)
}

let move = (dir: Constants.direction, state: State.state): State.state => {
  let internals = State.getInternals(state)
  let tiles = internals.tiles

  let size = gridSize
  let rotated = rotateToMoveToRight(size, dir, tiles)

  if isMoveToRightPossible(rotated, size) {
    let moved = moveRight(size, rotated)
    let movedInternals = State.setTiles(internals, rotateBack(size, dir, moved))

    if isWin(moved) {
      State.Win(movedInternals)
    } else if isLoss(size, moved) {
      State.Loss(movedInternals)
    } else {
      let updated = Belt.List.add(moved, GameTile.createNewTile(moved))
      let updatedInternals = State.setTiles(internals, rotateBack(size, dir, updated))

      if isWin(updated) {
        State.Win(updatedInternals)
      } else if isLoss(size, updated) {
        State.Loss(updatedInternals)
      } else {
        State.Playing(updatedInternals)
      }
    }
  } else {
    state
  }
}
