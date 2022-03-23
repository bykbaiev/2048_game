module GameTile = {
  type position = {
    x: int,
    y: int
  }

  type tileInternals = {
    id: string,
    val: int,
    pos: position,
  }

  type tile =
  | NewTile(tileInternals)
  | MergedTile(tileInternals)
  | AverageTile(tileInternals)

  let gridSize = 4

  let createTile = (~id, ~val, ~x, ~y): tile =>
    NewTile({
      id: id,
      val: val,
      pos: { x: x, y: y },
    })

  module Getters = {
    let status = tile => {
      switch tile {
      | AverageTile(_) => "average"
      | MergedTile(_)  => "merged"
      | NewTile(_)     => "new"
      }
    }

    let internals = tile => switch tile {
    | NewTile(data)     => data
    | MergedTile(data)  => data
    | AverageTile(data) => data
    }

    let id = tile => internals(tile).id

    let val = tile => internals(tile).val

    let x = tile => internals(tile).pos.x

    let y = tile => internals(tile).pos.y

    let new = tile => switch tile {
    | NewTile(_) => true
    | _          => false
    }

    let merged = tile => switch tile {
    | MergedTile(_) => true
    | _             => false
    }
  }

  let updateInternals = (fn: tileInternals => tileInternals, tile: tile) => switch tile {
  | NewTile(internals)     => NewTile(fn(internals))
  | MergedTile(internals)  => MergedTile(fn(internals))
  | AverageTile(internals) => AverageTile(fn(internals))
  }

  module Setters = {
    let id = (tile, id) => updateInternals(internals => { ...internals, id: id }, tile)

    let x = (tile, x) => updateInternals(internals => { ...internals, pos: { ...internals.pos, x: x } }, tile)

    let y = (tile, y) => updateInternals(internals => { ...internals, pos: { ...internals.pos, y: y } }, tile)

    let val = (tile, val) => updateInternals(internals => { ...internals, val: val }, tile)
  }

  module Converters = {
    let toNew = tile => switch tile {
    | AverageTile(internals) => NewTile(internals)
    | MergedTile(internals)  => NewTile(internals)
    | other                  => other
    }

    let toMerged = tile => switch tile {
    | AverageTile(internals) => MergedTile(internals)
    | NewTile(internals)     => MergedTile(internals)
    | other                  => other
    }

    let toAverage = tile => switch tile {
    | MergedTile(internals) => AverageTile(internals)
    | NewTile(internals)    => AverageTile(internals)
    | other                 => other
    }
  }

  let positionFilterPred = (tiles: list<tile>, position: (int, int)) => switch position {
  | (-1, -1) => false
  | (x, y)   => !Belt.List.some(tiles, tile => {
      Getters.x(tile) === x && Getters.y(tile) === y
    })
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

  let isWinningValue = (tile: tile) => Getters.val(tile) === Constants.winningValue

  let encode = tile => {
    let dict = Js.Dict.empty()

    Js.Dict.set(dict, "status", tile -> Getters.status -> Js.Json.string)
    Js.Dict.set(dict, "id", tile -> Getters.id -> Js.Json.string)
    Js.Dict.set(dict, "val", tile -> Getters.val -> Belt.Int.toFloat -> Js.Json.number)
    Js.Dict.set(dict, "x", tile -> Getters.x -> Belt.Int.toFloat -> Js.Json.number)
    Js.Dict.set(dict, "y", tile -> Getters.y -> Belt.Int.toFloat -> Js.Json.number)

    Js.Json.object_(dict)
  }

  let decode = tile => {
    switch Js.Json.classify(tile) {
    | Js.Json.JSONObject(value) => {
        let status = value -> Js.Dict.get("status") -> Belt.Option.flatMap(Js.Json.decodeString)
        let id =     value -> Js.Dict.get("id")     -> Belt.Option.flatMap(Js.Json.decodeString)
        let val =    value -> Js.Dict.get("val")    -> Belt.Option.flatMap(Js.Json.decodeNumber) -> Belt.Option.map(Js.Math.ceil_int)
        let x =      value -> Js.Dict.get("x")      -> Belt.Option.flatMap(Js.Json.decodeNumber) -> Belt.Option.map(Js.Math.ceil_int)
        let y =      value -> Js.Dict.get("y")      -> Belt.Option.flatMap(Js.Json.decodeNumber) -> Belt.Option.map(Js.Math.ceil_int)

        let pos = switch (x, y) {
        | (Some(x), Some(y)) => Some({ x: x, y: y })
        | _                  => None
        }

        let internals = switch (id, val, pos) {
        | (Some(id), Some(val), Some(pos)) => Some({ id: id, val: val, pos: pos })
        | _                                => None
        }
        
        switch (status, internals) {
        | (Some("new"), Some(internals))     => internals -> NewTile     -> Some
        | (Some("average"), Some(internals)) => internals -> AverageTile -> Some
        | (Some("merged"), Some(internals))  => internals -> MergedTile  -> Some
        | _                                  => None
        }
      }
    | _                         => None
    }
  }
}
