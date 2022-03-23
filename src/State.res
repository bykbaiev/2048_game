open Tile

type stateInternals = {
  best:  option<int>,
  score: int,
  tiles: list<GameTile.tile>
}

type state =
| Playing(stateInternals)
| Win(stateInternals)
| Loss(stateInternals)
| PlayingAfterWin(stateInternals)

let gameStateKey = "gameState"

let bestScoreKey = "bestScore"

// SIDE EFFECT
let initialize = () => {
  let fst = GameTile.createNewTile(list{})
  let snd = GameTile.createNewTile(list{ fst })
  Playing({
    best:  None,
    score: 0,
    tiles: list{ fst, snd }
  })
}

let getInternals = (state: state): stateInternals => {
  switch state {
  | Playing(internals)
  | Win(internals)
  | Loss(internals)
  | PlayingAfterWin(internals) => internals
  }
}

let getBestScore = (state: state): int => {
  let internals = getInternals(state)
  let best = internals.best
  let score = internals.score

  Belt.Option.mapWithDefault(best, score, Js.Math.max_int(score))
}

let isWin = (state: state): bool => {
  switch state {
  | Win(_) => true
  | _      => false
  }
}

let isLoss = (state: state): bool => {
  switch state {
  | Loss(_) => true
  | _       => false
  }
}

let isPlayingAfterWin = (state: state): bool => {
  switch state {
  | PlayingAfterWin(_) => true
  | _                  => false
  }
}

let setTiles = (internals: stateInternals, tiles: list<GameTile.tile>): stateInternals => {
  { ...internals, tiles: tiles }
}

let setScore = (internals: stateInternals, score: int): stateInternals => {
  { ...internals, score: score }
}

let setBestScore = (internals: stateInternals, best: option<int>): stateInternals => {
  { ...internals, best: best }
}

let encodeBestScore = Js.Json.number

let encodeStatus = (state: state) => {
  switch state {
  | Playing(_)         => "playing"
  | Win(_)             => "win"
  | Loss(_)            => "loss"
  | PlayingAfterWin(_) => "playingAfterWin"
  }
}

let encodeGameState = (state: state) => {
  let status = encodeStatus(state)

  let { score, tiles } = getInternals(state)

  let gameState = Js.Dict.empty()

  Js.Dict.set(gameState, "status", Js.Json.string(status))

  Js.Dict.set(gameState, "score", Js.Json.number(Belt.Int.toFloat(score)))

  Js.Dict.set(gameState, "tiles", Js.Json.array(Belt.List.toArray(Belt.List.map(tiles, Tile.GameTile.encode))))

  Js.Json.object_(gameState)
}

let decodeBestScore = (best: option<string>): option<int> => {
  switch best {
  | Some(bestScore) => {
      let scoreJson = try Js.Json.parseExn(bestScore) catch {
      | _ => Js.Json.string("invalid")
      }

      switch Js.Json.classify(scoreJson) {
      | Js.Json.JSONNumber(value) => value -> Js.Math.ceil_int -> Some
      | _                         => None
      }
    }
  | None            => None
  }
}

let decodeGameState = (state: option<string>): option<state> => {
  switch state {
  | Some(value) => {
      let json = try Js.Json.parseExn(value) catch {
      | _ => Js.Json.number(0.)
      }

      switch Js.Json.classify(json) {
      | Js.Json.JSONObject(value) => {
          let status = value -> Js.Dict.get("status") -> Belt.Option.flatMap(Js.Json.decodeString)
          let score =  value -> Js.Dict.get("score")  -> Belt.Option.flatMap(Js.Json.decodeNumber) -> Belt.Option.map(Js.Math.ceil_int)
          let tiles =  value -> Js.Dict.get("tiles")
            -> Belt.Option.flatMap(Js.Json.decodeArray)
            -> Belt.Option.map(ts => ts
                                      -> Belt.List.fromArray
                                      -> Belt.List.map(Tile.GameTile.decode)
                                      -> Belt.List.keep(Belt.Option.isSome))

          let internals = switch (score, tiles) {
          | (Some(score), Some(tiles)) => Some({
                                            best: None,
                                            score: score,
                                            tiles: Belt.List.map(
                                              tiles,
                                              tile => Belt.Option.getWithDefault(
                                                tile,
                                                Tile.GameTile.createNewTile(list{})
                                              )
                                            )
                                          })
          | _                          => None
          }
          
          switch (status, internals) {
          | (Some("playing"), Some(internals))         => internals -> Playing         -> Some
          | (Some("win"), Some(internals))             => internals -> Win             -> Some
          | (Some("loss"), Some(internals))            => internals -> Loss            -> Some
          | (Some("playingAfterWin"), Some(internals)) => internals -> PlayingAfterWin -> Some
          | _                                          => None
          }
        }
      | _                         => None
      }
    }
  | None        => None
  }
}

let localStorageEffect = ({ setSelf, onSet }: Recoil.atomEffect<'a>) => {
  let savedGameState = Dom.Storage.getItem(gameStateKey, Dom.Storage.localStorage)
  let savedBestScore = Dom.Storage.getItem(bestScoreKey, Dom.Storage.localStorage)

  onSet((~newValue, ~oldValue as _, ~isReset) => {
    isReset
      ? {
        Dom.Storage.removeItem(bestScoreKey, Dom.Storage.localStorage)
        Dom.Storage.removeItem(gameStateKey, Dom.Storage.localStorage)
      }
      : {
        Dom.Storage.setItem(
          bestScoreKey,
          newValue -> getBestScore -> Belt.Int.toFloat -> encodeBestScore -> Js.Json.stringify,
          Dom.Storage.localStorage
        )
        Dom.Storage.setItem(
          gameStateKey,
          newValue -> encodeGameState -> Js.Json.stringify,
          Dom.Storage.localStorage
        )
      }
  })

  let bestScore = decodeBestScore(savedBestScore)

  let gameState = decodeGameState(savedGameState)

  setSelf(state => {
    let actualState = Belt.Option.getWithDefault(gameState, state)
    let internals = getInternals(actualState)
    let updated = setBestScore(internals, bestScore)

    switch actualState {
    | Playing(_)         => Playing(updated)
    | Win(_)             => Win(updated)
    | Loss(_)            => Loss(updated)
    | PlayingAfterWin(_) => PlayingAfterWin(updated)  
    }
  })

  None
};

let gameState: Recoil.readWrite<state> = Recoil.atomWithEffects({
  key: "gameState",
  default: {
    initialize()
  },
  effects_UNSTABLE: [localStorageEffect]
})

let internalsState: Recoil.readOnly<stateInternals> = Recoil.selector({
  key: "internalsState",
  get: ({ get }) => {
    gameState -> get -> getInternals
  }
})

let tilesState: Recoil.readOnly<list<GameTile.tile>> = Recoil.selector({
  key: "tilesState",
  get: ({ get }) => {
    internalsState -> get -> (internals => internals.tiles)
  }
})

let scoreState: Recoil.readOnly<int> = Recoil.selector({
  key: "scoreState",
  get: ({ get }) => {
    internalsState -> get -> (internals => internals.score)
  }
})

let bestScoreState: Recoil.readOnly<int> = Recoil.selector({
  key: "bestScoreState",
  get: ({ get }) => {
    gameState -> get -> getBestScore
  }
})

let winState: Recoil.readOnly<bool> = Recoil.selector({
  key: "winState",
  get: ({ get }) => {
    let state = get(gameState)

    isWin(state)
  }
})

let lossState: Recoil.readOnly<bool> = Recoil.selector({
  key: "lossState",
  get: ({ get }) => {
    let state = get(gameState)

    isLoss(state)
  }
})

let endOfGameState: Recoil.readOnly<bool> = Recoil.selector({
  key: "endOfGameState",
  get: ({ get }) => {
    get(winState) || get(lossState)
  }
})

let messageState: Recoil.readOnly<option<string>> = Recoil.selector({
  key: "messageState",
  get: ({ get }) => {
    let state = get(gameState)

    switch state {
    | Win(_)  => Some("You win!")
    | Loss(_) => Some("Game over!")
    | _       => None
    }
  }
})
